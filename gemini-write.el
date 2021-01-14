;;; gemini-write.el --- Elpher for Titan  -*- lexical-binding:t -*-

;; Copyright (C) 2020 Alex Schroeder
;; Copyright (C) 2019 Tim Vaughan

;; Author: Alex Schroeder <alex@gnu.org>
;; Keywords: comm gemini
;; Homepage: https://alexschroeder.ch/cgit/gemini-write
;; Package-Requires: ((emacs "26"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This uses Elpher to browse Gemini sites and Gemini Mode to edit
;; them using the Titan protocol.

;; - https://thelambdalab.xyz/elpher/
;; - https://git.carcosa.net/jmcbray/gemini.el

;; Use 'e' to edit a Gemini page on a site that has Titan enabled. Use
;; 'C-c C-c' to save. Customize 'elpher-gemini-tokens' to set
;; passwords, tokens, or whatever you need in order to edit sites.

;;; Code:

(require 'elpher)
(require 'gemini-mode)
(require 'auth-source)

;;; gemini-write support

(define-key elpher-mode-map (kbd "e") 'elpher-edit)
(define-key elpher-mode-map (kbd "w") 'gemini-write-file)

(eval-after-load "elpher"
  '(advice-add 'elpher-render-gemini-plain-text :after 'gemini-write-mime-type-text))

(defvar gemini-write-text-p nil
  "A buffer local variable to store whether this is plain text.")

(defun gemini-write-mime-type-text (&rest ignore)
  "Remember that this buffer is plain/text."
  (setq-local gemini-write-text-p t))

(defun elpher-edit ()
  "Edit a copy of the current Elpher buffer, if possible.
Note that this only makes sense if you're looking at the raw
gemtext. If you're looking at the rendered text, editing it
will be a mess."
  (interactive)
  (let ((address (elpher-page-address elpher-current-page)))
    (cond ((not (equal (elpher-address-protocol address) "gemini"))
	   (error "Elpher does not know how to edit %s"
		  (elpher-address-protocol address)))
	  ((not gemini-write-text-p)
	   (error "Elpher only knows how to edit text/plain"))
	  (t (elpher-edit-buffer (buffer-string)
				 elpher-current-page (point))))))

(defun elpher-edit-buffer (text page point)
  "Edit TEXT using Gemini mode for PAGE.
PAGE is an Elpher page like `elpher-current-page'."
  (switch-to-buffer
   (get-buffer-create
    (generate-new-buffer-name "*elpher edit*")))
  (insert text)
  (goto-char point)
  (elpher-edit-gemini page))

(defun elpher-edit-gemini (page)
  "Edit the current buffer for PAGE."
  (gemini-mode)
  (setq-local elpher-current-page page)
  (let ((address (elpher-page-address elpher-current-page)))
    (when elpher-use-header
      (setq header-line-format (url-unhex-string (elpher-address-to-url address)))))
  (message "Use C-c C-c to save"))

(add-to-list 'gemini-mode-hook 'gemini-write-init)

(defun gemini-write-init ()
  "Add editing commands to `gemini-mode'."
  (local-set-key (kbd "C-c C-c") 'gemini-write))

(defcustom elpher-gemini-tokens
  '(("alexschroeder.ch" . "hello")
    ("communitywiki.org" . "hello")
    ("transjovian.org" . "hello")
    ("toki.transjovian.org" . "hello")
    ("next.oddmuse.org" . "hello")
    ("emacswiki.org" . "emacs")
    ("127.0.0.1" . "hello")
    ("localhost" . "hello"))
  "An alist of hostnames and authorization tokens
used when writing Gemini pages."
  :type '(alist :key-type (string :tag "Host") :value-type (string :tag "Token"))
  :group 'gemini-mode)

(defcustom gemini-write-use-auth-source t
  "Enable password fetching from `auth-source', as well as from `elpher-gemini-tokens'."
  :type 'boolean
  :group 'gemini-mode)

(defun get-elpher-buffer-showing (page)
  "Return the first Elpher buffer showing PAGE."
  (catch 'buf
    (let ((address (elpher-page-address page)))
      (dolist (buf (buffer-list))
	(with-current-buffer buf
	  (when (and (eq major-mode 'elpher-mode)
		     (or (equal (elpher-page-address elpher-current-page)
				address)
			 (equal (elpher-page-address (car elpher-history))
				address)))
	    (throw 'buf buf)))))))

(defun gemini-write-get-token (host &optional port)
  "Get a token from `elpher-gemini-tokens', or `auth-sources' if `gemini-write-use-auth-source' is enabled."
  (if-let (token (cdr (assoc host elpher-gemini-tokens)))
      token
    (if gemini-write-use-auth-source
	(let ((info (nth 0 (auth-source-search
			    :host host
			    :port (or port 1965)
			    :require '(:secret)))))
	  (if info
	      (let ((secret (plist-get info :secret)))
		(if (functionp secret)
		    (funcall secret)
		  secret))
	    nil))
      nil)))

(defun gemini-write ()
  "Save the current Gemini buffer.
This will be saved to `elpher-current-page'. If there's an Elpher
buffer that already shows this page, that's the buffer we're
going to use. Otherwise, a new buffer is used."
  (interactive)
  ;; using copy-sequence such that the redirect in the original buffer
  ;; doesn't change our address, too
  (let* ((page (copy-sequence elpher-current-page))
	 (address (elpher-page-address page))
	 (buf (or (get-elpher-buffer-showing page)
		  (with-current-buffer
		      (generate-new-buffer (default-value 'elpher-buffer-name))
		    (elpher-mode)
		    (current-buffer))))
	 (token (gemini-write-get-token (url-host address)))
	 (data (encode-coding-string (buffer-string) 'utf-8 t)))
    (switch-to-buffer buf)
    (setq-local elpher-current-page page)
    (condition-case the-error
	(progn
	  (elpher-with-clean-buffer
	   (insert "SAVING GEMINI... (use 'u' to cancel)\n"))
	  (setq elpher-gemini-redirect-chain nil)
	  (titan-write-response address 'elpher-render-gemini token data))
      (error
       (elpher-network-error address the-error)))))

(defun gemini-write-file (file url)
  "Upload a file."
  (interactive
   (list (read-file-name "Upload file: " nil nil t nil 'file-regular-p)
	 (read-string "URL: "
		      (elpher-address-to-url
		       (elpher-page-address elpher-current-page)))))
  ;; using copy-sequence such that the redirect in the original buffer
  ;; doesn't change our address, too
  (let* ((buf (with-current-buffer
		  (generate-new-buffer (default-value 'elpher-buffer-name))
		(elpher-mode)
		(current-buffer)))
	 (address (elpher-address-from-url url))
	 (token (gemini-write-get-token (url-host address)))
	 (mime-type (completing-read "MIME type: " (mailcap-mime-types) nil t
				     (mailcap-extension-to-mime
				      (file-name-extension file t))))
	 (data (with-temp-buffer
		 (insert-file-contents-literally file)
		 (buffer-string))))
    (switch-to-buffer buf)
    (setq-local elpher-current-page
		(elpher-make-page
		 (format "*elpher upload of %s*" file)
		 address))
    (condition-case the-error
	(progn
	  (elpher-with-clean-buffer
	   (insert "SAVING GEMINI... (use 'u' to cancel)\n"))
	  (setq elpher-gemini-redirect-chain nil)
	  (titan-write-response address 'elpher-render-gemini token data mime-type))
      (error
       (elpher-network-error address the-error)))))

(defun titan-write-response (address renderer token data &optional mime-type)
  "Write request to titan server at ADDRESS and render using RENDERER.
The TOKEN, MIME-TYPE, and data size are added as parameters to
the last address segment. The MIME type defaults to text/plain."
  ;; using copy sequence so that the buffer's address doesn't change
  ;; from gemini to titan
  (let ((titan-address (copy-sequence address)))
    (setf (url-type titan-address) "titan")
    (elpher-get-host-response
     titan-address 1965
     (concat (elpher-address-to-url titan-address)
	     ";mime=" (or mime-type "text/plain")
	     ";size=" (number-to-string (length data))
	     (if token (concat ";token=" token) "")
	     "\r\n"
	     data)
     (lambda (response-string)
       (elpher-process-gemini-response response-string renderer))
     'gemini)))

(provide 'gemini-write)

;;; gemini-write.el ends here
