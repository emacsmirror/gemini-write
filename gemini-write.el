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
;; 'C-c C-c' to save, use 'C-c C-k' to cancel. Customize
;; 'elpher-gemini-tokens' to set passwords, tokens, or whatever you
;; need in order to edit sites.

;;; Code:

(require 'elpher)
(require 'gemini-mode)

;;; gemini-write support

(add-hook 'elpher-mode-hook
	  (lambda ()
	    (local-set-key (kbd "e") 'elpher-edit)))

(defun elpher-edit ()
  "Edit something, if possible.
Editing can be attempted in two situations:
1. via gopher, when looking at item type 'w'
2. via titan, when looking at a gemini URL"
  (interactive)
  (let ((address (elpher-page-address elpher-current-page)))
    (cond ((equal (elpher-address-protocol address) "gemini")
           (elpher-edit-gemini address))
	  ;; FIXME: add support for gopher item 'w'
	  (t (error "Elpher does not know how to edit this")))))

(defun elpher-edit-gemini (address)
  "Edit ADDRESS.
This usually involves switching from gemini to the titan URL
scheme."
  (read-only-mode 0)
  (gemini-mode)
  (when (not (equal (elpher-address-protocol address) "titan"))
    (setf (url-type address) "titan"))
  (message "Use C-c C-c to save, C-c C-k to cancel"))

(add-to-list 'gemini-mode-hook 'gemini-write-init)

(defun gemini-write-init ()
  "Add editing commands to `gemini-mode'."
  (local-set-key (kbd "C-c C-c") 'gemini-write)
  (local-set-key (kbd "C-c C-k") 'gemini-write-cancel))

(defcustom elpher-gemini-tokens
  '(("alexschroeder.ch" . "hello")
    ("communitywiki.org" . "hello"))
  "An alist of hostnames and authorization tokens
used when writing Gemini pages."
  :type '(alist :key-type (string :tag "Host") :value-type (string :tag "Token"))
  :group 'gemini-mode)

(defun gemini-write-cancel ()
  "Reload current Gemini buffer."
  (interactive)
  (let ((address (elpher-page-address elpher-current-page)))
    (when (not (equal (elpher-address-protocol address) "gemini"))
      (setf (url-type address) "gemini")))
  (elpher-reload))

(defun gemini-write ()
  "Save the current Gemini buffer.
This will be saved to `elpher-current-page'."
  (interactive)
  (let* ((address (elpher-page-address elpher-current-page))
	 (token (cdr (assoc (url-host address) elpher-gemini-tokens)))
	 (data (encode-coding-string (buffer-string) 'utf-8 t)))
    (condition-case the-error
	(progn
          (elpher-with-clean-buffer
           (insert "SAVING GEMINI... (use 'u' to cancel)\n"))
	  (setq elpher-gemini-redirect-chain nil)
	  (titan-write-response address 'elpher-render-gemini token data))
      (error
       (elpher-network-error address the-error)))))

(defun titan-write-response (address renderer token data)
  "Write request to titan server at ADDRESS and render using RENDERER.
The token, MIME type, and data size are added as parameters to
the last address segment."
  (elpher-get-host-response address 1965
                            (concat (elpher-address-to-url address)
				    ";mime=text/plain"
				    ";size=" (number-to-string (length data))
				    (if token (concat ";token=" token) "")
				    "\r\n"
				    data)
                            (lambda (response-string)
                              (elpher-process-gemini-response response-string renderer))
                            'gemini))

(provide 'gemini-write)

;;; gemini-write.el ends here
