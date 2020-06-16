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
;; them.

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
	  (gemini-write-response address 'elpher-render-gemini token data))
      (error
       (elpher-network-error address the-error)))))

(defun gemini-write-response (address renderer token data &optional force-ipv4)
  "Write DATA using TOKEN to gemini ADDRESS, then render using RENDERER.
TOKEN is some kind of password or other string, see `elpher-gemini-tokens'.
DATA must be raw bytes. Use (encode-coding-string (buffer-string) 'utf-8 t).
If FORCE-IPV4 is non-nil, explicitly look up and use IPv4 address corresponding
to ADDRESS."
  (unless elpher-gemini-TLS-cert-checks
    (setq-local network-security-level 'low))
  (if (not (gnutls-available-p))
      (error "Cannot establish gemini connection: GnuTLS not available")
    (unless (< (elpher-address-port address) 65536)
      (error "Cannot establish gemini connection: port number > 65536"))
    (defvar gnutls-verify-error)
    (condition-case nil
        (let* ((kill-buffer-query-functions nil)
               (gnutls-verify-error nil) ; We use the NSM for verification
               (port (elpher-address-port address))
               (host (elpher-address-host address))
               (response-string-parts nil)
               (bytes-received 0)
               (hkbytes-received 0)
               (proc (open-network-stream "elpher-process"
                                          nil
                                          (if (or elpher-ipv4-always force-ipv4)
                                              (dns-query host)
                                            host)
                                          (if (> port 0) port 1965)
                                          :type 'tls
                                          :nowait t))
               (timer (run-at-time elpher-connection-timeout nil
                                   (lambda ()
                                     (elpher-process-cleanup)
                                     (unless (or elpher-ipv4-always force-ipv4)
                                        ; Try again with IPv4
                                       (message "Connection timed out.  Retrying with IPv4.")
                                       (elpher-get-gemini-response address renderer t))))))
          (setq elpher-network-timer timer)
          (set-process-coding-system proc 'binary)
          (set-process-filter proc
                              (lambda (_proc string)
                                (when timer
                                  (cancel-timer timer)
                                  (setq timer nil))
                                (setq bytes-received (+ bytes-received (length string)))
                                (let ((new-hkbytes-received (/ bytes-received 102400)))
                                  (when (> new-hkbytes-received hkbytes-received)
                                    (setq hkbytes-received new-hkbytes-received)
                                    (with-current-buffer "*elpher*"
                                      (let ((inhibit-read-only t))
                                        (goto-char (point-min))
                                        (beginning-of-line 2)
                                        (delete-region (point) (point-max))
                                        (insert "("
                                                (number-to-string (/ hkbytes-received 10.0))
                                                " MB read)")))))
                                (setq response-string-parts
                                      (cons string response-string-parts))))
          (set-process-sentinel proc
                                (lambda (proc event)
                                  (condition-case the-error
                                      (cond
                                       ((string-prefix-p "open" event)    ; request URL
                                        (let ((inhibit-eol-conversion t))
                                          (process-send-string
                                           proc
                                           (concat (elpher-address-to-url address)
						   ";mime=text/plain"
						   ";size=" (number-to-string (length data))
						   (if token (concat ";token=" token) "")
						   "\n"
						   data))))
				       ((string-prefix-p "deleted" event)) ; do nothing
                                       ((and (not response-string-parts)
                                             (not (or elpher-ipv4-always force-ipv4)))
                                        ; Try again with IPv4
                                        (message "Connection failed. Retrying with IPv4.")
                                        (cancel-timer timer)
                                        (elpher-get-gemini-response address renderer t))
                                       (t
                                        (funcall #'elpher-process-gemini-response
                                                 (apply #'concat (reverse response-string-parts))
                                                 renderer)
                                        (elpher-restore-pos)))
                                    (error
                                     (elpher-network-error address the-error))))))
      (error
       (error "Error initiating connection to server")))))

(provide 'gemini-write)

;;; gemini-write.el ends here
