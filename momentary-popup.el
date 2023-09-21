;;; momentary-popup.el --- Configure popup -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/momentary-popup
;; Keywords: lisp
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides functions similar to `momentary-string-display' but inside popup windows.

;; - `momentary-popup' (content &rest setup-args)
;; Momentarily display CONTENT in popup window.
;; Display remains until next event is input.
;; Persist popup if input is a key binding of a command
;;  `momentary-popup-open-inspector'in `momentary-popup-switch-keymap'.
;; SETUP-ARGS can includes keymaps, syntax table, filename and function.
;; See a function `momentary-popup-open-inspector'

;; Usage examples:

;; (require 'momentary-popup)
;; (momentary-popup (sexp-at-point))
;; (momentary-popup (text-properties-at (point)) 'emacs-lisp-mode)
;; (momentary-popup "const b = 34;" 'js-mode 'js-mode-syntax-table))
;; (momentary-popup "const b = 34;" 'js-mode 'js-mode-syntax-table my-keymap))

;; Commands

;; M-x `momentary-popup-file' (file)
;;      Momentarily display content of the FILE in popup window.
;;
;;      Display remains until next event is input.
;;
;;      To persist popup use \<momentary-popup-switch-keymap> `\[momentary-popup-open-inspector]'.

;; M-x `momentary-popup-open-inspector'
;;      Open or restore popup in a buffer `momentary-popup-inspect-buffer-name'.

;; M-x `momentary-popup-maybe-find-file'
;;      If `header-line-format' is a file, open it.
;;      Also kill buffer `momentary-popup-inspect-buffer-name' if exists.

;;; Code:


(defvar momentary-popup-window-last-key nil)
(defvar momentary-popup-content nil)
(defvar momentary-popup-meta nil)
(defvar momentary-popup-inspect-buffer-name "*momentary-popup-insepct*")
(defvar momentary-popup-momentary-buffer-name "*momentary-popup*")

(defun momentary-popup-fontify (content &optional mode-fn &rest args)
  "Fontify CONTENT according to MODE-FN called with ARGS.
If CONTENT is not a string, instead of MODE-FN emacs-lisp-mode will be used."
  (with-temp-buffer
    (delay-mode-hooks
      (apply (or mode-fn 'emacs-lisp-mode) args)
      (goto-char (point-min))
      (insert (if (or (eq major-mode 'emacs-lisp-mode)
                      (not (stringp content)))
                  (pp-to-string content)
                content))
      (font-lock-ensure)
      (buffer-string))))

(defun momentary-popup-minibuffer-select-window ()
  "Select minibuffer window if it is active."
  (when-let ((wind (active-minibuffer-window)))
    (select-window wind)))

;;;###autoload
(defun momentary-popup-maybe-find-file ()
  "If `header-line-format' is a file, open it.
Also kill buffer `momentary-popup-inspect-buffer-name' if exists."
  (interactive)
  (when-let ((file (seq-find #'file-exists-p (seq-filter
                                             #'stringp
                                             momentary-popup-meta))))
    (when (get-buffer momentary-popup-inspect-buffer-name)
      (kill-buffer momentary-popup-inspect-buffer-name))
    (find-file file)))

(defvar momentary-popup-inspect-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x 0") #'kill-this-buffer)
    (define-key map (kbd "C-c C-o") #'momentary-popup-maybe-find-file)
    map))

;;;###autoload
(defun momentary-popup-inspect (content &rest setup-args)
  "Display CONTENT in popup window.

SETUP-ARGS can includes keymaps, syntax table, filename and function.
A filename can be opened with \\<momentary-popup-inspect-keymap>\ `\\[momentary-popup-maybe-find-file]'.
A function will be called without args inside quit function.

If SETUP-ARGS contains syntax table, it will be used in the inspect buffer."
  (let ((buffer (get-buffer-create momentary-popup-inspect-buffer-name))
        (keymaps (seq-filter #'keymapp setup-args))
        (stx-table (seq-find #'syntax-table-p setup-args))
        (mode-fn (seq-find #'functionp setup-args)))
    (setq momentary-popup-content (if (or
                                       mode-fn
                                       (not (stringp content)))
                                      (apply #'momentary-popup-fontify
                                             (list content mode-fn))
                                    content))
    (with-current-buffer buffer
      (with-current-buffer-window
          buffer
          (cons (or 'display-buffer-in-direction)
                '((window-height . window-preserve-size)))
          (lambda (window _value)
            (with-selected-window window
              (setq buffer-read-only t)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (momentary-popup-inspect-mode)
                (progn  (save-excursion
                          (insert momentary-popup-content))
                        (add-hook 'kill-buffer-hook
                                  #'momentary-popup-minibuffer-select-window
                                  nil t)
                        (when mode-fn
                          (funcall mode-fn))
                        (use-local-map
                         (let ((map (copy-keymap
                                     momentary-popup-inspect-keymap)))
                           (if buffer-read-only
                               (define-key map (kbd "q")
                                           #'kill-this-buffer)
                             (define-key map (kbd "q")
                                         #'self-insert-command))
                           (add-hook
                            'read-only-mode-hook
                            (lambda ()
                              (if buffer-read-only
                                  (define-key map (kbd "q")
                                              #'kill-this-buffer)
                                (define-key map (kbd "q")
                                            #'self-insert-command)))
                            t)
                           (when keymaps
                             (setq map (make-composed-keymap
                                        keymaps
                                        map)))
                           (set-keymap-parent map (current-local-map))
                           map)))))))
      (when stx-table
        (set-syntax-table stx-table))
      (setq header-line-format (or header-line-format "*Inspect*"))
      (unless (active-minibuffer-window)
        (select-window (get-buffer-window buffer))))))

;;;###autoload
(defun momentary-popup-open-inspector ()
  "Open or restore popup in a buffer `momentary-popup-inspect-buffer-name'."
  (interactive)
  (let ((file (seq-find #'file-exists-p (seq-filter
                                        #'stringp
                                        momentary-popup-meta))))
    (unless momentary-popup-content
      (setq momentary-popup-content
            (when file
              (with-temp-buffer
                (insert-file-contents file)
                (buffer-string)))))
    (apply #'momentary-popup-inspect
           (list (or momentary-popup-content "")
                 momentary-popup-meta))))

(defvar momentary-popup-switch-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") #'momentary-popup-open-inspector)
    map)
  "Keymap with commands to execute just before exiting.")

(defun momentary-popup-setup-quit-fn ()
  "Setup a quit function for the buffer `momentary-popup-momentary-buffer-name'.

Display remains until next event is input. If the input is a key binding
 of a command from `momentary-popup-switch-keymap', execute it."
  (lambda (window _value)
    (with-selected-window window
      (setq header-line-format
            (substitute-command-keys "\\<momentary-popup-switch-keymap>\
Use `\\[momentary-popup-open-inspector]' to open popup"))
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (momentary-popup-mode)
        (when momentary-popup-content
          (erase-buffer)
          (insert momentary-popup-content))
        (unwind-protect
            (setq momentary-popup-window-last-key
                  (read-key-sequence ""))
          (quit-restore-window window 'kill)
          (if (lookup-key momentary-popup-switch-keymap
                          momentary-popup-window-last-key)
              (run-at-time '0.5 nil #'momentary-popup-open-inspector)
            (setq unread-command-events
                  (append (this-single-command-raw-keys)
                          unread-command-events)))
          (setq momentary-popup-window-last-key nil))))))

;;;###autoload
(defun momentary-popup (content &rest setup-args)
  "Momentarily display CONTENT in popup window.
Display remains until next event is input.

Persist popup if input is a key binding of a command
 `momentary-popup-open-inspector'in `momentary-popup-switch-keymap'.

SETUP-ARGS can includes keymaps, syntax table, filename and function.
See a function `momentary-popup-open-inspector'."
  (let ((buffer (get-buffer-create
                 momentary-popup-momentary-buffer-name))
        (mode-fn (seq-find #'functionp setup-args)))
    (setq momentary-popup-content (if (or
                                       mode-fn
                                       (not (stringp content)))
                                      (apply #'momentary-popup-fontify
                                             (list content mode-fn))
                                    content))
    (setq momentary-popup-meta setup-args)
    (with-current-buffer buffer
      (with-current-buffer-window
          buffer
          (cons 'display-buffer-in-direction
                '((window-height . window-preserve-size)))
          (momentary-popup-setup-quit-fn)
        (momentary-popup-mode)
        (insert momentary-popup-content)))))

;;;###autoload
(defun momentary-popup-file (file)
  "Momentarily display content of the FILE in popup window.

Display remains until next event is input.

To persist popup use \\<momentary-popup-switch-keymap>\
 `\\[momentary-popup-open-inspector]'."
  (interactive "f")
  (when-let ((filename (and
                        file
                        (file-readable-p file)
                        (file-exists-p file)
                        (not (file-directory-p file))
                        file))
             (buffer (get-buffer-create
                      momentary-popup-momentary-buffer-name)))
    (setq momentary-popup-meta `(,file))
    (setq momentary-popup-content nil)
    (with-current-buffer buffer
      (with-current-buffer-window
          buffer
          (cons 'display-buffer-in-side-window
                '((window-height . fit-window-to-buffer)))
          (momentary-popup-setup-quit-fn)
        (insert-file-contents filename)
        (let ((buffer-file-name filename))
          (delay-mode-hooks (set-auto-mode)
                            (font-lock-ensure))
          (push major-mode momentary-popup-meta))
        (setq header-line-format
              (abbreviate-file-name filename))))))

;;;###autoload
(define-minor-mode momentary-popup-inspect-mode
  "Toggle `momentary-popup-inspect-mode'."
  :lighter " popup-inspect"
  :keymap momentary-popup-inspect-keymap
  :global nil)

;;;###autoload
(define-minor-mode momentary-popup-mode
  "Toggle momentary pop mode."
  :lighter " momentary"
  :keymap momentary-popup-switch-keymap
  :global nil)

(provide 'momentary-popup)
;;; momentary-popup.el ends here