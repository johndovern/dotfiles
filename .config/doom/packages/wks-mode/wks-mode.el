;;; wks-mode.el --- Major mode for editing wks files -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2023, 3L0C
;; Author: 3L0C ( dotbox@mailbox.org )
;; Version: 0.0.1
;; Created: December 24 2023
;; Keywords: Configuration files
;; Homepage: https://codeberg.org/3L0C/wks-mode
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;;; License: GPLv3

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;  ...
;;; Code:

;;; Vars

;; TODO use defvar directly in the future instead of setq.
(defvar wks-mode-syntax-table nil "Syntax table for `wks-mode'.")

(setq wks-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "." st)
    ;; (modify-syntax-entry ?\n ">" st)
    ;; (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?' "." st)
    (modify-syntax-entry ?\" "." st)
    (modify-syntax-entry ?\( "." st)
    (modify-syntax-entry ?\) "." st)
    (modify-syntax-entry ?\{ "." st)
    (modify-syntax-entry ?\} "." st)
    (modify-syntax-entry ?\[ "." st)
    (modify-syntax-entry ?\] "." st)
    st))

(defvar wks-mode--font-lock-keywords nil "Font-lock keywords for `wks-mode'.")

(setq wks-mode--font-lock-keywords
      (let* (
             ;; define several category of keywords
             (wks-keywords '(
                             "after" "async-before" "before"
                             "close" "inherit" "keep" "no-after"
                             "no-before" "sync-after" "sync-command"
                             "unhook" "write"
                             ;; keywords
                             ))
             (keywords-regexp (regexp-opt wks-keywords 'words))
             ;; end
             )
        `(
          ;; Color commands
          ;; comments
          ("^\s*\\(#.*$\\)" 1 'font-lock-comment-face)
          ;; commands
          ("%{{\\(.*?\\)}}" 1 'font-lock-constant-face)
          ;; Treat everything in brackets as a key [abcd] `hint' etc.
          ("\\[\\(\\(?:\\\\.\\|[^]\\\\]\\)*?\\)\\]\s*\"" 1 'font-lock-type-face)
          ;; keywords
          (,keywords-regexp . 'font-lock-keyword-face)
          ;; Give some color to delimiters.
          ("\\(%{{\\|}}\\)" . 'font-lock-builtin-face)
          ("\\(\\[\\|\\]\\)" . 'font-lock-builtin-face)
          ("\\(^{$\\|\s*{$\\|^}$\\|\s+}$\\)" . 'font-lock-builtin-face)
          ;; Match keys i.e. C-H-a etc.
          ("^\s*?\\(.*?\\)\s+\"" 1 'font-lock-type-face)
          ;; double quotes as "strings"
          ;; ("[^\\]\\(\"\\(?:\\\\.\\|[^\\\\]\\)*?\"\\)" 1 'font-lock-string-face)
          ("\s+\\(\"\\(?:\\\\.\\|[^\\\\]\\)*?\"\\)" 1 'font-lock-string-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

;; TODO implement indentation logic.

;;; Define Major Mode

;;;###autoload
(define-derived-mode wks-mode fundamental-mode "wks"
  "Major mode for editing wks files."
  (set-syntax-table wks-mode-syntax-table)
  (setq font-lock-defaults '((wks-mode--font-lock-keywords))))

;; Associate `.wks' extension with `wks-mode'.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wks\\'" . wks-mode))

;; add the mode to the `features' list
(provide 'wks-mode)

;;; wks-mode.el ends here
