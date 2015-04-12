;;; electric-formatter-config.el --- electric-formatter additional configuations

;;-------------------------------------------------------------------
;;
;; Copyright (C) 2015 Yuuki Arisawa
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;-------------------------------------------------------------------

;;; Commentary:

;;; Code:
(require 'electric-formatter)

(defun ef-rule-space-after (&rest strings)
  (cons
   (concat "\\(" (regexp-opt strings) "\\)" ef-beginning-regexp)
   "\\1 \\2"))

(defun ef-rule-space-before (&rest strings)
  (cons
   (concat ef-end-regexp "\\(" (regexp-opt strings) "\\)")
   "\\1 \\2"))

 ;;; Setting:
(defvar ef-text-mode-rule-list
  (list
   '("\n[\n]+" . "\n\n");;Two blank lines to one blank line
   '("\\([[:multibyte:]]\\)\\([[:unibyte:]]\\)" . "\\1 \\2")
   '("\\([[:unibyte:]]\\)\\([[:multibyte:]]\\)" . "\\1 \\2")
   (ef-rule-space-after ",")
   ))

(defvar ef-prog-mode-rule-list
  (list
   '("\n[\n]+" . "\n\n") ;;Two blank lines to one blank line
   (ef-rule-space-after  "=" ">" "<" "&" "|" ",")
   (ef-rule-space-before "=" ">" "<" "&" "|")
   ;; Don't use syntax table because punctuations include ","
   ;;(cons (concat ef-end-regexp "\\(\\s.\\)") "\\1 \\2")
   ))

(defvar ef-ruby-mode-rule-list
  (list
   ;;advanced
   '("\\_<and\\_>" . "&&")
   '("\\_<or\\_>" . "||")))

(defvar ef-python-mode-rule-list
  (list
   ;;for default param
   (cons "\\((.*\\)[\t ]+\\(=\\)" "\\1\\2")
   ))

(defvar ef-c-mode-rule-list
  (list
   ;;for #include <foo.h>
   (cons (concat "\\(<\\) " ef-beginning-regexp) "\\1\\2")
   ;;(concat ef-end-regexp "\\(" (regexp-opt strings) "\\)")
   ))

;;http://emacswiki.org/emacs/elisp-format.el
(defvar ef-emacs-lisp-mode-rule-list
  (list
   (ef-rule-space-after  ")" "\"" ".")
   (ef-rule-space-before "(" "\"" ".")
   ;;advanced
   '(")[\n\t ]+)" . "))")
   ))

(setq-default ef-rule-list ef-prog-mode-rule-list)

(setq-default
 ef-comment-rule-list
 (list
  (cons (concat "\\(" "\\s<" "\\)" ef-beginning-regexp) "\\1 \\2")
  ))

(global-electric-formatter-mode 1)

(defun ef-ruby-mode-setup()
  (setq ef-rule-list
        (append ef-prog-mode-rule-list ef-ruby-mode-rule-list)))

(defun ef-text-mode-setup()
  (setq ef-rule-list ef-text-mode-rule-list))

(defun ef-emacs-lisp-mode-setup()
  (setq ef-rule-list
        (append ef-prog-mode-rule-list ef-emacs-lisp-mode-rule-list)))

(defun ef-python-mode-setup()
  (setq ef-rule-list
        (append ef-prog-mode-rule-list ef-python-mode-rule-list)))

(defun ef-c-mode-setup()
  (setq ef-rule-list
        (append ef-prog-mode-rule-list ef-c-mode-rule-list))
  (setq ef-comment-rule-list
        (list
         (ef-rule-space-after  "//" "/*"))))

(add-hook 'c-mode-common-hook
          'ef-c-mode-setup)

(add-hook 'python-mode-hook
          'ef-python-mode-setup)

(add-hook 'emacs-lisp-mode-hook
          'ef-emacs-lisp-mode-setup)

(add-hook 'lisp-interaction-mode-hook
          'ef-emacs-lisp-mode-setup)

(add-hook 'ruby-mode-hook
          'ef-ruby-mode-setup)

(add-hook 'org-mode-hook
          'ef-text-mode-setup)

(add-hook 'markdown-mode-hook
          'ef-text-mode-setup)

(provide 'electric-formatter-config)
;;; electric-formatter-config.el ends here
