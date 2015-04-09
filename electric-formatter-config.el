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

;;; Setting:
(setq-default ef-rule-list
              '((space-after . "=")
                (space-after . ",")
                (space-after . ">")
                (space-after . "<")
                (space-after . "&")
                (space-after . "|")

                (space-before . "=")
                (space-before . ">")
                (space-before . "<")
                (space-before . "&")
                (space-before . "|")
                ("\n\n" . "\n");;Two blank lines to one blank line
                ;; multibyte
                ))

;;http://emacswiki.org/emacs/elisp-format.el
(defun ef-emacs-lisp-mode-setup()
  (setq ef-rule-list
        '(;;common
          ;;(space-after . "=") for symbol name
          ;;(space-after . ",") for macro escape
          ;;(space-before . "=") for symbol name
          ("\n[\n]+" . "\n\n");;Two blank lines to one blank line

          ;;mode specify
          (space-after . ")")
          (space-after . "\"")
          (space-after . ".")

          (space-before . "(")
          (space-before . "\"")
          (space-before . ".")
          ;;advanced
          (")[\n\t ]+)" . "))")
          )))
;; (setq-default ef-comment-list
;;               '((space-after . "comment")
;;                 ))

(defun ef-ruby-mode-setup()
  (setq ef-rule-list
        `(("\n[\n]+" . "\n\n");;Two blank lines to one blank line
          (,(concat "\\(\\s.\\)" ef-beginning-regexp) . "\\1 \\2")
          (,(concat ef-end-regexp "\\(\\s.\\)") . "\\1 \\2")
          ;;advanced
          ("\\_<and\\_>" . "&&")
          ("\\_<or\\_>" . "||")
          )))

(defun ef-text-mode-setup()
  (setq ef-rule-list
        '(("\n[\n]+" . "\n\n");;Two blank lines to one blank line
          ;;Space between multibyte and unibyte
          ("\\([[:multibyte:]]\\)\\([[:unibyte:]]\\)" . "\\1 \\2")
          ("\\([[:unibyte:]]\\)\\([[:multibyte:]]\\)" . "\\1 \\2"))
        ))

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
