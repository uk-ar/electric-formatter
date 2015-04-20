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
;; Including symbol(\\s_) for ruby's symbol ":foo"
;; Including *& for c's pointer operator ADD ++ -- + - ~ !
;; Including | for pythons string "'"
(defvar ef-beginning-regexp "\\(?:\\|+\\|-\\|~\\|!\\)?\\(?:\\_<\\|\\w\\|\\s'\\|\\s\"\\|\\s(\\|\\s_\\|\\s|\\)")
(make-variable-buffer-local 'ef-beginning-regexp)

(defvar ef-end-regexp "\\(?:\\_>\\|\\w\\|\\s)\\|\\s\"\\)")
(make-variable-buffer-local 'ef-end-regexp)

(defun ef-rule-space-between (pre-string post-string)
  (ef-rule-space-between-regexp (regexp-quote pre-string)
                                (regexp-quote post-string)))

(defun ef-rule-space-between-regexp (pre-regexp post-regexp)
  (cons
   (concat "\\(" pre-regexp "\\)"
           "[ \t]?";; drop 1 space
           "\\(" post-regexp "\\)")
   "\\1 \\2"))

(defun ef-rule-space-after-regexp (regexp)
  (ef-rule-space-between-regexp regexp ef-beginning-regexp))

(defun ef-rule-space-after (&rest strings)
  (ef-rule-space-after-regexp (regexp-opt strings)))

(defun ef-rule-space-before-regexp (regexp)
  (ef-rule-space-between-regexp ef-end-regexp regexp))

(defun ef-rule-space-before (&rest strings)
  (ef-rule-space-before-regexp (regexp-opt strings)))

(defun ef-rule-space-around-regexp (regexp)
  (cons
   (concat "\\(" ef-end-regexp "\\)"
           "[ \t]?";; drop 1 space
           "\\(" "[ \t]*" regexp "\\)"
           "[ \t]?";; drop 1 space
           "\\(" "[ \t]*" ef-beginning-regexp "\\)")
   "\\1 \\2 \\3"))

(defun ef-rule-space-around (&rest strings)
  (ef-rule-space-around-regexp (regexp-opt strings)))

;; todo regexp
(defun ef-rule-delete-space (pre-regexp post-regexp)
  (cons
   (concat "\\(" pre-regexp "\\)" "[\t ]+" "\\(" post-regexp "\\)")
   "\\1\\2"))

(defvar ef-text-mode-rule-list
  '(("\n[\n]+" . "\n\n");;Two blank lines to one blank line
    (ef-rule-space-between-regexp "[[:multibyte:]]" "[[:unibyte:]]")
    (ef-rule-space-between-regexp "[[:unibyte:]]" "[[:multibyte:]]")
    ;;https://github.com/zk-phi/electric-spacing
    (ef-rule-space-after ",")
    ))

(defvar ef-prog-mode-rule-list
  '(("\n[\n]+" . "\n\n") ;;Two blank lines to one blank line
    ;; Don't use "&" because of poiter operator
    (ef-rule-space-around "=" "==" "==="
                          "=>" ">=" "<=" "<<=" ">>=" "=<"
                          ">" ">>" "<" "<<"
                          "||" "|=" ;; "|" TODO: conflict with ruby
                          "!="
                          "&" "&=" "&&"
                          "^" "^="
                          "%" "%="
                          "/=" ;;"/"
                          "*=" ;;"*";; hard to support
                          "+" "+="
                          "-" "-="
                          )
    (ef-rule-space-after  ",")
    (ef-rule-space-before-regexp "\\s<")
    ;; Don't use syntax table because punctuations include ","
    ;;(cons (concat "\\(" ef-end-regexp "\\)" "\\(\\s.\\)") "\\1 \\2")
    ))

(defvar ef-ruby-mode-rule-list
  '((ef-rule-space-around "..." ".."
                          "||=" "&&=" "<=>"
                          "!~" "|"
                          "**=" "**" "*"
                          "/" "=~")
    ;; method parameter
    (ef-rule-space-between-regexp "\\_>" "\\s\"")
    ;;(ef-rule-space-between-regexp "\\w" ":[^:]")
    ;;tertiary operator
    (ef-rule-space-between-regexp ef-end-regexp "\\?[^;]*:");;before ?
    (ef-rule-space-between-regexp "\\?" "[^;]*:");;after ?
    (ef-rule-space-between-regexp "\\?[^;]*[^ ]" ":");;before :
    (ef-rule-space-between-regexp "\\?[^;]*:" ef-beginning-regexp);;after :
    ;; multiple assign
    (ef-rule-space-between "," "=")
    (ef-rule-space-between ",*" "=")
    (ef-rule-space-between "," "*")
    (ef-rule-space-between-regexp (regexp-opt '("{" "do")) "|")
    (ef-rule-space-between-regexp "|" (regexp-opt '("}" "end")))
    (ef-rule-space-after "* =" "*=" ", =" ",=")
    (ef-rule-space-between-regexp "\\(?:[;]\\|^\\)[ /t]*\\*" "=");;between *=
    ;; block param
    ;; after 1st |
    (ef-rule-delete-space "\\(?:do\\|{\\)[^|]*|" ef-beginning-regexp)
    ;; before 2nd |
    (ef-rule-delete-space
     "\\(?:do\\|{\\)[^|]*\\(?:|[^| ]*\\(?:[ ][^ |]+\\)*\\)" "|")
    ;; space before/after block
    (ef-rule-space-after  "{" "do")
    (ef-rule-space-before "}" "end")
    ;; hash keyword
    (ef-rule-space-after-regexp "[{,][ \t]*\\w+:")
    ;; block parameter separator
    (ef-rule-space-after  ";")
    (ef-rule-space-after  "!")
    ;; advanced
    ;; convert keyword
    ("\\_<and\\_>" . "&&")
    ("\\_<or\\_>" . "||")))

(defvar ef-python-mode-rule-list
  '(;;delete space for default param: foo(a=b)
    (ef-rule-delete-space "[(,][^(,]+" "=")
    (ef-rule-delete-space "[(,][^(,]+=" ef-beginning-regexp)
    ))

(defvar ef-c-mode-rule-list
  ;;copy keyword from electric-spacing
  ;;https://github.com/xwl/electric-spacing/blob/master/electric-spacing.el#L17
  (let ((keyword-regexp
         (regexp-opt '("#include" "vector" "deque" "list" "map" "stack"
                       "multimap" "set" "hash_map" "iterator" "template"
                       "pair" "auto_ptr" "static_cast" "dynmaic_cast"
                       "const_cast" "reintepret_cast" "#import"))))
    `(;;delete space for keyword :#include <foo.h>
      (ef-rule-delete-space (concat ,keyword-regexp "[\t ]+<")
                            ef-beginning-regexp)
      (ef-rule-delete-space (concat ,keyword-regexp "[^;\n]+")
                            ">")
      ;; rvalue return func(param) value of right
      ;;(equal "b =& a" "b = &a")
      ;; (ef-rule-delete-space "=&" ef-beginning-regexp)
      ;; (ef-rule-delete-space "[;\n][ \t]*&" ef-beginning-regexp)
      (ef-rule-space-around "?" ":") ;;tertiary operator
      (ef-rule-space-around "/*" "/" "|")
      )))

;;http://emacswiki.org/emacs/elisp-format.el
(defvar ef-emacs-lisp-mode-rule-list
  '((ef-rule-space-after  ")" "\"" ".")
    (ef-rule-space-before "(" "\"" ".")
    (ef-rule-delete-space "," ef-beginning-regexp);;regexp
    ;;advanced
    ;;delete space trailing whitespaces :)\n)
    '(")[\n\t ]+)" . "))")
    '("\n[\n]+" . "\n\n")
    ))

(setq-default ef-rule-list ef-prog-mode-rule-list)

(setq-default
 ef-comment-rule-list
 '(;;Space after single comment
   (ef-rule-space-after-regexp "\\s<")
   ;;(ef-rule-space-after-regexp "\\'")
   ))

(global-electric-formatter-mode 1)

(defun ef-ruby-mode-setup()
  (setq ef-rule-list
        (append ef-ruby-mode-rule-list ef-prog-mode-rule-list))
  (setq ef-comment-rule-list
        (append ef-comment-rule-list
                '((ef-rule-space-after "#=>"))))
  ;; / is hundled as punctuation
  (setq ef-beginning-regexp
        (concat "\\(?:/\\)?"
                ef-beginning-regexp))
  ;;(defvar ef-end-regexp "\\(?:\\_>\\|\\w\\|\\s)\\|s\"\\)")
  (setq ef-end-regexp
        (concat "\\(?:" ef-end-regexp
                "\\|" "/" "\\)"))
  )

(defun ef-text-mode-setup()
  (setq ef-rule-list ef-text-mode-rule-list))

(defun ef-emacs-lisp-mode-setup()
  (setq ef-rule-list
        (append ef-emacs-lisp-mode-rule-list)))

(defun ef-python-mode-setup()
  (setq ef-rule-list
        (append ef-prog-mode-rule-list ef-python-mode-rule-list)))

(defun ef-c-mode-setup()
  (setq ef-rule-list
        (append ef-prog-mode-rule-list ef-c-mode-rule-list))
  (setq ef-comment-rule-list
        (append ef-comment-rule-list
                '((ef-rule-space-after  "//" "/*")
                  (ef-rule-space-before "*/"))))
  (setq ef-beginning-regexp
        (concat "\\(?:++\\|--\\|&\\|*\\)?"
                ef-beginning-regexp))
  ;;(defvar ef-end-regexp "\\(?:\\_>\\|\\w\\|\\s)\\|s\"\\)")
  (setq ef-end-regexp
        (concat "\\(?:" ef-end-regexp
                "\\|" (regexp-opt '("++" "--")) "\\)"))
  )

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
