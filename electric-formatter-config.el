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
(defvar ef-beginning-regexp (concat (regexp-opt '("+" "-" "~" "!")) "?\\(?:\\<\\|\\s'\\|\\s\"\\|\\s|\\|\\s(\\|\\s_\\)"))
(make-variable-buffer-local 'ef-beginning-regexp)
;;\\w include 0-9
(defvar ef-end-regexp "\\(?:\\w\\|\\s)\\|\\s\"\\)")
;; Don't add "\\_>" ,because for corner case like "/xxx/=~yyy" in ruby
(make-variable-buffer-local 'ef-end-regexp)

(defun ef-rule-space-between (pre-string post-string)
  (ef-rule-space-between-regexp (regexp-quote pre-string)
                                (regexp-quote post-string)))

(defun ef-rule-space-between-regexp (pre-regexp post-regexp)
  (cons
   (concat "\\(" (or pre-regexp ef-end-regexp) "\\)"
           "[ \t]?";; drop 1 space
           "\\(" (or post-regexp ef-beginning-regexp) "\\)")
   "\\1 \\2"))

(defun ef-rule-space-after-regexp (regexp)
  (ef-rule-space-between-regexp regexp ef-beginning-regexp))

(defun ef-rule-space-after (&rest strings);; TODO:change to list
  (ef-rule-space-after-regexp (regexp-opt strings)))

(defun ef-rule-space-before-regexp (regexp)
  (ef-rule-space-between-regexp ef-end-regexp regexp))

(defun ef-rule-space-before (&rest strings)
  (ef-rule-space-before-regexp (regexp-opt strings)))

(defun ef-rule-space-around-regexp (regexp &optional pre-regexp post-regexp)
  (cons
   (concat "\\(" (or pre-regexp ef-end-regexp) "\\)"
           "[ \t]?";; drop 1 space
           "\\(" "[ \t]*" regexp "\\)"
           "[ \t]?";; drop 1 space
           "\\(" "[ \t]*" (or post-regexp ef-beginning-regexp) "\\)")
   "\\1 \\2 \\3"))

(defun ef-rule-space-around (&rest strings)
  (ef-rule-space-around-regexp (regexp-opt strings)))

(defun ef-rule-delete-space-regexp (&optional pre-regexp post-regexp)
  (cons
   (concat "\\(" (or pre-regexp ef-end-regexp) "\\)"
           "[\t ]+";; drop all space
           "\\(" (or post-regexp ef-beginning-regexp) "\\)")
   "\\1\\2"))

(defun ef-rule-delete-space (pre-string post-string)
  (ef-rule-delete-space-regexp (regexp-quote pre-string)
                               (regexp-quote post-string)))

(defvar ef-text-mode-rule-list
  '(("\n[\n]+" . "\n\n");;Two blank lines to one blank line
    (ef-rule-space-between-regexp "[[:multibyte:]]" "[0-9A-Za-z]")
    (ef-rule-space-between-regexp "[0-9A-Za-z]" "[[:multibyte:]]")
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
                          "=~"
                          "/=" ;;"/"
                          "*=" ;;"*";; hard to support
                          "+" "+="
                          "-" "-="
                          )
    (ef-rule-space-after  "," ";")
    ;; Space before single comment
    (ef-rule-space-before-regexp "\\s<")
    ;; Don't use syntax table because punctuations include ","
    ;;(cons (concat "\\(" ef-end-regexp "\\)" "\\(\\s.\\)") "\\1 \\2")
    ))

(defvar ef-ruby-mode-rule-list
  `((ef-rule-space-around "..." ".."
                          "||=" "&&=" "<=>"
                          "!~" "|"
                          "**=" "**" "*"
                          "/")
    ;; method parameter
    (ef-rule-space-between-regexp "\\_>" "\\s\"")
    ;;(ef-rule-space-between-regexp "\\w" ":[^:]")
    ;; ternary operator
    ;;(ef-rule-space-around-regexp "[ \t]\\?" nil "[^;]*?:")
    ;;(ef-rule-space-around-regexp "\\?" nil "[^;]*?:")
    ;; "\\_<\\?" "\\?"
    ;;(ef-rule-space-between-regexp nil "\\?[^;]*?:");;before ?
    (ef-rule-space-between-regexp "\\?" "[^;]*?:");;after ?

    (ef-rule-space-between-regexp "[ \t]\\?[^;]*[^ ]" ":");;before :
    (ef-rule-space-between-regexp "[ \t]\\?[^;]*:" nil);;after :
    ;; multiple assign
    (ef-rule-space-between "," "=")
    (ef-rule-space-between ",*" "=")
    (ef-rule-space-between "," "*")
    (ef-rule-space-between ";" "*")
    (ef-rule-space-between-regexp ,(regexp-opt '("{" "do")) "|")
    (ef-rule-space-between-regexp "|" ,(regexp-opt '("}" "end")))
    (ef-rule-space-after "* =" "*=" ", =" ",=")
    (ef-rule-space-between-regexp "\\(?:[;]\\|^\\)[ /t]*\\*" "=");;between *=
    ;; block param(advance)
    ;; after 1st |
    (ef-rule-delete-space-regexp "\\(?:do\\|{\\)[^|]*|" nil)
    ;; before 2nd |
    (ef-rule-delete-space-regexp
     "\\(?:do\\|{\\)[^|]*|[^|]*" "|")
    ;; space before/after block(advance?)
    (ef-rule-space-after  "{" "do")
    (ef-rule-space-before "}" "end")
    ;; hash keyword
    (ef-rule-space-after-regexp "[{,][ \t]*\\w+:")
    ;; block parameter separator
    (ef-rule-space-after  ";")
    (ef-rule-space-after  "!")
    ;; overwrite operators
    (ef-rule-delete-space-regexp "def \\s.+" "(")
    (ef-rule-delete-space "def []" "=")
    (ef-rule-delete-space "def []=" "(")
    ;; advanced
    ;; convert keyword
    ("\\_<and\\_>" . "&&")
    ("\\_<or\\_>" . "||")))

(defvar ef-python-mode-rule-list
  '(;; delete space for default param: def foo(a=b)
    (ef-rule-delete-space-regexp "def[ \t]+\\(?:\\w\\|\\s_\\)+([^)]+=" nil)
    (ef-rule-delete-space-regexp "def[ \t]+\\(?:\\w\\|\\s_\\)+([^)]+" "=")
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
      (ef-rule-delete-space-regexp
       ,(concat keyword-regexp "[\t ]+<") nil)
      (ef-rule-delete-space-regexp
       ,(concat keyword-regexp "[^;\n]+") ">")
      ;; rvalue return func(param) value of right
      ;;(equal "b =& a" "b = &a")
      ;; (ef-rule-delete-space-regexp "=&" ef-beginning-regexp)
      ;; (ef-rule-delete-space-regexp "[;\n][ \t]*&" ef-beginning-regexp)
      (ef-rule-space-around "?" ":") ;;tertiary operator
      (ef-rule-space-around "/*" "/" "|");; TODO: use comment-start
      )))

;;http://emacswiki.org/emacs/elisp-format.el
(defvar ef-emacs-lisp-mode-rule-list
  '((ef-rule-space-after  ")" "\"")
    (ef-rule-space-before "(" "\"")
    ;; experimental
    ;; including . in symbol is not invalid
    ;;(ef-rule-space-between-regexp "\\." "\\w*?[^ \t)0-9]")
    ;;(ef-rule-space-between-regexp "[[:alpha:]]\\w*?" "\\.")

    ;; (rule-name
    ;;  ("" "")
    ;;  ("" "")
    ;;  (delete-between "" "")
    ;;  )
    (ef-rule-delete-space-regexp "," nil);;regexp
    ;;advanced
    ;;delete space trailing whitespaces :)\n)
    (")[\n\t ]+)" . "))")
    ("\n[\n]+" . "\n\n")
    ))

(setq-default ef-rule-list ef-prog-mode-rule-list)

;; TODO:integrate to text mode
(setq-default
 ef-comment-rule-list
 (append
  ef-text-mode-rule-list
  '(;;Space after single comment
    (ef-rule-space-after-regexp "\\s<")
    ;;(ef-rule-space-after-regexp "\\'")
    )))

(defun ef-ruby-mode-setup()
  (setq ef-rule-list
        ;; Must delete space in overwrite opearator
        (append ef-prog-mode-rule-list ef-ruby-mode-rule-list))
  (setq ef-comment-rule-list
        (append ef-comment-rule-list
                '((ef-rule-space-after "#=>"))))
  ;; ruby-mode treat / as punctuation
  (setq ef-beginning-regexp
        (concat "\\(?:/\\)?"
                ef-beginning-regexp))
  (setq ef-end-regexp
        (concat "\\(?:" ef-end-regexp
                "\\|" "/" "\\)"))
  )

(defun ef-text-mode-setup()
  (setq ef-rule-list ef-text-mode-rule-list))

(defun ef-emacs-lisp-mode-setup()
  (setq ef-rule-list
        (append ef-emacs-lisp-mode-rule-list))
  (setq ef-comment-rule-list
        (append ef-comment-rule-list
                '((ef-rule-delete-space ";;;" "###autoload")))))

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

(global-electric-formatter-mode)

(provide 'electric-formatter-config)
;;; electric-formatter-config.el ends here
