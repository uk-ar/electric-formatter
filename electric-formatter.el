(require 'cl)

(defvar electric-formatter-list nil)
(make-variable-buffer-local 'electric-formatter-list)
(defvar electric-formatter-default-list nil)

(defun electric-formatter (string)
  (reduce #'electric-formatter-1
          (cons string electric-formatter-list)))

(defun electric-formatter-1 (string rule)
  (replace-regexp-in-string (car rule) (cdr rule) string))

(defun electric-formatter-electric-1 (start end)
  (let* ((str (buffer-substring-no-properties start end))
         (to-str (electric-formatter str)))
    (unless (equal str to-str)
      (delete-region start end)
      (insert to-str))
    ))

(defun electric-formatter-electric ()
  (electric-formatter-electric-1 (line-beginning-position) (point))
  )
;;'(ba, b, c, d, e, g, a, b, e, f, g, f, a == b = d = e = g = d == b =,= a, b, d, e, e,, f == f == def == g = aba, be, f == a, a, e === b = f)

(add-to-list 'electric-formatter-default-list
             '(",\\(\\w\\)" . ", \\1");;space after "," ??",\\(\\w\\|\\s.\\)"
             )
(add-to-list 'electric-formatter-default-list
             '("\\(\\w\\)=" . "\\1 =");;space before "="
             )
(add-to-list 'electric-formatter-default-list
             '("=\\(\\w\\)" . "= \\1");;space after "="
             )
;; for ruby's :hoge,:fuga
;; (replace-regexp-in-string ",\\(\\w\\|\\s.\\)" . ", \\1" . ":foo,:bar")

(setq-default electric-formatter-list electric-formatter-default-list)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-to-list 'electric-formatter-list
                         '("\\(\\w\\)(" . "\\1 ("));;paren
            (add-to-list 'electric-formatter-list
                         '(")\\(\\w\\)" . ") \\1"));;close paren
            (add-to-list 'electric-formatter-list
                         '("\\(\\w\\)_\\(\\w\\)" . "\\1-\\2"));;underscore
            ))

(define-minor-mode electric-formatter-mode
  "Toggle electric formatter."
  :global t
  :lighter " EF"
  :group 'electric-formatter
  (if electric-formatter-mode
      (progn
        (add-hook 'post-self-insert-hook #'electric-formatter-electric t t)
        )
    (remove-hook 'post-self-insert-hook #'electric-formatter-electric t)))

(provide 'electric-formatter)
