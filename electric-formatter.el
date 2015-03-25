(require 'cl)

(defvar electric-formatter-list nil)
(make-variable-buffer-local 'electric-formatter-list)
(defvar electric-formatter-default-list nil)

;; replace-regexp is better?
(defun electric-formatter-replace-regexp (regexp rep)
  `(lambda (str) (replace-regexp-in-string ,regexp ,rep str) ))

(defun electric-formatter (string)
  (reduce (lambda (res rule) (replace-regexp-in-string (car rule) (cdr rule) res))
          (cons string electric-formatter-list)))

(defun electric-formatter-electric-1 ()
  (let* ((start (line-beginning-position))
         (end (point))
         (str (buffer-substring-no-properties start end))
         (to-str (electric-formatter str)))
    (unless (equal str to-str)
      (delete-region start end)
      (insert to-str))
    ))
(defun electric-formatter-electric ()
  (when (memq last-command-event '(?, ?=))
    (electric-formatter-electric-1)
    )
  )
;;'(ba,b,c, d, e, g, a, b, e, f, g, f, a==b=d=e=g=)

(add-to-list 'electric-formatter-default-list
             '(",\\(\\w\\|\\s.\\)" . ", \\1");;space after ","
             )
(add-to-list 'electric-formatter-default-list
             '("\\(\\w\\|\\s.\\)=" . "\\1 =");;space before "="
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
