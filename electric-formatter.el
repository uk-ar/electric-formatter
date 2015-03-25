(require 'cl)

(defvar electric-formatter-list nil)
(make-variable-buffer-local 'electric-formatter-list)
(defvar electric-formatter-default-list nil)

(defun electric-formatter (string)
  (reduce (lambda (res func) (funcall func res))
          (cons string electric-formatter-list)))

(defun electric-formatter-electric ()
  (when (memq last-command-event '(?, ?=))
    (let* (
           (start (line-beginning-position))
           (end (point))
           (str (buffer-substring-no-properties start end))
           (to-str (electric-formatter str)))
      (unless (equal str to-str)
        (delete-region start end)
        (insert to-str))
      ))
  )
;;'(b, a, b, c, d, e, g, a, b, e, f, g, f, a==b=d=e=g)

;; replace-regexp is better?
(defun electric-formatter-replace-regexp (regexp rep)
  `(lambda (str) (replace-regexp-in-string ,regexp ,rep str) ))

(add-to-list 'electric-formatter-default-list
             (electric-formatter-replace-regexp ",\\(\\w\\|\\s.\\)" ", \\1");;space after ","
             )
(add-to-list 'electric-formatter-default-list
             (electric-formatter-replace-regexp "\\(\\w\\|\\s.\\)=" "\\1 =");;space before "="
             )
;; for ruby's :hoge,:fuga
;; (replace-regexp-in-string ",\\(\\w\\|\\s.\\)" ", \\1" ":foo,:bar")

(setq-default electric-formatter-list electric-formatter-default-list)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-to-list 'electric-formatter-list
                         (electric-formatter-replace-regexp
                          "\\(\\w\\)(" "\\1 ("));;paren
            (add-to-list 'electric-formatter-list
                         (electric-formatter-replace-regexp
                          ")\\(\\w\\)" ") \\1"));;close paren
            (add-to-list 'electric-formatter-list
                         (electric-formatter-replace-regexp
                          "\\(\\w\\)_\\(\\w\\)" "\\1-\\2"));;underscore
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
