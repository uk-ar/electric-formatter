(require 'cl)

(defvar electric-formatter-list nil)
(make-variable-buffer-local 'electric-formatter-list)

(defun electric-formatter (string)
  (reduce (lambda (res func) (funcall func res))
          (cons string electric-formatter-list)))

(defun electric-formatter-electric (char)
  (when (= char ?\n)
    (save-excursion
      (forward-line -1)
      (let* ((start (line-beginning-position))
             (end (line-end-position))
             (str (buffer-substring-no-properties start end))
             (to-str (electric-formatter str)))
        (unless (equal str to-str)
          (delete-region start end)
          (insert to-str))
        ))))

;; replace-regexp is better?
(defun electric-formatter-replace-regexp (regexp rep)
  `(lambda (str) (replace-regexp-in-string ,regexp ,rep str) ))

(add-to-list 'electric-formatter-list
             (electric-formatter-replace-regexp ",\\(\\w\\|\\s.\\)" ", \\1"))
;; ruby
;; (replace-regexp-in-string ",\\(\\w\\|\\s.\\)" ", \\1" ":foo,:bar")

(setq-default electric-formatter-list electric-formatter-list)

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
;;comma
;; for ruby's :hoge,:fuga
;; elisp
;; (electric-indent-mode)

(define-minor-mode electric-formatter-mode
  "Toggle electric formatter."
  :lighter " EF"
  :group 'electric-formatter
  (if electric-formatter-mode
      (add-hook 'electric-indent-functions #'electric-formatter-electric t t)
    (remove-hook 'electric-indent-functions #'electric-formatter-electric t)
    )
  )

(electric-indent-mode 1)
;(electric-indent-mode 0)

(ert-deftest electric-formatter-comma ()
  (should
   (equal
    (funcall (electric-formatter-replace-regexp ",\\(\\w\\)" ", \\1") ",hoge")
    ", hoge"))
  (should
   (equal
    (electric-formatter ",hoge")
    ", hoge")))

(ert-deftest electric-formatter-paren ()
  (should
   (equal
    (funcall (electric-formatter-replace-regexp "\\(\\w\\)(" "\\1 (") "hoge(")
    "hoge ("))
  (should
   (equal
    (electric-formatter "hoge(")
    "hoge (")))

(ert-deftest electric-formatter-close-paren ()
  (should
   (equal
    (funcall (electric-formatter-replace-regexp ")\\(\\w\\)" ") \\1") ")hoge")
    ") hoge"))
  (should
   (equal
    (electric-formatter ")hoge")
    ") hoge")))
;; (ert-deftest electric-formatter ()
;;   (should
;;    (equal
;;     (execute-kbd-macro ")hoge")
;;     ") hoge"))
;;   )
(provide 'electric-formatter)
