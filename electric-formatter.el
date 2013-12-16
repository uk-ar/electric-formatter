(require 'cl)

(defvar electric-formatter-list nil)
(make-variable-buffer-local 'electric-formatter-list)
(defvar electric-formatter-default-list nil)

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

(add-to-list 'electric-formatter-default-list
             (electric-formatter-replace-regexp ",\\(\\w\\|\\s.\\)" ", \\1"))
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
        (electric-indent-mode 1)
        (add-hook 'electric-indent-functions #'electric-formatter-electric t t))
    (remove-hook 'electric-indent-functions #'electric-formatter-electric t)))

(ert-deftest electric-formatter-comma ()
  (should
   (equal
    (funcall (electric-formatter-replace-regexp ",\\(\\w\\)" ", \\1") ",hoge")
    ", hoge")))

(ert-deftest electric-formatter-paren ()
  (should
   (equal
    (funcall (electric-formatter-replace-regexp "\\(\\w\\)(" "\\1 (") "hoge(")
    "hoge (")))

(ert-deftest electric-formatter-close-paren ()
  (should
   (equal
    (funcall (electric-formatter-replace-regexp ")\\(\\w\\)" ") \\1") ")hoge")
    ") hoge")))

(defmacro electric-formatter-test-common (&rest body)
  (declare (debug t))
  `(with-temp-buffer
  ;; `(with-current-buffer (get-buffer-create "hoge")
     (switch-to-buffer (current-buffer))
     (fundamental-mode)
     (erase-buffer)
     ;; (should-not electric-indent-mode)
     ;; (should-not electric-formatter-mode)
     (unwind-protect
         ,@body
       ;; (electric-indent-mode -1)
       ;; (electric-formatter-mode -1)
       ;; chomp
       (replace-regexp-in-string "[ \t\n]*$" ""
                                 (substring-no-properties (buffer-string)))
       )))

(defun electric-formatter-test-execute (string)
  (insert string)
  (execute-kbd-macro (kbd "RET"))
  (replace-regexp-in-string "[ \t\n]*$" ""
                            (substring-no-properties (buffer-string)))
  )

(ert-deftest electric-formatter-in-default ()
  (electric-formatter-test-common

   (electric-formatter-mode 1)

   (should electric-formatter-mode)
   (should (eq (length electric-indent-functions) 2))
   (should (eq (length electric-formatter-list) 1))

   (should (equal (electric-formatter ",hoge") ", hoge"))
   (should (equal (electric-formatter-test-execute ",hoge") ", hoge"))
   )
  (electric-formatter-test-common

   (electric-formatter-mode 1)

   (should (equal (electric-formatter ")hoge") ")hoge"))
   (should (equal (electric-formatter-test-execute ")hoge") ")hoge"))
   )
  )

(ert-deftest electric-formatter-in-elisp ()
  (electric-formatter-test-common

   (emacs-lisp-mode)
   (electric-formatter-mode 1)

   (should electric-formatter-mode)
   (should (eq (length electric-indent-functions) 2))
   (should (eq (length electric-formatter-list) 4))

   (should (equal (electric-formatter ",hoge") ", hoge"))
   (should (equal (electric-formatter-test-execute ",hoge") ", hoge"))
   )
  (electric-formatter-test-common

   (emacs-lisp-mode)
   (electric-formatter-mode 1)
   (should (equal (electric-formatter ")hoge") ") hoge"))
   (should (equal (electric-formatter "hoge(") "hoge ("))
   (should (equal (electric-formatter-test-execute ")hoge") ") hoge"))
   )
  )

(provide 'electric-formatter)
