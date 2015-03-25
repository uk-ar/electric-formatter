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
  ;;(execute-kbd-macro (kbd "RET"))
  (electric-formatter-electric-1)
  (replace-regexp-in-string "[ \t\n]*$" ""
                            (substring-no-properties (buffer-string)))
  )

(ert-deftest electric-formatter-in-default ()
  (electric-formatter-test-common

   (electric-formatter-mode 1)

   (should electric-formatter-mode)
   (should (eq (length electric-formatter-list) 2))

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
   (should (eq (length electric-formatter-list) 5))

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
