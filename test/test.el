(ert-deftest electric-formatter-comma ()
  (should
   (equal
    (electric-formatter-1 ",hoge" '(",\\(\\w\\)" . ", \\1"))
    ", hoge")))

(ert-deftest electric-formatter-paren ()
  (should
   (equal
    (electric-formatter-1 "hoge(" '("\\(\\w\\)(" . "\\1 ("))
    "hoge (")))

(ert-deftest electric-formatter-close-paren ()
  (should
   (equal
    (electric-formatter-1 ")hoge" '(")\\(\\w\\)" . ") \\1"))
    ") hoge")))

(ert-deftest electric-formatter-before-= ()
  (should
   (equal
    (electric-formatter-1 "a=" '("\\(\\w\\|\\s.\\)=" . "\\1 ="))
    "a =")))

(ert-deftest electric-formatter-after-= ()
  (should
   (equal
    (electric-formatter-1 "=a" '("=\\(\\w\\)" . "= \\1"))
    "= a")))

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
  (electric-formatter-electric)
  (replace-regexp-in-string "[ \t\n]*$" ""
                            (substring-no-properties (buffer-string)))
  )

(ert-deftest electric-formatter-in-default ()
  (electric-formatter-test-common

   (electric-formatter-mode 1)

   (should electric-formatter-mode)
   ;;(should (eq (length electric-formatter-list) 3))

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
   ;;(should (eq (length electric-formatter-list) 6))

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
