(ert-deftest electric-formatter-comma ()
  (should
   (equal
    (electric-formatter-1 ",hoge" '(",\\(\\w\\)" . ", \\1"))
    ", hoge")))

(ert-deftest electric-formatter-regex-space-after ()
  (should
   (equal
    (electric-formatter-regex
     '((space-after . "=")
       (space-after . ",")
       (space-before . "=")
       ("foo" . "bar"))
     'space-after)
    "\\([,=]\\)"
    )))

(ert-deftest electric-formatter-regex-space-after-not-exist ()
  (should
   (equal
    (electric-formatter-regex
     '((space-before . "=")
       ("foo" . "bar"))
     'space-after)
    nil
    )))

(ert-deftest electric-formatter-regex-space-before ()
  (should
   (equal
    (electric-formatter-regex
     '((space-after . "=")
       (space-after . ",")
       (space-before . "=")
       ("foo" . "bar"))
     'space-before)
    "\\(=\\)"
    )))

(ert-deftest electric-formatter-regex-opt ()
  (should
   (equal
    (electric-formatter-regex-opt
     '(("foo" . "bar")
       (space-after . "=")
       (space-after . ",")
       (space-before . "=")
       ))
    '(("\\([,=]\\)\\(\\w\\|\\s.\\)" . "\\1 \\2")
      ("\\(\\w\\|\\s.\\)\\(=\\)" . "\\1 \\2")
      ("foo" . "bar"))
    )))

(ert-deftest electric-formatter-regex-opt-after-not-exist ()
  (should
   (equal
    (electric-formatter-regex-opt
     '(("foo" . "bar")
       (space-before . "=")))
    '(("\\(\\w\\|\\s.\\)\\(=\\)" . "\\1 \\2")
      ("foo" . "bar"))
    )))

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
  (substring-no-properties (buffer-string)))

(ert-deftest electric-formatter-in-default ()
  (electric-formatter-test-common

   (electric-formatter-mode 1)

   (should electric-formatter-mode)
   (should (eq 3 (length electric-formatter-list)))

   (should (equal (electric-formatter ",hoge") ", hoge"))
   (should (equal (electric-formatter-test-execute ",hoge") ", hoge"))
   (should (equal (point) (+ (length ", hoge") 1)))
   (erase-buffer)

   (should (equal (electric-formatter ")hoge") ")hoge"))
   (should (equal (electric-formatter-test-execute ")hoge") ")hoge"))
   (should (equal (point) (+ (length ")hoge") 1)))
   (erase-buffer)
   )
  )

(ert-deftest electric-formatter-in-elisp ()
  (electric-formatter-test-common

   (emacs-lisp-mode)
   (electric-formatter-mode 1)

   (should electric-formatter-mode)
   ;;(should (eq (length electric-formatter-list) 6))

   ;;end of buffer
   (should (equal (electric-formatter-test-execute "a") "a"))
   (should (equal (point) (+ (point-min) 1)))
   (erase-buffer)

   (should (equal (electric-formatter ",hoge") ", hoge"))
   (should (equal (electric-formatter-test-execute ",hoge") ", hoge"))
   (should (equal (point) (+ (length ", hoge") 1)))
   (erase-buffer)

   ;;(electric-pair-mode 1)
   (should (equal (electric-formatter ")hoge") ") hoge"))
   (should (equal (electric-formatter-test-execute ")hoge") ") hoge"))
   (should (equal (point) (+ (length ") hoge") 1)))
   (erase-buffer)

   ;; (should (equal (electric-formatter "hoge(") "hoge ("))
   ;; (should (equal (electric-formatter-test-execute "hoge(") "hoge ("))
   ;; (should (equal (point) 7))
   ;; (erase-buffer)

   (insert ",hoge\n")
   (should (equal (electric-formatter-test-execute ",hoge") ",hoge\n, hoge"))
   (should (equal (point) (+ (length ",hoge\n, hoge") 1)))
   (erase-buffer)

   (insert "\n,hoge")
   (goto-char (point-min))
   (should (equal (electric-formatter-test-execute ",hoge") ", hoge\n,hoge"))
   (should (equal (point) (+ (length ", hoge") 1)))
   (erase-buffer)

   (should (equal (electric-formatter-test-execute ",hoge \",hoge")
                  ", hoge \",hoge"))
   (should (equal (point) (+ (length ", hoge \",hoge") 1)))
   (erase-buffer)

   (insert "\n\",hoge")
   (goto-char (point-min))
   (should (equal (electric-formatter-test-execute ",hoge") ", hoge\n\",hoge"))
   (should (equal (point) (+ (length ", hoge") 1)))
   (erase-buffer)

   (should (equal (electric-formatter-test-execute ",hoge ;,hoge")
                  ", hoge ;,hoge"))
   (should (equal (point) (+ (length ", hoge ;,hoge") 1)))
   (erase-buffer)

   (insert "\n;,hoge")
   (goto-char (point-min))
   (should (equal (electric-formatter-test-execute ",hoge") ", hoge\n;,hoge"))
   (should (equal (point) (+ (length ", hoge") 1)))
   (erase-buffer)

   ;; (insert "1,2")
   ;; (goto-char (point-min))
   ;; ;;(set-mark-command 1)
   ;; (goto-char (end-of-line))
   ;; (electric-formatter-region (point-min) (point-max))
   ;; ;; (should (equal (substring-no-properties (buffer-string))
   ;; ;;                "1, 2"))
   ;; (should (equal (point) (+ (length ", hoge") 1)))
   ;; (erase-buffer)
   ))

(ert-run-tests-interactively t)
