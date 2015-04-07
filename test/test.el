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

(ert-deftest electric-formatter-regexp-space-after ()
  (should
   (equal
    (electric-formatter-regexp
     '((space-after . "=")
       (space-after . ",")
       (space-before . "=")
       ("foo" . "bar"))
     'space-after)
    "\\([,=]\\)"
    )))

(ert-deftest electric-formatter-regexp-space-after-not-exist ()
  (should
   (equal
    (electric-formatter-regexp
     '((space-before . "=")
       ("foo" . "bar"))
     'space-after)
    nil
    )))

(ert-deftest electric-formatter-regexp-space-before ()
  (should
   (equal
    (electric-formatter-regexp
     '((space-after . "=")
       (space-after . ",")
       (space-before . "=")
       ("foo" . "bar"))
     'space-before)
    "\\(=\\)"
    )))

(ert-deftest electric-formatter-regexp-opt ()
  (should
   (equal
    (electric-formatter-regexp-opt
     '(("foo" . "bar")
       (space-after . "=")
       (space-after . ",")
       (space-before . "=")
       ))
    `((,(concat "\\([,=]\\)" electric-formatter-beginnig-regexp). "\\1 \\2")
      ("\\(\\w\\|\\s.\\|\\s)\\)\\(=\\)" . "\\1 \\2")
      ("foo" . "bar"))
    )))

(ert-deftest electric-formatter-regexp-opt-after-not-exist ()
  (should
   (equal
    (electric-formatter-regexp-opt
     '(("foo" . "bar")
       (space-before . "=")))
    '(("\\(\\w\\|\\s.\\|\\s)\\)\\(=\\)" . "\\1 \\2")
      ("foo" . "bar"))
    )))

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

(defun electric-formatter-test-execute (string expect &optional pre post)
  (should (equal (electric-formatter string) expect))
  (when pre (insert pre))
  (insert string)
  (when post (save-excursion (insert post)))
  ;;(execute-kbd-macro (kbd "RET"))
  (electric-formatter-electric)
  (should (equal (substring-no-properties (buffer-string))
                 (concat pre expect post)))
  (should (equal (point) (+ (length (concat pre expect)) 1)))
  (erase-buffer)
  )

(defmacro electric-formatter-test-region (string expect point)
  (declare (debug t))
  `(progn
     (insert ,string)
     (goto-char , point)
     (electric-formatter-region (+ (point-min) 1) (- (point-max) 1))
     (should (equal (substring-no-properties (buffer-string))
                    ,expect))
     (should (equal (point) , point))
     (erase-buffer)))

(ert-deftest electric-formatter-in-default ()
  (electric-formatter-test-common

   (electric-formatter-mode 1)

   (should electric-formatter-mode)
   (should (eq 3 (length electric-formatter-list)))

   (electric-formatter-test-execute ",hoge" ", hoge")
   (electric-formatter-test-execute ")hoge" ")hoge")

      (electric-formatter-test-execute ",hoge" ", hoge" nil "\n,hoge")
   (electric-formatter-test-execute ",hoge" ", hoge" ",hoge\n" nil)

   (electric-formatter-test-execute ",hoge" ", hoge" nil "\n\",hoge\"")
   (electric-formatter-test-execute ",hoge" ", hoge" nil "\",hoge\"")
   (electric-formatter-test-execute ",hoge" ", hoge" "\",hoge\"\n" nil)
   (electric-formatter-test-execute ",hoge" ", hoge" "\",hoge\"" nil)
   (electric-formatter-test-execute ",hoge" ", hoge"
                                    "\",hoge\"\n" "\",hoge\"\n")

   (electric-formatter-test-execute ",hoge" ", hoge" nil "\n;,hoge")
   (electric-formatter-test-execute ",hoge" ", hoge" nil ";,hoge")
   (electric-formatter-test-execute ",hoge" ", hoge" ";,hoge\n" nil)
   (electric-formatter-test-execute ",hoge" ", hoge" ";,hoge\n" ";,hoge")
   ;;(electric-formatter-test-execute ",hoge" ", hoge" ";,hoge" nil)

   ;;end of region
   (electric-formatter-test-region "\n1,2\n" "\n1, 2\n" (- (point-max) 1))
   ;;beginnig of region
   (electric-formatter-test-region "\n1,2\n" "\n1, 2\n" (+ (point-min) 1))
   ;;above region
   (electric-formatter-test-region "\n1,2\n" "\n1, 2\n" (point-min))
   ;;below region
   (electric-formatter-test-region "\n1,2\n" "\n1, 2\n" (point-max))
   ;;inside region
   (electric-formatter-test-region "\n1,2\n" "\n1, 2\n" (+ (point-min) 2))
   )
  )

(ert-deftest electric-formatter-in-elisp ()
  (electric-formatter-test-common

   (emacs-lisp-mode)
   (electric-formatter-mode 1)

   (should electric-formatter-mode)
   ;;(should (eq (length electric-formatter-list) 6))
   (electric-formatter-test-execute "a" "a")
   ;;(electric-pair-mode 1)
   (electric-formatter-test-execute ")hoge" ") hoge")
   (electric-formatter-test-execute "hoge(" "hoge (")

   (electric-formatter-test-execute ")(" ") (")
   (electric-formatter-test-execute ")`(" ") `(")

   (electric-formatter-test-execute "),(" ") ,(")
   ;;(electric-formatter-test-execute ")\"" ") \"")
   ))

(ert-run-tests-interactively t)
