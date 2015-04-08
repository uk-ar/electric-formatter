(require 'electric-formatter)
(require 'ert)
(require 'ert-x)

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

(ert-deftest electric-formatter-blank-lines ()
  (should
   (equal
    (electric-formatter-1 "\n\n" '("\n\n" . "\n"))
    "\n")))

(ert-deftest electric-formatter-whitespace ()
  (should
   (equal
    (electric-formatter-1 ") \n )" '(")[\n\t ]+)" . "))"))
    "))")))

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
    `((,(concat "\\([,=]\\)" electric-formatter-beginning-regexp) . "\\1 \\2")
      (,(concat electric-formatter-end-regexp "\\(=\\)") . "\\1 \\2")
      ("foo" . "bar"))
    )))

(ert-deftest electric-formatter-regexp-opt-after-not-exist ()
  (should
   (equal
    (electric-formatter-regexp-opt
     '(("foo" . "bar")
       (space-before . "=")))
    `((,(concat electric-formatter-end-regexp "\\(=\\)") . "\\1 \\2")
      ("foo" . "bar"))
    )))

(defun electric-formatter-test-execute (string expect &optional pre post)
  (erase-buffer)
  ;; for test depend on syntax table
  ;;(should (equal (electric-formatter string) expect))
  (when pre (insert pre))
  (insert string)
  (when post (save-excursion (insert post)))
  ;;(execute-kbd-macro (kbd "RET"))
  (electric-formatter-electric)
  (should (equal (substring-no-properties (buffer-string))
                 (concat pre expect post)))
  (should (equal (point) (+ (length (concat pre expect)) 1)))
  )

(defmacro electric-formatter-test-region (string expect point)
  (declare (debug t))
  `(progn
     (erase-buffer)
     (insert "\n")
     (insert ,string)
     (save-excursion (insert "\n"))
     (goto-char , point)
     (electric-formatter-region (+ (point-min) 1) (- (point-max) 1))
     (should (equal (substring-no-properties
                     (buffer-substring (+ (point-min) 1) (- (point-max) 1)))
                    ,expect))
     (should (equal (point) , point))))

(ert-deftest electric-formatter-in-default ()
  (ert-with-test-buffer (:name "electric-formatter")
   (electric-formatter-mode 1)
   (should electric-formatter-mode)

   (should (< 3 (length electric-formatter-list)))

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
   (electric-formatter-test-region "1,2" "1, 2" (- (point-max) 1))
   ;;beginnig of region
   (electric-formatter-test-region "1,2" "1, 2" (+ (point-min) 1))
   ;;above region
   (electric-formatter-test-region "1,2" "1, 2" (point-min))
   ;;below region
   (electric-formatter-test-region "1,2" "1, 2" (point-max))
   ;;inside region
   (electric-formatter-test-region "1,2" "1, 2" (+ (point-min) 2))
   (electric-formatter-test-region "\n\n" "\n"  (+ (point-min) 2))
   (electric-formatter-test-region "\n\n\n" "\n"(+ (point-min) 2))
   )
  )

(ert-deftest electric-formatter-in-elisp ()
  (ert-with-test-buffer (:name "electric-formatter")
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
   (electric-formatter-test-execute ")\"" ") \"")
   (electric-formatter-test-execute "a\"" "a \"")

   (electric-formatter-test-execute "a." "a .")
   (electric-formatter-test-execute ".a" ". a")

   (electric-formatter-test-execute "\"a\"a" "\"a\" a")
   (electric-formatter-test-execute "\"a\"a" "\"a\" a" "\n")

   (electric-formatter-test-execute "\"\"a" "\"\" a")
   (electric-formatter-test-execute "\"\"a" "\"\" a" "\n" nil)
   (electric-formatter-test-execute "\"\"a" "\"\" a" nil "\n")
   ;;(electric-formatter-test-execute "\"\"(" "\"\" (")
   (electric-formatter-test-region ") )" "))"(+ (point-min) 2))
   (electric-formatter-test-region ") \n \n )" "))"(+ (point-min) 2))
   ))

(ert-deftest electric-formatter-in-ruby ()
  (ert-with-test-buffer (:name "electric-formatter")
   (ruby-mode)
   (electric-formatter-mode 1)

   (should electric-formatter-mode)
   ;;(should (eq (length electric-formatter-list) 6))
   (electric-formatter-test-execute "a=b" "a = b")
   (electric-formatter-test-execute "a===b" "a === b")
   (electric-formatter-test-execute "a===:b" "a === :b")
   (electric-formatter-test-execute ":a===:b" ":a === :b")

   (electric-formatter-test-execute "a=>b" "a => b")
   (electric-formatter-test-execute "a<=b" "a <= b")
   (electric-formatter-test-execute "a<=>b" "a <=> b")
   (electric-formatter-test-execute "a&&b" "a && b")
   (electric-formatter-test-execute "a||b" "a || b")
   (electric-formatter-test-execute "a and b" "a && b")
   (electric-formatter-test-execute "a or b" "a || b")
   ))

(ert-run-tests-interactively t)
