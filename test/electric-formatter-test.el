;;; electric-formatter-test.el --- test for electric-formatter

;;-------------------------------------------------------------------
;;
;; Copyright (C) 2015 Yuuki Arisawa
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;-------------------------------------------------------------------

;;; Commentary:
(require 'electric-formatter)
(require 'ert)
(require 'ert-x)
;;; Code:
(ert-deftest ef-comma ()
  (should
   (equal
    (ef-format-1 ",hoge" '(",\\(\\w\\)" . ", \\1"))
    ", hoge")))

(ert-deftest ef-paren ()
  (should
   (equal
    (ef-format-1 "hoge(" '("\\(\\w\\)(" . "\\1 ("))
    "hoge (")))

(ert-deftest ef-close-paren ()
  (should
   (equal
    (ef-format-1 ")hoge" '(")\\(\\w\\)" . ") \\1"))
    ") hoge")))

(ert-deftest ef-before-= ()
  (should
   (equal
    (ef-format-1 "a=" '("\\(\\w\\|\\s.\\)=" . "\\1 ="))
    "a =")))

(ert-deftest ef-after-= ()
  (should
   (equal
    (ef-format-1 "=a" '("=\\(\\w\\)" . "= \\1"))
    "= a")))

(ert-deftest ef-blank-lines ()
  (let ((rule '("\n[\n]+" . "\n\n")))
    (should
     (equal
      (ef-format-1 "a\n\na" rule)
      "a\n\na"))
    (should
     (equal
      (ef-format-1 "a\n\n\na" rule)
      "a\n\na"))
    (should
     (equal
      (ef-format-1 "a\n\n\n\na" rule)
      "a\n\na"))))

(ert-deftest ef-whitespace ()
  (should
   (equal
    (ef-format-1 ") \n )" '(")[\n\t ]+)" . "))"))
    "))")))

(ert-deftest ef-multibyte ()
  (should
   (equal
    (ef-format-1 "あa" '("\\([[:multibyte:]]\\)\\([[:unibyte:]]\\)"
                                  . "\\1 \\2"))
    "あ a")))

(ert-deftest ef-regexp-space-after ()
  (should
   (equal
    (ef-regexp
     '((space-after . "=")
       (space-after . ",")
       (space-before . "=")
       ("foo" . "bar"))
     'space-after)
    "\\([,=]\\)"
    )))

(ert-deftest ef-regexp-space-after-not-exist ()
  (should
   (equal
    (ef-regexp
     '((space-before . "=")
       ("foo" . "bar"))
     'space-after)
    nil
    )))

(ert-deftest ef-regexp-space-before ()
  (should
   (equal
    (ef-regexp
     '((space-after . "=")
       (space-after . ",")
       (space-before . "=")
       ("foo" . "bar"))
     'space-before)
    "\\(=\\)"
    )))

(ert-deftest ef-regexp-opt ()
  (should
   (equal
    (ef-regexp-opt
     '(("foo" . "bar")
       (space-after . "=")
       (space-after . ",")
       (space-before . "=")
       ))
    `((,(concat "\\([,=]\\)" ef-beginning-regexp) . "\\1 \\2")
      (,(concat ef-end-regexp "\\(=\\)") . "\\1 \\2")
      ("foo" . "bar"))
    )))

(ert-deftest ef-regexp-opt-after-not-exist ()
  (should
   (equal
    (ef-regexp-opt
     '(("foo" . "bar")
       (space-before . "=")))
    `((,(concat ef-end-regexp "\\(=\\)") . "\\1 \\2")
      ("foo" . "bar"))
    )))

(defun ef-test-execute (string expect &optional pre post)
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

(defmacro ef-test-region (string expect point)
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

(ert-deftest ef-in-default ()
  (ert-with-test-buffer (:name "electric-formatter")
   (electric-formatter-mode 1)
   (should electric-formatter-mode)

   (should (< 2 (length ef-rule-list)))

   (ef-test-execute ",hoge" ", hoge")
   (ef-test-execute ")hoge" ")hoge")

   (ef-test-execute ",hoge" ", hoge" nil "\n,hoge")
   (ef-test-execute ",hoge" ", hoge" ",hoge\n" nil)

   (ef-test-execute ",hoge" ", hoge" nil "\n\",hoge\"")
   (ef-test-execute ",hoge" ", hoge" nil "\",hoge\"")
   (ef-test-execute ",hoge" ", hoge" "\",hoge\"\n" nil)
   (ef-test-execute ",hoge" ", hoge" "\",hoge\"" nil)
   (ef-test-execute ",hoge" ", hoge"
                                    "\",hoge\"\n" "\",hoge\"\n")

   (ef-test-execute ",hoge" ", hoge" nil "\n;,hoge")
   (ef-test-execute ",hoge" ", hoge" nil ";,hoge")
   (ef-test-execute ",hoge" ", hoge" ";,hoge\n" nil)
   (ef-test-execute ",hoge" ", hoge" ";,hoge\n" ";,hoge")
   ;;(ef-test-execute ",hoge" ", hoge" ";,hoge" nil)

   ;;end of region
   (ef-test-region "1,2" "1, 2" (- (point-max) 1))
   ;;beginnig of region
   (ef-test-region "1,2" "1, 2" (+ (point-min) 1))
   ;;above region
   (ef-test-region "1,2" "1, 2" (point-min))
   ;;below region
   (ef-test-region "1,2" "1, 2" (point-max))
   ;;inside region
   (ef-test-region "1,2" "1, 2" (+ (point-min) 2))
   (ef-test-region "\n\n" "\n"  (+ (point-min) 2))
   (ef-test-region "\n\n\n" "\n"(+ (point-min) 2))
   ))

(ert-deftest ef-in-elisp ()
  (ert-with-test-buffer (:name "electric-formatter")
   (emacs-lisp-mode)
   (electric-formatter-mode 1)

   (should electric-formatter-mode)
   ;;(should (eq (length ef-rule-list) 6))
   (ef-test-execute "a" "a")
   ;;(electric-pair-mode 1)
   (ef-test-execute ")hoge" ") hoge")
   (ef-test-execute "hoge(" "hoge (")

   (ef-test-execute ")(" ") (")
   (ef-test-execute ")`(" ") `(")

   (ef-test-execute "),(" ") ,(")
   (ef-test-execute ")\"" ") \"")
   (ef-test-execute "a\"" "a \"")

   (ef-test-execute "a." "a .")
   (ef-test-execute ".a" ". a")

   (ef-test-execute "\"a\"a" "\"a\" a")
   (ef-test-execute "\"a\"a" "\"a\" a" "\n")

   (ef-test-execute "\"\"a" "\"\" a")
   (ef-test-execute "\"\"a" "\"\" a" "\n" nil)
   (ef-test-execute "\"\"a" "\"\" a" nil "\n")
   ;;(ef-test-execute "\"\"(" "\"\" (")
   (ef-test-region ") )" "))"(+ (point-min) 2))
   (ef-test-region ") \n \n )" "))"(+ (point-min) 2))
   ))

(ert-deftest ef-in-ruby ()
  (ert-with-test-buffer (:name "electric-formatter")
   (ruby-mode)
   (electric-formatter-mode 1)

   (should electric-formatter-mode)
   ;;(should (eq (length ef-rule-list) 6))
   (ef-test-execute "a=b" "a = b")
   (ef-test-execute "a===b" "a === b")
   (ef-test-execute "a===:b" "a === :b")
   (ef-test-execute ":a===:b" ":a === :b")

   (ef-test-execute "a=>b" "a => b")
   (ef-test-execute "a<=b" "a <= b")
   (ef-test-execute "a<=>b" "a <=> b")
   (ef-test-execute "a&&b" "a && b")
   (ef-test-execute "a||b" "a || b")
   (ef-test-execute "a and b" "a && b")
   (ef-test-execute "a or b" "a || b")
   ))

(ert-deftest ef-in-org ()
  (ert-with-test-buffer (:name "electric-formatter")
    (org-mode)
    (electric-formatter-mode 1)

    (should electric-formatter-mode)

    (ef-test-execute "あaあ" "あ a あ")
    (ef-test-execute "aあa" "a あ a")
    ))

(ert-run-tests-interactively t)
