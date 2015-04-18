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
(require 'electric-formatter-config)
(require 'ert)
(require 'ert-x)
;;; Code:
;; (ert-deftest ef-space-after ()
;;   (should
;;    (equal
;;     (ef-rule-space-after "&" "=")
;;     (cons (concat "\\([&=]\\)" ef-beginning-regexp) "\\1 \\2")
;;     )))

;; (ert-deftest ef-space-before ()
;;   (should
;;    (equal
;;     (ef-rule-space-before "&" "=")
;;     (cons (concat ef-end-regexp "\\([&=]\\)") "\\1 \\2")
;;     )))

(defun ef-test-rule (str rule)
  (ef-test-rules str (list rule)))

(defun ef-test-rules (str rules)
  (erase-buffer)
  (save-excursion (insert str))
  (electric-formatter-region-1
   (point-min) (point-max)
   (lambda (rule)
     (replace-match (cdr rule)))
   (ef-convert-rules rules))
  (substring-no-properties (buffer-string)))

(ert-deftest ef-comment ()
  (should
   (equal
    (ef-test-rule "//a" (ef-rule-space-after "//" "/*"))
    "// a"))
  (should
   (equal
    (ef-test-rule "/*a*/" (ef-rule-space-after "//" "/*"))
    "/* a*/")))

(ert-deftest ef-comma ()
  (should
   (equal
    (ef-test-rule ",hoge" '(",\\(\\w\\)" . ", \\1"))
    ", hoge")))

(ert-deftest ef-paren ()
  (should
   (equal
    (ef-test-rule "hoge(" '("\\(\\w\\)(" . "\\1 ("))
    "hoge (")))

(ert-deftest ef-close-paren ()
  (should
   (equal
    (ef-test-rule ")hoge" '(")\\(\\w\\)" . ") \\1"))
    ") hoge")))

(ert-deftest ef-before-= ()
  (should
   (equal
    (ef-test-rule "a=" '("\\(\\w\\|\\s.\\)=" . "\\1 ="))
    "a =")))

(ert-deftest ef-after-= ()
  (should
   (equal
    (ef-test-rule "=a" '("=\\(\\w\\)" . "= \\1"))
    "= a")))

(ert-deftest ef-blank-lines ()
  (let ((rule '("\n[\n]+" . "\n\n")))
    (should
     (equal
      (ef-test-rule "a\n\na" rule)
      "a\n\na"))
    (should
     (equal
      (ef-test-rule "a\n\n\na" rule)
      "a\n\na"))
    (should
     (equal
      (ef-test-rule "a\n\n\n\na" rule)
      "a\n\na"))))

(ert-deftest ef-whitespace ()
  (should
   (equal
    (ef-test-rule ") \n )" '(")[\n\t ]+)" . "))"))
    "))")))

(ert-deftest ef-around ()
  (ert-with-test-buffer (:name "electric-formatter")
    (c-mode)
    (should
     (equal
      (ef-test-rules "a=" (list (ef-rule-space-around "=")))
      "a="))
    (should
     (equal
      (ef-test-rules "a=b" (list (ef-rule-space-around "=")))
      "a = b"))
    (should
     (equal
      (ef-test-rules "a  =  b" (list (ef-rule-space-around "=")))
      "a  =  b"))
    (should
     (equal
      (ef-test-rules "a=b=c" (list (ef-rule-space-around "=")))
      "a = b = c"))))

(ert-deftest ef-multibyte ()
  (ert-with-test-buffer (:name "electric-formatter")
    (org-mode)
    (should
     (equal
      (ef-test-rules "あaあ" '(("\\([[:multibyte:]]\\)\\([[:unibyte:]]\\)"
                                . "\\1 \\2")))
      "あ aあ"))
    (should
     (equal
      (ef-test-rules "あaあ" '(("\\([[:unibyte:]]\\)\\([[:multibyte:]]\\)"
                                . "\\1 \\2")))
      "あa あ"))
    (should
     (equal
      (ef-test-rules "あaあ" '(("\\([[:multibyte:]]\\)\\([[:unibyte:]]\\)"
                                . "\\1 \\2")
                               ("\\([[:unibyte:]]\\)\\([[:multibyte:]]\\)"
                                . "\\1 \\2")))
      "あ a あ"))
    ))

(ert-deftest ef--> ()
  (ert-with-test-buffer (:name "electric-formatter")
  (should
   (equal
    (ef-test-rules "a-> b"
                   (list (ef-rule-delete-space "->" ef-beginning-regexp)))
    "a->b"))))

(ert-deftest ef-inside-paren ()
  (should
   (equal
    (ef-test-rule "a(bar = 'bar',baz = []):"
                 '("\\([(,][^(,]+\\)[\t ]+\\(=\\)" . "\\1\\2"))
    "a(bar= 'bar',baz= []):"))
  (should
   (equal
    (ef-test-rule "a(bar = 'bar',baz = []):"
                 '("\\([(,][^(,]+\\)\\(=\\)[\t ]+" . "\\1\\2"))
    "a(bar ='bar',baz =[]):"))
  (should
   (equal
    (ef-test-rules "def foo(bar = 'bar',baz = [],qux = 1):"
               (list
                (cons "\\([(,][^(,]+\\)\\(=\\)[\t ]+" "\\1\\2")
                (cons "\\([(,][^(,]+\\)[\t ]+\\(=\\)" "\\1\\2")))
    "def foo(bar='bar',baz=[],qux=1):"))
  (should
   (equal
    (ef-test-rules ",baz = [],qux = 1):"
               (list
                (cons "\\([(,][^(,]+\\)\\(=\\)[\t ]+" "\\1\\2")
                (cons "\\([(,][^(,]+\\)[\t ]+\\(=\\)" "\\1\\2")))
    ",baz=[],qux=1):"))
  )

;;bug in inside string

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

(ert-deftest ef-test-convert-rules ()
  (should
   (equal (ef-convert-rules '((ef-rule-space-after  ")" "\"" ".")))
          (list (ef-rule-space-after  ")" "\"" ".")))))

(defun ef-test-execute (string &optional expect pre post)
  (let ((expect (or expect string)))
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
    ;; test twice
    (electric-formatter-electric)
    (should (equal (substring-no-properties (buffer-string))
                   (concat pre expect post)))
    (should (equal (point) (+ (length (concat pre expect)) 1)))
    ))

(defmacro ef-test-region (string expect &optional point)
  (declare (debug t))
  `(let ((point (or ,point (point-min))))
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

(ert-deftest ef-mode ()
  (ert-with-test-buffer (:name "electric-formatter")
    ;;default is on
    (should electric-formatter-mode)
    (should (memq 'electric-formatter-electric post-self-insert-hook))
    ;;turn off
    (electric-formatter-mode -1)
    (should-not electric-formatter-mode)
    (should-not (memq 'electric-formatter-electric post-self-insert-hook))
    ;;turn on
    (electric-formatter-mode 1)
    (should electric-formatter-mode)
    (should (memq 'electric-formatter-electric post-self-insert-hook))

    (should (< 2 (length ef-rule-list)))
    ))

(ert-deftest ef-in-default ()
  (ert-with-test-buffer (:name "electric-formatter")
   (electric-formatter-mode 1)
   (should electric-formatter-mode)

   (should (< 2 (length ef-rule-list)))

   (ef-test-execute ",hoge" ", hoge")
   (ef-test-execute ",  hoge")
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
   (ef-test-region "a\n\na"   "a\n\na"  (+ (point-min) 2))
   (ef-test-region "a\n\n\na" "a\n\na"(+ (point-min) 2))
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
   (ef-test-execute "),a" ") ,a")

   (ef-test-execute ")\"" ") \"") ;;in string
   (ef-test-execute "a\"" "a \"") ;; in string
   (ef-test-execute "\"a" "\"a") ;; in string

   (ef-test-execute "a." "a .")
   (ef-test-execute ".a" ". a")

   (ef-test-execute "\"a\"a" "\"a\" a") ;; in string
   (ef-test-execute "\"a\"a" "\"a\" a" "\n") ;; in string

   (ef-test-execute "\"\"a" "\"\" a")
   (ef-test-execute "\"\"a" "\"\" a" "\n" nil)
   (ef-test-execute "\"\"a" "\"\" a" nil "\n")
   (ef-test-execute "\"\"(" "\"\" (")
   (ef-test-execute ";a" "; a")
   (ef-test-execute ";;a" ";; a")
   (ef-test-execute ";a" "; a" nil "\n")
   (ef-test-execute ";a" "; a" "\n" nil)

   (ef-test-region ") )" "))"(+ (point-min) 2))
   (ef-test-region ") \n \n )" "))"(+ (point-min) 2))
   ))

;;http://docs.ruby-lang.org/ja/1.9.3/doc/spec=2foperator.html
(ert-deftest ef-in-ruby ()
  (ert-with-test-buffer (:name "electric-formatter")
   (ruby-mode)
   (electric-formatter-mode 1)

   (should electric-formatter-mode)
   ;;(should (eq (length ef-rule-list) 6))
   (ef-test-execute "a=b" "a = b")
   (ef-test-execute "a[0]=b" "a[0] = b")
   (ef-test-execute "a.b=c" "a.b = c")

   (ef-test-execute "a+=b" "a += b")
   (ef-test-execute "a-=b" "a -= b")
   (ef-test-execute "a*=b" "a *= b")
   (ef-test-execute "a/=b" "a /= b")
   (ef-test-execute "a%=b" "a %= b")
   (ef-test-execute "a**=b" "a **= b")
   (ef-test-execute "a&=b" "a &= b")
   (ef-test-execute "a|=b" "a |= b")
   (ef-test-execute "a^=b" "a ^= b")
   (ef-test-execute "a<<=b" "a <<= b")
   (ef-test-execute "a>>=b" "a >>= b")
   ;;(ef-test-execute "a&&=b" "a &&= b")
   (ef-test-execute "a||=b" "a ||= b")

   (ef-test-execute "foo,bar,baz=1,2,3" "foo, bar, baz = 1, 2, 3")
   ;; (ef-test-execute "foo,=list()" "foo, = list()");; ,as single operator
   ;; (ef-test-execute "foo,*rest=list2()" "foo, *rest = list2()")
   (ef-test-execute "(foo,bar),baz=[1,2],3" "(foo, bar), baz = [1, 2], 3")
   (ef-test-execute "1..20" "1 .. 20")
   ;; (ef-test-execute "/first/.../second/" "/first/ ... /second/") / as word separator
   (ef-test-execute "!me" "! me")
   (ef-test-execute "i!=you" "i != you")
   ;;http://docs.ruby-lang.org/ja/2.0.0/doc/symref.html
   ;; ?
   (ef-test-execute "def xxx!" "def xxx!")
   (ef-test-execute "?a")
   (ef-test-execute "def xxx?")
   (ef-test-execute "/xxx?/")
   ;; %
   (ef-test-execute "10%3" "10 % 3")
   ;;(ef-test-execute "\"04b\"%3" "\"04b\" % 3")
   (ef-test-execute "%r{/etc/httpd/logs$}")
   (ef-test-execute "%w[foo bar baz]")
   (ef-test-execute "%!nomad!")
   ;; &
   (ef-test-execute "10&3" "10 & 3")
   (ef-test-execute "a&=yyy" "a &= yyy")
   (ef-test-execute "xxx&&yyy" "xxx && yyy")
   (ef-test-execute "def xxx(&yyy)")
   (ef-test-execute "xxx(&b)")
   ;; |
   (ef-test-execute "10|3" "10 | 3")
   (ef-test-execute "a||=yyy" "a ||= yyy")
   (ef-test-execute "xxx||yyy" "xxx || yyy")
   ;;(ef-test-execute "5.times{|n| p n}")
   (ef-test-execute "/xx(xx|xx)/")
   ;; +
   (ef-test-execute "10+3" "10 + 3")
   (ef-test-execute "+3" "+3")
   (ef-test-execute "/xxx+/")
   ;; -
   (ef-test-execute "10-3" "10 - 3")
   (ef-test-execute "3*(-5)" "3 * (-5)")
   ;; *
   (ef-test-execute "2*3" "2 * 3")
   ;;(ef-test-execute "2**3" "2**3")
   (ef-test-execute "def xxx(*yy)")
   ;;(ef-test-execute "x,*y=foo()" "x, *y = foo()")
   (ef-test-execute "/xx*/")
   ;; /
   (ef-test-execute "10/3" "10 / 3")
   (ef-test-execute "/xxx/")
   (ef-test-execute "'23'.split(//)")
   ;; ^
   (ef-test-execute "true^true" "true ^ true")
   (ef-test-execute "a^=true" "a ^= true")
   (ef-test-execute "/^xxx/")
   ;; :
   (ef-test-execute "a=:b" "a = :b")
   ;; (ef-test-execute "A::B" "A::B")
   ;; (ef-test-execute "::B" "::B")
   ;; (ef-test-execute "foo::(bar)" "foo::(bar)")
   (ef-test-execute "a?b:c" "a ? b : c")
   ;;(ef-test-execute "{key:value}" "{key: value}");;space after?
   (ef-test-execute "{:a=>'aaa',:b=>'bbb'}" "{:a => 'aaa', :b => 'bbb'}")
   ;;(ef-test-execute "{a:'aaa',b:'bbb'}" "{a:'aaa', b:'bbb'}")
   ;; .
   (ef-test-execute "xxx.yyy")
   (ef-test-execute "1..20" "1 .. 20")
   (ef-test-execute "1...20" "1 ... 20")
   ;;(ef-test-execute "if/^begin/../^end/" "if /^begin/ .. /^end/")
   (ef-test-execute "/xx.xx/")
   ;; ,
   (ef-test-execute "a,b=[1,2,3]" "a, b = [1, 2, 3]")
   (ef-test-execute "a=b,c" "a = b, c")
   (ef-test-execute "def foo(bar,baz)" "def foo(bar, baz)")
   (ef-test-execute "[:a,:b,c:]" "[:a, :b, c:]")
   (ef-test-execute "{:a=>1,:b=>2}.each{|key,val|}"
                    "{:a => 1, :b => 2}.each{|key, val|}")

   (ef-test-execute "+a")
   (ef-test-execute "-a")
   (ef-test-execute "a+b" "a + b")
   (ef-test-execute "a-b" "a - b")
   ;;(ef-test-execute "a*b"  "a * b")
   (ef-test-execute "a/b"  "a / b")
   (ef-test-execute "a%b"  "a % b")
   ;;(ef-test-execute "a**b" "a ** b")
   (ef-test-execute "a<<b" "a << b")

   (ef-test-execute "reg=~str" "reg =~ str")
   (ef-test-execute "reg!~str" "reg !~ str")

   (ef-test-execute "a===b" "a === b")
   (ef-test-execute "a=b;c=d" "a = b;c = d")

   (ef-test-execute "a=>b" "a => b")
   (ef-test-execute "a<=b" "a <= b")
   (ef-test-execute "a<=>b" "a <=> b")
   (ef-test-execute "a&&b" "a && b")
   (ef-test-execute "a||b" "a || b")
   (ef-test-execute "a and b" "a && b")
   (ef-test-execute "a or b" "a || b")
   (ef-test-execute "#a" "# a")
   (ef-test-execute "##a" "## a")
   ))

(ert-deftest ef-in-python ()
  (ert-with-test-buffer (:name "electric-formatter")
    (python-mode)
    (electric-formatter-mode 1)

    (should electric-formatter-mode)
    ;;(should (eq (length ef-rule-list) 6))
    (ef-test-execute "a=b" "a = b")
    (ef-test-execute "a=b=c" "a = b = c")
    (ef-test-execute "def foo(bar = 'bar', baz = [], qux = 1):"
                     "def foo(bar='bar', baz=[], qux=1):")
    ))

(ert-deftest ef-in-c ()
  (ert-with-test-buffer (:name "electric-formatter")
    (c-mode)
    (should electric-formatter-mode)
    ;;(should (eq (length ef-rule-list) 6))
    ;;must use parse-partial-sexp
    (ef-test-execute "//a" "// a")
    (ef-test-execute "/*a*/" "/* a */")

    ;; ref http://www.c-lang.org/operator.html
    ;; 1st
    (ef-test-execute "a[b]" "a[b]")
    (ef-test-execute "a(b)" "a(b)")
    (ef-test-execute "a.b" "a.b")
    (ef-test-execute "a->b" "a->b")
    (ef-test-execute "a++" "a++")
    (ef-test-execute "a--" "a--")
    ;; 2nd
    (ef-test-execute "++a" "++a")
    (ef-test-execute "--a" "--a")
    (ef-test-execute "b=&a" "b = &a")
    (ef-test-execute "foo(&a)" "foo(&a)")
    (ef-test-execute "int *a" "int *a")
    (ef-test-execute "*a" "*a")
    (ef-test-execute "int **a" "int **a")
    (ef-test-execute "+a" "+a")
    (ef-test-execute "-a" "-a")
    (ef-test-execute "~a" "~a")
    (ef-test-execute "!a" "!a")
    ;; 3rd
    (ef-test-execute "(a)b" "(a)b")
    ;; 4th
    ;; (ef-test-execute "b*a" "b * a")
    ;; (ef-test-execute "b/a" "b / a")
    ;; (ef-test-execute "b/*a" "b/* a");;comment
    (ef-test-execute "b%a" "b % a")
    ;; 5th
    (ef-test-execute "b+a" "b + a")
    (ef-test-execute "b-a" "b - a")
    ;; 6th
    (ef-test-execute "b<<a" "b << a")
    (ef-test-execute "b>>a" "b >> a")
    ;; 7th
    (ef-test-execute "a<b" "a < b")
    (ef-test-execute "a<=b" "a <= b")
    (ef-test-execute "a>b" "a > b")
    (ef-test-execute "a>=b" "a >= b")
    ;; 8th
    (ef-test-execute "a==b" "a == b")
    (ef-test-execute "a!=b" "a != b")
    ;; 9th
    (ef-test-execute "b&a" "b & a")
    (ef-test-execute "b()&a" "b() & a")
    ;; 10th
    (ef-test-execute "b^a" "b ^ a")
    ;; 11th
    (ef-test-execute "b|a" "b | a")
    ;; 12th
    (ef-test-execute "b&&a" "b && a")
    ;; 13th
    (ef-test-execute "b||a" "b || a")
    ;; 14th
    (ef-test-execute "a?b:c" "a ? b : c")
    ;; 15h
    (ef-test-execute "b=a" "b = a")
    (ef-test-execute "b+=a" "b += a")
    (ef-test-execute "b-=a" "b -= a")
    (ef-test-execute "b*=a" "b *= a")
    (ef-test-execute "b/=a" "b /= a")
    (ef-test-execute "b%=a" "b %= a")
    (ef-test-execute "b<<=a" "b <<= a")
    (ef-test-execute "b>>=a" "b >>= a")
    (ef-test-execute "b&=a" "b &= a")
    (ef-test-execute "b^=a" "b ^= a")
    (ef-test-execute "b|=a" "b |= a")
    (ef-test-execute "b,a" "b, a")

    (ef-test-execute "#include < foo.h >" "#include <foo.h>")
    (ef-test-execute "vector < int > v1;deque < int > v2;"
                     "vector <int> v1;deque <int> v2;")
    (ef-test-region "int  a;\nchar b;" "int  a;\nchar b;" (+ (point-min) 2))
    ))

(ert-deftest ef-in-org ()
  (ert-with-test-buffer (:name "electric-formatter")
    (org-mode)
    (electric-formatter-mode 1)

    (should electric-formatter-mode)
    ;; TODO:
    (ef-test-execute "あaあ" "あ a あ")
    (ef-test-execute "あ a あ" "あ a あ")
    (ef-test-execute "あ  a  あ" "あ  a  あ")
    (ef-test-execute "aあa" "a あ a")
    ))

(mapc
 'kill-buffer
 (remove-if-not
  (lambda (buffer-name)
    (when (string-match "*Test buffer" buffer-name) buffer-name))
  (mapcar 'buffer-name (buffer-list))))
(ert-run-tests-interactively t)
