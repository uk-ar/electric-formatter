(require 'cl)

(defvar electric-formatter-list nil)
(make-variable-buffer-local 'electric-formatter-list)

(defun electric-formatter (string)
  (cons
   (concat
    (regexp-opt
     (delq
      nil
      (mapcar
       '(lambda (elem)
          (when (eq (car elem) 'after)
            (cdr elem)))
       electric-formatter-list)) t)
    "\\(\\w\\|\\s.\\)")
   "\\1 \\2")
  (cons
   (concat
    "\\(\\w\\|\\s.\\)"
    (regexp-opt
     (delq
      nil
      (mapcar
       '(lambda (elem)
          (when (eq (car elem) 'before)
            (cdr elem)))
       electric-formatter-list)) t))
   "\\1 \\2")
  (let ((after
         )
        (before
         )
        (other
         (remove-if-not
          '(lambda (elem)
             (stringp (car elem))
             ) electric-formatter-list))
        )
    )






  (reduce #'electric-formatter-1
          (cons string electric-formatter-list)))

(defun electric-formatter-1 (string rule)
  (replace-regexp-in-string (car rule) (cdr rule) string))

(defun electric-formatter-electric-1 (start end)
  (let* ((str (buffer-substring-no-properties start end))
         (to-str (electric-formatter str)))
    (unless (equal str to-str)
      (delete-region start end)
      (insert to-str))
    ))

(defun electric-formatter-electric ()
  (electric-formatter-electric-1 (line-beginning-position) (point))
  )

(defun electric-formatter-electric-region (&optional beg end)
  ;;
  (interactive "r")
  (let ((pos (point)))
    (electric-formatter-electric-1 beg end)
    (if (= beg pos)
        (goto-char beg))
    ))

;;'(ba, bc, d, e, g, a, b, e, f, g, f, a == b = d = e = g = d == b =,= a, b, d, e, e,, f== f== def== g = aba, be, f == a, a, e === b = f)
;;TODO:remove
;; (defconst ef/space-after-\, '(",\\(\\w\\|\\s.\\)" . ", \\1"))
;; (defconst ef/space-after-\= '("\\(\\w\\)=" . "\\1 ="))
;; (defconst ef/space-before-\= '("=\\(\\w\\)" . "= \\1"))
(setq electric-formatter-list nil)

(add-to-list 'electric-formatter-list
             '(after . "=")
             )
(add-to-list 'electric-formatter-list
             '(after . ",")
             )
(add-to-list 'electric-formatter-list
             '(before . "=")
             )
;;(mapcar 'car electric-formatter-list)
(remove-if-not
 '(lambda (elem) (eq (car elem) 'after))
 electric-formatter-list)

(add-to-list 'electric-formatter-list
             `(,(concat (regexp-opt '("," "=") t) "\\(\\w\\|\\s.\\)") . "\\1 \\2")
             )
;;"\\(,\\|=\\)\\(\\w\\|\\s.\\)"
;; (add-to-list 'electric-formatter-default-list
;;              '(",\\(\\w\\)" . ", \\1");;space after "," ??",\\(\\w\\|\\s.\\)"
;;              )
;; (add-to-list 'electric-formatter-default-list
;;              '("\\(\\w\\)=" . "\\1 =");;space before "="
;;              )
;; (add-to-list 'electric-formatter-default-list
;;              '("=\\(\\w\\)" . "= \\1");;space after "="
;;              )
;; for ruby's :hoge,:fuga
;; (replace-regexp-in-string ",\\(\\w\\|\\s.\\)" . ", \\1" . ":foo,:bar")

;;(setq-default electric-formatter-list electric-formatter-default-list)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-to-list 'electric-formatter-list
                         '("\\(\\w\\)(" . "\\1 ("));;paren
            (add-to-list 'electric-formatter-list
                         '(")\\(\\w\\)" . ") \\1"));;close paren
            (add-to-list 'electric-formatter-list
                         '("\\(\\w\\)_\\(\\w\\)" . "\\1-\\2"));;underscore
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
