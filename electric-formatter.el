(require 'cl)

(defvar electric-formatter-list nil)
;;(make-variable-buffer-local 'electric-formatter-list)

(defun electric-formatter-regex (formatter-list symbol)
  (let ((strings
         (delq
          nil
          (mapcar
           '(lambda (elem)
              (when (eq (car elem) symbol)
                (cdr elem)))
           formatter-list))))
    (if strings
        (regexp-opt strings t))))

(defun electric-formatter-regex-opt (formatter-list)
  (delq nil
  (append
   (remove-if-not
    '(lambda (elem)
       (stringp (car elem))
       ) formatter-list)
   (let ((space-after (electric-formatter-regex formatter-list 'space-after))
         (space-before (electric-formatter-regex formatter-list 'space-before)))
     (list
      (if space-after
          (cons
           (concat space-after "\\(\\w\\|\\s.\\)")
           "\\1 \\2"))
      (if space-before
          (cons
           (concat "\\(\\w\\|\\s.\\)" space-before)
           "\\1 \\2"))
     )))))

(defun electric-formatter (string)
  (reduce #'electric-formatter-1
          (cons
           string
           (electric-formatter-regex-opt electric-formatter-list))))

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
             '(space-after . "=")
             )
(add-to-list 'electric-formatter-list
             '(space-after . ",")
             )
(add-to-list 'electric-formatter-list
             '(space-before . "=")
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
            ;; (add-to-list 'electric-formatter-list
            ;;              '("\\(\\w\\)(" . "\\1 ("));;paren
            ;; (add-to-list 'electric-formatter-list
            ;;              '(")\\(\\w\\)" . ") \\1"));;close paren
            ;; (add-to-list 'electric-formatter-list
            ;;              '("\\(\\w\\)_\\(\\w\\)" . "\\1-\\2"));;underscore
            (add-to-list 'electric-formatter-list
                         '(space-after . "=")
                         )
            (add-to-list 'electric-formatter-list
                         '(space-after . ",")
                         )
            (add-to-list 'electric-formatter-list
                         '(space-before . "=")
                         )
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
