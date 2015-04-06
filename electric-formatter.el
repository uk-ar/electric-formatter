(require 'cl)

(defvar electric-formatter-list nil)
(make-variable-buffer-local 'electric-formatter-list)

(defun electric-formatter-regex (formatter-list symbol)
  (let ((strings
         (mapcar 'cdr
                 (remove-if-not
                  (lambda (elem) (eq (car elem) symbol))
                  formatter-list))))
    (if strings
        (regexp-opt strings t))))

(defun electric-formatter-regex-opt (formatter-list)
  (delq
   nil
   (append
    (let ((space-after
           (electric-formatter-regex formatter-list 'space-after))
          (space-before
           (electric-formatter-regex formatter-list 'space-before)))
      (list
       (if space-after
           (cons
            (concat space-after "\\(\\w\\|\\s.\\)")
            "\\1 \\2"))
       (if space-before
           (cons
            (concat "\\(\\w\\|\\s.\\)" space-before)
            "\\1 \\2"))))
    (remove-if-not
     '(lambda (elem)
        (stringp (car elem)))
     formatter-list))))

(defun electric-formatter (string)
  (reduce #'electric-formatter-1
          (cons
           string
           (electric-formatter-regex-opt electric-formatter-list))))

(defun electric-formatter-1 (string rule)
  (replace-regexp-in-string (car rule) (cdr rule) string))

(defun electric-formatter-electric-1 (start end)
  ;; must go to after inserted region
  ;; save-execursion cause bug
  (let* ((str (buffer-substring-no-properties start end))
         (to-str (electric-formatter str)))
    (unless (equal str to-str)
      (delete-region start end)
      (goto-char (min start end))
      (insert to-str)
      (cons (min start end) (point))
      ;;(save-excursion)
      )))

;; (setq
;;  result
;;  (append
;;   result
;;   (buffer-substring
;;    (- (point)
;;       )
;;    (point))))

(defun electric-formatter-electric ()
  (undo-boundary)
  (goto-char
   (or (cdr (electric-formatter-electric-region
             (line-beginning-position) (point)))
       (point))))

(defun electric-formatter-electric-region (&optional beg end)
  (interactive "r")
  (let ((beg (min beg end))
        (end-marker (set-marker (make-marker) (max beg end)))
        ;;(end (max beg end))
        )
    ;; parse forward because text inserted
    (goto-char (- beg 1));;-1 for forward-char
    (while (< (point) (marker-position end-marker))
      (forward-char 1)
      (cond
       ;; in string
       ((nth 3 (syntax-ppss))
        ;;until string start
        (skip-syntax-forward "^\"" (marker-position end-marker))
        )
       ;; in comment
       ((nth 4 (syntax-ppss))
        ;;until comment start
        (skip-syntax-forward "^>" (marker-position end-marker))
        )
       (t
        ;;until comment end or string end
        (let ((pos (point)))
          (skip-syntax-forward "^\"<" (marker-position end-marker))
          (electric-formatter-electric-1
           (- pos 1)
           (point)
           ;(if (eobp) (point) (+ (point) 1))
           ;; (min (+ (point) 1)
           ;;      ;;-1 for not to delete marker
           ;;      ;;(- (marker-position end-marker) 1)
           ;;      (marker-position end-marker)
           ;;      )
           )
          ;; (unless (< (point) (marker-position end-marker))
          ;;   (goto-char (marker-position end-marker)))
          ))))
    (prog1
        (cons beg
              ;;(marker-position end-marker)
              (point)
              )
      (set-marker end-marker nil))
    ))
;;aa, a bb "ee""c,c" ;;dd

;;'(ba, bc, d, e, g, a, b, e, f, g, f, a == b = d = e = g = d == b =,= a, b, d, e, e,, f== f== def== g = aba, be, f == a, a, e === b = f)
;;TODO:remove
(setq electric-formatter-list nil)

;;nconc?
(setq-default electric-formatter-list
              '((space-after . "=")
                (space-after . ",")
                (space-before . "=")))

;;(assoc-default 'space-after electric-formatter-list)

;; for ruby's :hoge,:fuga
;; (replace-regexp-in-string ",\\(\\w\\|\\s.\\)" . ", \\1" . ":foo,:bar")

;;(setq-default electric-formatter-list electric-formatter-default-list)

(define-minor-mode electric-formatter-mode
  "Toggle electric formatter."
  :global t
  :lighter " EF"
  :group 'electric-formatter
  (if electric-formatter-mode
      (progn
        (add-hook 'post-self-insert-hook #'electric-formatter-electric)
        )
    (remove-hook 'post-self-insert-hook #'electric-formatter-electric)))
;;post-self-insert-hook

;;; Setting
(defun electric-formatter-emacs-lisp-mode-setup()
  (setq electric-formatter-list
        ;;(append electric-formatter-list
        '((space-after . "=")
          (space-after . ",")
          (space-after . ")")
          (space-before . "=")
          (space-before . "(")
          )))

(add-hook 'emacs-lisp-mode-hook
          'electric-formatter-emacs-lisp-mode-setup)

(add-hook 'lisp-interaction-mode-hook
          'electric-formatter-emacs-lisp-mode-setup)

(provide 'electric-formatter)
