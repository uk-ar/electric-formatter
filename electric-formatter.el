;;; electric-formatter.el --- Realtime code formatter which independent of programming language. -*- lexical-binding: t -*-

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

;; Author: Yuuki Arisawa <yuuki.ari@gmail.com>
;; URL: https://github.com/uk-ar/electric-formatter
;; Package-Requires: ((cl-lib "0.5"))
;; Created: 1 April 2015
;; Version: 1.0
;; Keywords: region formatter
;;; Compatibility: GNU Emacs 24.4

;;; Commentary:

(require 'cl-lib)
;;; Code:
(defvar ef-rule-list nil)
(make-variable-buffer-local 'ef-rule-list)

(defvar ef-comment-rule-list nil)
(make-variable-buffer-local 'ef-comment-rule-list)

(defun ef-regexp (formatter-list symbol)
  (let ((strings
         (mapcar 'cdr
                 (cl-remove-if-not
                  (lambda (elem) (eq (car elem) symbol))
                  formatter-list))))
    (if strings
        (regexp-opt strings t))))

(defun ef-regexp-opt (formatter-list)
  (delq
   nil
   (append
    (let ((space-after
           (ef-regexp formatter-list 'space-after))
          (space-before
           (ef-regexp formatter-list 'space-before)))
      (list
       (if space-after
           (cons
            (concat space-after ef-beginning-regexp)
            "\\1 \\2"))
       (if space-before
           (cons
            (concat ef-end-regexp space-before)
            "\\1 \\2"))))
    (cl-remove-if-not
     '(lambda (elem)
        (stringp (car elem)))
     formatter-list))))

(defun ef-format (string rule-list)
  (cl-reduce #'ef-format-1
          (cons
           string
           (ef-regexp-opt rule-list))))

(defun ef-format-1 (string rule)
  (replace-regexp-in-string (car rule) (cdr rule) string))

;;hooked
(defun electric-formatter-electric ()
  (undo-boundary)
  (electric-formatter-region
             (line-beginning-position) (point)))

(defun ef-range (start end rule-list)
  ;; must go to after inserted region
  ;; save-execursion cause bug
  (let* ((str (buffer-substring-no-properties start end))
         (to-str (ef-format str rule-list)))
    (unless (equal str to-str)
      (delete-region start end)
      (goto-char (min start end))
      (insert to-str)
      (cons (min start end) (point))
      ;;(save-excursion)
      )))

;;(electric-formatter-region-1 '("\\w," . "\\1 ,"))
;;http://kouzuka.blogspot.jp/2011/03/replace-regexp-replace-multi-pairs.html?m=1
(defun electric-formatter-region-func (rule)
  (let ((ppss
         ;;match-end
         (save-match-data
           (save-excursion
             (syntax-ppss (match-end 1)
                          ;; (if (and (match-end 1)
                          ;;          (match-string 2)
                          ;;          (equal "" (match-string 2)))
                          ;;     (+ 1 (match-end 1))
                          ;;   )
                          ;; (+
                          ;; 1
                          ;; ;; (if (and (match-string 2)
                          ;; ;;          ) 1 0)
                          ;; )
                          )))))
    (cond
     ;; in string
     ((and (nth 3 ppss)));;nop
     ;; in comment
     ((nth 4 ppss)
      (when (memql rule ef-comment-rule-list)
        (replace-match (cdr rule))))
     (t
      (when (memql rule ef-rule-list)
        (replace-match (cdr rule)))))))

(defun electric-formatter-region-1 (beg end func rules)
  ;; point must be region beginning
  (let ((end-marker (set-marker (make-marker) end)))
    (mapc
     (lambda (rule)
       (save-excursion
         (goto-char beg)
         (while (and (< (point) (marker-position end-marker))
                     (progn
                       ;;(unless (eq beg (point)) (forward-char -1))
                       (re-search-forward (car rule)
                                          (marker-position end-marker) t)))
           (funcall func rule)
           )))
     rules)
    (set-marker end-marker nil)
    ))

;;;###autoload
(defun electric-formatter-region (&optional beg end)
  (interactive "r")
  (let ((pos (point))
        (eobp (eobp))
        (pos-marker (set-marker (make-marker) (point)))
        (beg (min beg end))
        (end (max beg end)))
    ;;end is moving
    (electric-formatter-region-1
     beg end
     #'electric-formatter-region-func
     (append ef-rule-list ef-comment-rule-list))
    ;;(electric-formatter-electric-1 beg end)
    (cond
     ((= beg pos)
      (goto-char beg))
     ((= end pos)) ;;nop
     ((and (< beg pos) (< pos end))
      (goto-char pos))
     (eobp
      (goto-char (point-max)))
     ;; (t
     ;;  (goto-char (marker-position pos-marker)))
     )
    ;; (set-marker pos-marker nil)
    ))

;;;###autoload
(defun electric-formatter-buffer ()
  (interactive)
  (electric-formatter-region (point-min) (point-max))
  )

;;(assoc-default 'space-after ef-rule-list)

;;;###autoload
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

(defcustom electric-formatter-disable-modes nil
  "Major modes `electric-formatter-mode' can not run on."
  :group 'electric-formatter)

;; copy from auto-complete-mode-maybe
(defun electric-formatter-mode-maybe ()
  "What buffer `electric-formatter-mode' prefers."
  (when (and (not (minibufferp (current-buffer)))
             (not (memq major-mode electric-formatter-disable-modes))
             (electric-formatter-mode 1)
             )))

;; copy from global-auto-complete-mode
;;;###autoload
(define-global-minor-mode global-electric-formatter-mode
  electric-formatter-mode electric-formatter-mode-maybe
  ;; :init-value t has bug?
  :group 'electric-formatter)

(provide 'electric-formatter)
;;; electric-formatter.el ends here
