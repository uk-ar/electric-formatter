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
;; Package-Requires: ((cl-lib "0.5") (anaphora "1.0.0"))
;; Created: 1 April 2015
;; Version: 1.0
;; Keywords: region formatter
;;; Compatibility: GNU Emacs 24.4

;;; Commentary:

(require 'cl-lib)
;;(require 'anaphora)
;;; Code:
;; TODO: rename to ef-rules
(defvar ef-rule-list nil)
(make-variable-buffer-local 'ef-rule-list)

;; TODO: rename to ef-comment-rules
(defvar ef-comment-rule-list nil)
(make-variable-buffer-local 'ef-comment-rule-list)

;;hooked
(defun electric-formatter-current ()
  (undo-boundary)
  ;; TODO: from end to beginning
  (electric-formatter-region
             (line-beginning-position) (point)))

(defalias 'electric-formatter-post-self-insert-function
  'electric-formatter-current)

(defun ef-convert-rule (rule)
  (if (symbolp (car rule))
      (apply (car rule) (cdr rule))
      ;;(eval rule)
    rule));; regexp

(defun ef-convert-rules (rules)
  (mapcar 'ef-convert-rule rules))

;;http://kouzuka.blogspot.jp/2011/03/replace-regexp-replace-multi-pairs.html?m=1
(defun electric-formatter-region-func (rule rules comment-rules)
  (let* ((end (or (match-end 1)
                  (match-end 0)))
         ;; marker didnot work...
         ;;(end-marker (set-marker (make-marker) (+ 1 end)))
         (end-ppss
          (save-match-data
            (save-excursion
              (syntax-ppss end))))
         (beg-ppss
          (save-match-data
            (save-excursion
              (syntax-ppss (or (match-beginning 1)
                               (match-beginning 0)))))))
    (cond
     ;; in string
     ;; TODO: beg-ppss
     ((nth 3 end-ppss)
      ;; this is for overlapped regexp match
      (goto-char (+ 1 (match-beginning 0)))
      );;nop
     ;; in comment
     ((or (nth 4 end-ppss) (nth 4 beg-ppss))
      ;; FIXME: member seems to be slow
      (when (member rule (ef-convert-rules comment-rules))
        (replace-match (cdr rule))))
     (t
      (when (member rule (ef-convert-rules rules))
        (replace-match (cdr rule)))))
    ))

(defun ef-region-1 (beg end rule rules comment-rules)
  (let ((end-marker (set-marker (make-marker) end))
        (converted-rule (ef-convert-rule rule))
        (orig-string
         ;; TODO:region
         (buffer-substring-no-properties (point-min) (point-max))))
    (save-excursion
      (goto-char beg)
      (while (and (< (point) (marker-position end-marker))
                  (re-search-forward (car converted-rule)
                                     (marker-position end-marker) t))
        (electric-formatter-region-func converted-rule
                                        rules comment-rules))
      )
    (prog1
        (unless
            (equal orig-string
                   (buffer-substring-no-properties (point-min) (point-max)))
          (cons beg (marker-position end-marker)))
      (set-marker end-marker nil)
      )))

(defun ef-region (beg end rules comment-rules)
  ;; TODO: integrate to buffer
  ;; narrowing is hard to integrate to other elisp (eg. align)
  (combine-after-change-calls
    ;; dolist has problem with edebug?
    (let ((beg beg)
          (end end))
      (mapc
       (lambda (rule)
         ;; TODO:change nesting
         (awhile (ef-region-1 beg end rule rules comment-rules)
           (setq end (cdr it))))
       (append rules comment-rules);; Do not convert rules for easy debug
       ))))

;;;###autoload
(defun electric-formatter-region (beg end &optional rules comment-rules)
  (interactive "r")
  (let ((pos (point))
        (eobp (eobp))
        (pos-marker (set-marker (make-marker) (point)))
        (beg (min beg end))
        (end (max beg end)))
    ;;end is moving
    (ef-region
     beg end
     (or rules ef-rule-list)
     (or comment-rules ef-comment-rule-list))
    ;;(electric-formatter-electric-1 beg end)
    (cond
     ((= beg pos)
      (goto-char beg))
     ((= end pos)) ;;nop
     ((and (< beg pos) (< pos end))
      (goto-char pos))
     (eobp
      (goto-char (point-max)))
     ((< pos beg)
      (goto-char pos))
     (t ;;(< end pos)
      (goto-char (marker-position pos-marker)))
     )
    (set-marker pos-marker nil)
    ))

;;;###autoload
(defun electric-formatter-buffer (&optional rules comment-rules)
  (interactive)
  (electric-formatter-region
   (point-min) (point-max)
   (or rules ef-rule-list)
   (or comment-rules ef-comment-rule-list)))

;;(assoc-default 'space-after ef-rule-list)

;;;###autoload
(define-minor-mode electric-formatter-mode
  "Toggle electric formatter."
  :lighter " EF"
  :group 'electric-formatter
  (if electric-formatter-mode
      (progn
        (add-hook 'post-self-insert-hook #'electric-formatter-post-self-insert-function nil t))
    (remove-hook 'post-self-insert-hook #'electric-formatter-post-self-insert-function t)))
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
(define-globalized-minor-mode global-electric-formatter-mode
  electric-formatter-mode electric-formatter-mode-maybe
  ;; :init-value t has bug?
  :group 'electric-formatter)

(provide 'electric-formatter)
;;; electric-formatter.el ends here
