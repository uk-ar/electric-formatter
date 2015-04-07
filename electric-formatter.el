;;; electric-formatter.el --- Realtime code formatter which independent of programming language.

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

;; Smart region guess what you want to select by one command.
;; If you call this command multiple times at the same position, it expands selected region (it calls ```er/expand-region```).
;; Else, if you move from the mark and call this command, it select the region rectangular (it call ```rectangle-mark-mode```).
;; Else, if you move from the mark and call this command at same column as mark, it add cursor to each line (it call ```mc/edit-lines```).

;; This basic concept is from [sense-region](https://gist.github.com/tnoda/1776988).

(require 'cl-lib)
;;; Code:
(defvar electric-formatter-list nil)
(make-variable-buffer-local 'electric-formatter-list)

(defun electric-formatter-regexp (formatter-list symbol)
  (let ((strings
         (mapcar 'cdr
                 (remove-if-not
                  (lambda (elem) (eq (car elem) symbol))
                  formatter-list))))
    (if strings
        (regexp-opt strings t))))

(defvar electric-formatter-beginning-regexp "\\(\\w\\|\\s.\\|\\s'\\|s\"\\)")
(defvar electric-formatter-end-regexp "\\(\\w\\|\\s.\\|\\s)\\|s\"\\)")

(defun electric-formatter-regexp-opt (formatter-list)
  (delq
   nil
   (append
    (let ((space-after
           (electric-formatter-regexp formatter-list 'space-after))
          (space-before
           (electric-formatter-regexp formatter-list 'space-before)))
      (list
       (if space-after
           (cons
            ;; include punctuation for ruby's symbol :foo
            (concat space-after electric-formatter-beginnig-regexp)
            "\\1 \\2"))
       (if space-before
           (cons
            (concat electric-formatter-end-regexp space-before)
            "\\1 \\2"))))
    (remove-if-not
     '(lambda (elem)
        (stringp (car elem)))
     formatter-list))))

(defun electric-formatter (string)
  (reduce #'electric-formatter-1
          (cons
           string
           (electric-formatter-regexp-opt electric-formatter-list))))

(defun electric-formatter-1 (string rule)
  (replace-regexp-in-string (car rule) (cdr rule) string))

(defun electric-formatter-electric ()
  (undo-boundary)
  (electric-formatter-region
             (line-beginning-position) (point)))

(defun electric-formatter-range (start end)
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

(defun electric-formatter-region-1 (beg end)
  (let ((beg (min beg end))
        (end-marker (set-marker (make-marker) (max beg end))))
    ;; parse forward because text inserted
    (goto-char (- beg 1));;-1 for forward-char
    (while (< (point) (marker-position end-marker))
      (forward-char 1)
      (cond
       ;; in string
       ((nth 3 (syntax-ppss))
        ;;until string end
        (skip-syntax-forward "^\"" (marker-position end-marker)))
       ;; in comment
       ((nth 4 (syntax-ppss))
        ;;until comment end
        (skip-syntax-forward "^>" (marker-position end-marker)))
       (t
        (let ((pos (point))
              );(offset (save-excursion (skip-syntax-backward "\">" beg)))
          ;;until comment or string start
          (skip-syntax-forward "^\"<" (marker-position end-marker))
          (electric-formatter-range
           (- pos 1);;-1 for forward-char
           (+ (point)
              (skip-syntax-forward
               "\"<" (+ 1 (point)))
           ))))))
    (set-marker end-marker nil)
    (cons beg (point))))

(defun electric-formatter-region (&optional beg end)
  (interactive "r")
  (let ((pos (point))
        (eobp (eobp))
        (pos-marker (set-marker (make-marker) (point)))
        (beg (min beg end))
        (end (max beg end)))
    (electric-formatter-region-1 beg end)
    ;;(electric-formatter-electric-1 beg end)
    (cond
     ((= beg pos)
      (goto-char beg))
     ((= end pos)) ;;nop
     ((and (< beg pos) (< pos end))
      (goto-char pos))
     (eobp
      (goto-char (point-max)))
     (t
      (goto-char (marker-position pos-marker))))
    (set-marker pos-marker nil)))

;;nconc?
(setq-default electric-formatter-list
              '((space-after . "=")
                (space-after . ",")
                (space-before . "=")))

;;(assoc-default 'space-after electric-formatter-list)

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
        '((space-after . "=")
          ;;(space-after . ",") macro escape
          (space-after . ")")
          (space-after . "\"")
          (space-after . ".")
          (space-before . "=")
          (space-before . "(")
          (space-before . "\"")
          (space-before . ".")
          )))

(add-hook 'emacs-lisp-mode-hook
          'electric-formatter-emacs-lisp-mode-setup)

(add-hook 'lisp-interaction-mode-hook
          'electric-formatter-emacs-lisp-mode-setup)

(provide 'electric-formatter)
