;;; orderless.el --- Completion style for matching regexps in any order  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: extensions
;; Version: 0.1
;; Homepage: https://github.com/oantolin/orderless
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Warning: this package is experminental!
;;
;; This package provides an `orderless' completion style that divides
;; the pattern into space-separated chunks, treats each on as a
;; regexp, and matches canidates that match all of the regexps in any
;; order.
;;
;; Completion styles are used as entries in the variables
;; `completion-styles' and `completion-category-overrides', see their
;; documentation.
;;
;; By default the space key is bound to `minibuffer-complete-word' in
;; `minibuffer-local-map', which isn't useful with this completion
;; method. So, if you use it, you should also unbind SPC.
;;
;; So to test this completion you can use the following configuration:
;;
;; (setq completion-styles '(orderless))
;; (define-key minibuffer-local-map (kbd "SPC") #'self-insert-command)

;;; Code:

(require 'cl-lib)

(defgroup orderless nil
  "Completion method that matches space-separated regexps in any order."
  :group 'completion)

(defface orderless-match-face-0
  '((((class color) (min-colors 88) (background dark))
     (:foreground "#72a4ff" :weight bold))
    (((class color) (min-colors 88) (background light))
     (:foreground "#223fbf" :weight bold))
    (t :foreground "blue" :weight bold))
  "Face for matches of components numbered 0 mod 4."
  :group 'orderless)

(defface orderless-match-face-1
  '((((class color) (min-colors 88) (background dark))
     (:foreground "#ed92f8" :weight bold))
    (((class color) (min-colors 88) (background light))
     (:foreground "#8f0075" :weight bold))
    (t :foreground "magenta" :weight bold))
  "Face for matches of components numbered 1 mod 4."
  :group 'orderless)

(defface orderless-match-face-2
  '((((class color) (min-colors 88) (background dark))
     (:foreground "#90d800" :weight bold))
    (((class color) (min-colors 88) (background light))
     (:foreground "#145a00" :weight bold))
    (t :foreground "green" :weight bold))
  "Face for matches of components numbered 2 mod 4."
  :group 'orderless)

(defface orderless-match-face-3
  '((((class color) (min-colors 88) (background dark))
     (:foreground "#f0ce43" :weight bold))
    (((class color) (min-colors 88) (background light))
     (:foreground "#804000" :weight bold))
    (t :foreground "yellow" :weight bold))
  "Face for matches of components numbered 3 mod 4."
  :group 'orderless)

(defcustom orderless-regexp-separator " +"
  "Regexp to match component separators for orderless completion.
This is passed to `split-string' to divide the pattern into
component regexps."
  :type '(choice (const :tag "Spaces" " +")
                 (const :tag "Spaces, hyphen or slash" " +\\|[-/]")
                 (regexp :tag "Custom regexp"))
  :group 'orderless)

(let ((faces [orderless-match-face-0
              orderless-match-face-1
              orderless-match-face-2
              orderless-match-face-3]))
  (defun orderless--highlight-match (regexp string face)
    ;; only call this when the match has already been checked!
    (string-match regexp string)
    (font-lock-prepend-text-property
     (match-beginning 0)
     (match-end 0)
     'face (aref faces (mod face 4))
     string)))

(defun orderless-all-completions (string table pred _point)
  (save-match-data
    (let* ((limit (car (completion-boundaries string table pred "")))
           (prefix (substring string 0 limit))
           (all (all-completions prefix table pred))
           (regexps (split-string (substring string limit)
                                  orderless-regexp-separator
                                  t)))
      (when minibuffer-completing-file-name
        (setq all (completion-pcm--filename-try-filter all)))
      (condition-case nil
          (progn
            (setq all
                  (cl-loop for original in all
                           when
                           (cl-loop for regexp in regexps
                                    always (string-match-p regexp original))
                           collect      ; it's a match, copy and highlight
                           (cl-loop with candidate = (copy-sequence original)
                                    for regexp in regexps and face from 0 do
                                    (orderless--highlight-match
                                     regexp candidate face)
                                    finally (return candidate))))
            (when all (nconc all (length prefix))))
        (invalid-regexp nil)))))

(defun orderless-try-completion (string table pred point &optional _metadata)
  (let* ((limit (car (completion-boundaries string table pred "")))
         (prefix (substring string 0 limit))
         (all (orderless-all-completions string table pred point)))
    (cl-flet ((measured (string) (cons string (length string))))
      (cond
       ((null all) nil)
       ((atom (cdr all)) (measured (concat prefix (car all))))
       (t (measured string))))))

(cl-pushnew '(orderless
              orderless-try-completion orderless-all-completions
              "Completion of multiple regexps, in any order.")
            completion-styles-alist
            :test #'equal)

(provide 'orderless)
;;; orderless.el ends here
