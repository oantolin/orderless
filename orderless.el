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

(defun orderless-highlight-match (regexp string)
  (when (string-match regexp string)
    (font-lock-prepend-text-property
     (match-beginning 0)
     (match-end 0)
     'face 'completions-common-part
     string)
    t))

(defun orderless-all-completions (string table pred _point)
  (let* ((lim (car (completion-boundaries string table pred "")))
         (prefix (substring string 0 lim))
         (all (all-completions prefix table pred))
         (regexps (split-string (substring string lim))))
    (when minibuffer-completing-file-name
      (setq all (completion-pcm--filename-try-filter all)))
    (condition-case nil
        (progn
          (setq all
           (save-match-data
             (cl-loop for original in all
                      for candidate = (copy-sequence original)
                      when (cl-loop for regexp in regexps
                                    always (orderless-highlight-match
                                            regexp candidate))
                      collect candidate)))
          (when all (nconc all (length prefix))))
      (invalid-regexp nil))))

(defun orderless-try-completion (string table pred point &optional _metadata)
  (let* ((lim (car (completion-boundaries string table pred "")))
         (prefix (substring string 0 lim))
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
