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

;; This package provides an `orderless' completion style that divides
;; the pattern into components chunks (space-separated by default),
;; treats each on as a regexp, and matches candidates that match all of
;; the regexps in any order.
;;
;; Completion styles are used as entries in the variables
;; `completion-styles' and `completion-category-overrides', see their
;; documentation.
;;
;; To use this completion style you can use the following minimal
;; configuration:
;;
;; (setq completion-styles '(orderless))
;;
;; You can customize the `orderless-regexp-separator' to decide how
;; the input pattern is split into component regexps.  The default
;; splits on spaces.  You might want to add hyphens and slashes, for
;; example, to ease completion of symbols and file paths,
;; respectively.

;;; Code:

(require 'cl-lib)

(defgroup orderless nil
  "Completion method that matches space-separated regexps in any order."
  :group 'completion)

(defface orderless-match-face-0
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#72a4ff")
    (((class color) (min-colors 88) (background light)) :foreground "#223fbf")
    (t :foreground "blue"))
  "Face for matches of components numbered 0 mod 4."
  :group 'orderless)

(defface orderless-match-face-1
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#ed92f8")
    (((class color) (min-colors 88) (background light)) :foreground "#8f0075")
    (t :foreground "magenta"))
  "Face for matches of components numbered 1 mod 4."
  :group 'orderless)

(defface orderless-match-face-2
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#90d800")
    (((class color) (min-colors 88) (background light)) :foreground "#145a00")
    (t :foreground "green"))
  "Face for matches of components numbered 2 mod 4."
  :group 'orderless)

(defface orderless-match-face-3
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#f0ce43")
    (((class color) (min-colors 88) (background light)) :foreground "#804000")
    (t :foreground "yellow"))
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
    "Highlight REGEXP match in STRING with the face numbered FACE.
Warning: only call this function when you know REGEXP matches STRING!"
    (string-match regexp string)
    (font-lock-prepend-text-property
     (match-beginning 0)
     (match-end 0)
     'face (aref faces (mod face 4))
     string)))

(defun orderless-all-completions (string table pred _point)
  "Split STRING into components and find entries TABLE matching all.
The predicate PRED is used to constrain the entries in TABLE.
This function is part of the `orderless' completion style."
  (save-match-data
    (let* ((limit (car (completion-boundaries string table pred "")))
           (all-regexps (split-string (substring string limit)
                                      orderless-regexp-separator
                                      t))
           (pending-regexps all-regexps)
           (first (if (> limit 0)
                      (substring string 0 limit)
                    (let ((first (car all-regexps)))
                      (if (not (string= first (regexp-quote first)))
                          ""
                        (setq pending-regexps (cdr all-regexps))
                        first))))
           (all (all-completions first table pred)))
      (when minibuffer-completing-file-name
        (setq all (completion-pcm--filename-try-filter all)))
      (condition-case nil
          (progn
            (setq all
                  (cl-loop for original in all
                           when
                           (cl-loop for regexp in pending-regexps
                                    always (string-match-p regexp original))
                           collect      ; it's a match, copy and highlight
                           (cl-loop with candidate = (copy-sequence original)
                                    for regexp in all-regexps and face from 0 do
                                    (orderless--highlight-match
                                     regexp candidate face)
                                    finally (return candidate))))
            (when all (nconc all limit)))
        (invalid-regexp nil)))))

(defun orderless-try-completion (string table pred point &optional _metadata)
  "Complete STRING to unique matching entry in TABLE.
This uses `orderless-all-completions' to find matches for STRING
in TABLE among entries satisfying PRED (that function ignores
POINT).  If there is only one match, it completes to that match.
If there are no matches, it returns nil.  In any other case it
\"completes\" STRING to itself.  This function is part of the
`orderless' completion style."
  (let* ((limit (car (completion-boundaries string table pred "")))
         (prefix (substring string 0 limit))
         (all (orderless-all-completions string table pred point)))
    (cond
     ((null all) nil)
     ((atom (cdr all))
      (let ((full (concat prefix (car all))))
        (cons full (length full))))
     (t (cons string point)))))

(cl-pushnew '(orderless
              orderless-try-completion orderless-all-completions
              "Completion of multiple regexps, in any order.")
            completion-styles-alist
            :test #'equal)

(provide 'orderless)
;;; orderless.el ends here
