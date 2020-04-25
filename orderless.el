;;; orderless.el --- Completion style for matching regexps in any order  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: extensions
;; Version: 0.3
;; Homepage: https://github.com/oantolin/orderless
;; Package-Requires: ((emacs "24.4"))

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
;; the pattern into components (space-separated by default), and
;; matches candidates that match all of the components in any order.

;; Completion styles are used as entries in the variables
;; `completion-styles' and `completion-category-overrides', see their
;; documentation.

;; To use this completion style you can use the following minimal
;; configuration:

;; (setq completion-styles '(orderless))

;; You can customize the `orderless-component-separator' to decide how
;; the input pattern is split into component regexps.  The default
;; splits on spaces.  You might want to add hyphens and slashes, for
;; example, to ease completion of symbols and file paths,
;; respectively.

;; Each component can match in any one of several matching styles:
;; literally, as a regexp, as an initialism, in the flex style, or as
;; word prefixes.  It is easy to add new styles: they are functions
;; from strings to strings that map a component to a regexp to match
;; against.  The variable `orderless-component-matching-styles' lists
;; the matching styles to be used for components, by default it allows
;; regexp and initialism matching.

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

(defcustom orderless-component-separator " +"
  "Regexp to match component separators for orderless completion.
This is passed to `split-string' to divide the pattern into
component regexps."
  :type '(choice (const :tag "Spaces" " +")
                 (const :tag "Spaces, hyphen or slash" " +\\|[-/]")
                 (regexp :tag "Custom regexp"))
  :group 'orderless)

(defcustom orderless-match-faces
  [orderless-match-face-0
   orderless-match-face-1
   orderless-match-face-2
   orderless-match-face-3]
  "Vector of faces used (cyclically) for component matches."
  :type '(vector 'face)
  :group 'orderless)

(defcustom orderless-component-matching-styles
  '(orderless-regexp orderless-initialism)
  "Default matching style for a component.
Can be a list of functions to indicate various matching styles
are allowed, but for other possibilities see the function
`orderless-apply-matching-style'.  If this variable is nil, regexp
matching is assumed.

Matching styles transform pattern to regexps, according to
`orderless-apply-matching-style'.  If the resulting regexp has no
capturing groups, the entire match is highlighted, otherwise just
the captured groups are."
  :type '(choice
          (set :tag "Any of"
           (const :tag "Regexp" orderless-regexp)
           (const :tag "Literal" orderless-literal)
           (const :tag "Initialism" orderless-initialism)
           (const :tag "Flex" orderless-flex)
           (const :tag "Prefixes" orderless-prefixes))
          (sexp :tag "Custom atching style"))
  :group 'orderless)

(defcustom orderless-positional-overrides nil
  "List of overriding matching styles by index.
If the nth entry of this list is non-nil, it will be used instead
of `orderless-component-matching-style' for turning the nth
component of the input into a regexp."
  :type '(list sexp)
  :group 'orderless)

(defalias 'orderless-regexp #'identity
  "Match a component as a regexp.
This is simply the identity function.")

(defalias 'orderless-literal #'regexp-quote
  "Match a component as a literal string.
This is simply `regexp-quote'.")

(defun orderless--anything-between (rxs)
  "Return a regexp to match the rx-regexps RXS with .* in between."
  (rx-to-string
   `(seq ,@(cl-loop for (sexp . more) on rxs
                    collect `(group ,sexp)
                    when more collect `(zero-or-more nonl)))))

(defun orderless-flex (component)
  "Match a component in flex style.
This means the characters in COMPONENT must occur in the
candidate in that order, but not necessarily consecutively."
  (orderless--anything-between
   (cl-loop for char across component collect char)))

(defun orderless-initialism (component)
  "Match a component as an initialism.
This means the characters in COMPONENT must occur in the
candidate, in that order, at the beginning of words."
  (orderless--anything-between
   (cl-loop for char across component collect `(seq word-start ,char))))

(defun orderless-prefixes (component)
  "Match a component as multiple word prefixes.
The COMPONENT is split at word endings, and each piece must match
at a word boundary in the candidate.  This is similar to the
`partial-completion' completion style."
  (orderless--anything-between
   (cl-loop for prefix in (split-string component "\\>" t)
            collect `(seq word-boundary ,prefix))))

(defun orderless--highlight (regexps string)
  "Propertize STRING to highlight a match of each of the REGEXPS.
Warning: only use this if you know all REGEXPs match!"
  (cl-loop with n = (length orderless-match-faces)
           for regexp in regexps and i from 0 do
           (string-match regexp string)
           (cl-loop
            for (x y) on (or (cddr (match-data)) (match-data)) by #'cddr
            when x do
            (font-lock-prepend-text-property
             x y
             'face (aref orderless-match-faces (mod i n))
             string)))
  string)

(defun orderless-highlight-matches (regexps strings)
    "Highlight a match of each of the REGEXPS in each of the STRINGS.
Warning: only use this if you know all REGEXPs match all STRINGS!
For the user's convenience, if REGEXPS is a string, it is
converted to a list of regexps according to the value of
`orderless-component-matching-styles'."
    (when (stringp regexps)
      (setq regexps (orderless--component-regexps regexps)))
    (cl-loop for original in strings
             for string = (copy-sequence original)
             collect (orderless--highlight regexps string)))

(defun orderless-apply-matching-style (style pattern)
  "Apply the matching STYLE to PATTERN to produce a regexp.
A matching style can be:

1. A function, which is called with PATTERN as its argmuent and
must return either a regexp or nil, if it cannot hanlde the
PATTERN.

2. A list of matching styles, all of which are applied to PATTERN
and the various regexps returned are joined with `\\|'.

3. A vector of matching styles, which are applied in turn to
PATTERN and the first non-nil result is returned."
  (cond
   ((functionp style) (funcall style pattern))
   ((listp style)
    (rx-to-string
     `(or ,@(cl-loop for case in style
                     collect
                     `(regexp
                       ,(orderless-apply-matching-style case pattern))))))
   ((vectorp style)
    (cl-loop for attempt across style
             thereis (orderless-apply-matching-style attempt pattern)))))

(defun orderless--component-regexps (pattern)
  "Build regexps to match PATTERN.
Consults `orderless-component-matching-styles' and
`orderless-positional-overrides' to decide what to match."
  (let ((components (split-string pattern orderless-component-separator t))
        (overrides orderless-positional-overrides)
        (default orderless-component-matching-styles))
    (cl-loop for component in components and index from 0
             for style = (or (when (< index (length overrides))
                               (elt overrides index))
                             default
                             #'orderless-regexp)
             collect (orderless-apply-matching-style style component))))

(defun orderless--prefix+pattern (string table pred)
  "Split STRING into prefix and pattern according to TABLE.
The predicate PRED is used to constrain the entries in TABLE."
  (let ((limit (car (completion-boundaries string table pred ""))))
    (cons (substring string 0 limit) (substring string limit))))

;;;###autoload
(defun orderless-filter (string table &optional pred)
  "Split STRING into components and find entries TABLE matching all.
The predicate PRED is used to constrain the entries in TABLE."
  (condition-case nil
      (save-match-data
        (pcase-let* ((`(,prefix . ,pattern)
                      (orderless--prefix+pattern string table pred))
                     (completion-regexp-list
                      (orderless--component-regexps pattern)))
          (all-completions prefix table pred)))
    (invalid-regexp nil)))

;;;###autoload
(defun orderless-all-completions (string table pred _point)
  "Split STRING into components and find entries TABLE matching all.
The predicate PRED is used to constrain the entries in TABLE.  The
matching portions of each candidate are highlighted.
This function is part of the `orderless' completion style."
  (let ((completions (orderless-filter string table pred)))
    (when completions
      (pcase-let ((`(,prefix . ,pattern)
                   (orderless--prefix+pattern string table pred)))
        (nconc
         (orderless-highlight-matches pattern completions)
         (length prefix))))))

;;;###autoload
(defun orderless-try-completion (string table pred point &optional _metadata)
  "Complete STRING to unique matching entry in TABLE.
This uses `orderless-all-completions' to find matches for STRING
in TABLE among entries satisfying PRED.  If there is only one
match, it completes to that match.  If there are no matches, it
returns nil.  In any other case it \"completes\" STRING to
itself, without moving POINT.
This function is part of the `orderless' completion style."
  (let ((all (orderless-filter string table pred)))
    (cond
     ((null all) nil)
     ((null (cdr all))
      (let ((full (concat
                   (car (orderless--prefix+pattern string table pred))
                   (car all))))
        (cons full (length full))))
     (t (cons string point)))))

;;;###autoload
(cl-pushnew '(orderless
              orderless-try-completion orderless-all-completions
              "Completion of multiple components, in any order.")
            completion-styles-alist
            :test #'equal)

(defvar orderless-old-component-separator nil
  "Stores the old value of `orderless-component-separator'.")

(defun orderless--restore-component-separator ()
  "Restore old value of `orderless-component-separator'."
  (when orderless-old-component-separator
    (setq orderless-component-separator orderless-old-component-separator
          orderless-old-component-separator nil))
  (remove-hook 'minibuffer-exit-hook #'orderless--restore-component-separator))

(defun orderless-temporarily-change-separator (separator)
  "Use SEPARATOR to split the input for the current completion session."
  (interactive
   (list (let ((enable-recursive-minibuffers t))
           (read-string "Orderless regexp separator: "))))
  (unless orderless-old-component-separator
    (setq orderless-old-component-separator orderless-component-separator))
  (setq orderless-component-separator separator)
  (add-to-list 'minibuffer-exit-hook #'orderless--restore-component-separator))

;;; ivy integration

(defvar ivy-regex)
(defvar ivy-highlight-functions-alist)

;;;###autoload
(defun orderless-ivy-re-builder (str)
  "Convert STR into regexps for use with ivy.
This function is for integration of orderless with ivy, use it as
a value in `ivy-re-builders-alist'."
  (or (mapcar (lambda (x) (cons x t)) (orderless--component-regexps str)) ""))

(defun orderless-ivy-highlight (str)
  "Highlight a match in STR of each regexp in `ivy-regex'.
This function is for integration of orderless with ivy."
  (orderless--highlight (mapcar #'car ivy-regex) str) str)

;;;###autoload
(with-eval-after-load 'ivy
  (add-to-list 'ivy-highlight-functions-alist
               '(orderless-ivy-re-builder . orderless-ivy-highlight)))

(provide 'orderless)
;;; orderless.el ends here
