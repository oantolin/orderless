;;; orderless.el --- Completion style for matching regexps in any order  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Maintainer: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: extensions
;; Version: 0.7
;; Homepage: https://github.com/oantolin/orderless
;; Package-Requires: ((emacs "26.1"))

;; This file is part of GNU Emacs.

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
;; against.  The variable `orderless-matching-styles' lists the
;; matching styles to be used for components, by default it allows
;; literal and regexp matching.

;;; Code:

(require 'cl-lib)

(defgroup orderless nil
  "Completion method that matches space-separated regexps in any order."
  :group 'minibuffer)

(defface orderless-match-face-0
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#72a4ff")
    (((class color) (min-colors 88) (background light)) :foreground "#223fbf")
    (t :foreground "blue"))
  "Face for matches of components numbered 0 mod 4.")

(defface orderless-match-face-1
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#ed92f8")
    (((class color) (min-colors 88) (background light)) :foreground "#8f0075")
    (t :foreground "magenta"))
  "Face for matches of components numbered 1 mod 4.")

(defface orderless-match-face-2
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#90d800")
    (((class color) (min-colors 88) (background light)) :foreground "#145a00")
    (t :foreground "green"))
  "Face for matches of components numbered 2 mod 4.")

(defface orderless-match-face-3
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#f0ce43")
    (((class color) (min-colors 88) (background light)) :foreground "#804000")
    (t :foreground "yellow"))
  "Face for matches of components numbered 3 mod 4.")

(defcustom orderless-component-separator " +"
  "Component separators for orderless completion.
This can either be a string, which is passed to `split-string',
or a function of a single string argument."
  :type '(choice (const :tag "Spaces" " +")
                 (const :tag "Spaces, hyphen or slash" " +\\|[-/]")
                 (const :tag "Escapable space"
                        orderless-escapable-split-on-space)
                 (const :tag "Quotable spaces" split-string-and-unquote)
                 (regexp :tag "Custom regexp")
                 (function : tag "Custom function")))

(defcustom orderless-match-faces
  [orderless-match-face-0
   orderless-match-face-1
   orderless-match-face-2
   orderless-match-face-3]
  "Vector of faces used (cyclically) for component matches."
  :type '(vector face))

(defcustom orderless-skip-highlighting nil
  "Skip highlighting the matching parts of candidates?
If this is set to a function, the function is called to decide
whether to skip higlighting the matches.  Any non-function non-nil
value means highlighting is skipped."
  :type '(choice boolean function))

(defcustom orderless-matching-styles
  '(orderless-literal orderless-regexp)
  "List of component matching styles.
If this variable is nil, regexp matching is assumed.

A matching style is simply a function from strings to strings
that takes a component to a regexp to match against.  If the
resulting regexp has no capturing groups, the entire match is
highlighted, otherwise just the captured groups are.  Several are
provided with this package: try customizing this variable to see
a list of them."
  :type 'hook
  :options '(orderless-regexp
             orderless-literal
             orderless-initialism
             orderless-prefixes
             orderless-flex))

(defcustom orderless-style-dispatchers nil
  "List of style dispatchers.
Style dispatchers are used to override the matching styles
based on the actual component and its place in the list of
components.  A style dispatcher is a function that takes a string
and two integers as arguments, it gets called with a component,
the 0-based index of the component and the total number of
components.  It can decide what matching styles to use for the
component and optionally replace the component with a different
string, or it can decline to handle the component leaving it for
future dispatchers.  For details see `orderless-dispatch'.

For example, a style dispatcher could arrange for the first
component to match as an initialism and subsequent components to
match as literals.  As another example, a style dispatcher could
arrange for a component starting with `?' to match the rest of
the component in the `orderless-flex' style.  For more
information on how this variable is used, see
`orderless-pattern-compiler'."
  :type 'hook)

(defcustom orderless-smart-case t
  "Whether to use smart case.
If this variable is t, then case-sensitivity is decided as
follows: if any component contains upper case letters, the
matches are case sensitive; otherwise case-insensitive.  This
like the behavior of `isearch' when `search-upper-case' is
non-nil.

On the other hand, if this variable is nil, then case-sensitivity
is determined by the values of `completion-ignore-case',
`read-file-name-completion-ignore-case' and
`read-buffer-completion-ignore-case', as usual for completion."
  :type 'boolean)

;;; Matching styles

(defun orderless-regexp (component)
  "Match COMPONENT as a regexp."
  (condition-case nil
      (progn (string-match-p component "") component)
    (invalid-regexp nil)))

(defalias 'orderless-literal #'regexp-quote
  "Match a component as a literal string.
This is simply `regexp-quote'.")

(defun orderless--separated-by (sep rxs &optional before after)
  "Return a regexp to match the rx-regexps RXS with SEP in between.
If BEFORE is specified, add it to the beginning of the rx
sequence.  If AFTER is specified, add it to the end of the rx
sequence."
  (declare (indent 1))
  (rx-to-string
   `(seq
     ,(or before "")
     ,@(cl-loop for (sexp . more) on rxs
                collect `(group ,sexp)
                when more collect sep)
     ,(or after ""))))

(defun orderless-flex (component)
  "Match a component in flex style.
This means the characters in COMPONENT must occur in the
candidate in that order, but not necessarily consecutively."
  (rx-to-string
   `(seq
     ,@(cdr (cl-loop for char across component
                     append `((zero-or-more (not ,char)) (group ,char)))))))

(defun orderless-initialism (component)
  "Match a component as an initialism.
This means the characters in COMPONENT must occur in the
candidate, in that order, at the beginning of words."
  (orderless--separated-by '(zero-or-more nonl)
    (cl-loop for char across component collect `(seq word-start ,char))))

(defun orderless-prefixes (component)
  "Match a component as multiple word prefixes.
The COMPONENT is split at word endings, and each piece must match
at a word boundary in the candidate.  This is similar to the
`partial-completion' completion style."
  (orderless--separated-by '(zero-or-more nonl)
    (cl-loop for prefix in (split-string component "\\>")
             collect `(seq word-boundary ,prefix))))

(defun orderless-without-literal (component)
  "Match strings that do *not* contain COMPONENT as a literal match."
  (rx-to-string
   `(seq
     (group string-start)               ; highlight nothing!
     (zero-or-more
      (or ,@(cl-loop for i below (length component)
                     collect `(seq ,(substring component 0 i)
                                   (or (not (any ,(aref component i)))
                                       string-end)))))
     string-end)))

;;; Highlighting matches

(defun orderless--highlight (regexps string)
  "Propertize STRING to highlight a match of each of the REGEXPS."
  (cl-loop with n = (length orderless-match-faces)
           for regexp in regexps and i from 0
           when (string-match regexp string) do
           (cl-loop
            for (x y) on (let ((m (match-data))) (or (cddr m) m)) by #'cddr
            when x do
            (add-face-text-property
             x y
             (aref orderless-match-faces (mod i n))
             nil string)))
  string)

(defun orderless-highlight-matches (regexps strings)
  "Highlight a match of each of the REGEXPS in each of the STRINGS.
Warning: only use this if you know all REGEXPs match all STRINGS!
For the user's convenience, if REGEXPS is a string, it is
converted to a list of regexps according to the value of
`orderless-matching-styles'."
  (when (stringp regexps)
    (setq regexps (orderless-pattern-compiler regexps)))
  (let ((case-fold-search
         (if orderless-smart-case
             (cl-loop for regexp in regexps
                      always (isearch-no-upper-case-p regexp t))
           completion-ignore-case)))
    (cl-loop for original in strings
             for string = (copy-sequence original)
             collect (orderless--highlight regexps string))))

;;; Compiling patterns to lists of regexps

(defun orderless-escapable-split-on-space (string)
  "Split STRING on spaces, which can be escaped with backslash."
  (mapcar
   (lambda (piece) (replace-regexp-in-string (string 0) " " piece))
   (split-string (replace-regexp-in-string
                  "\\\\\\\\\\|\\\\ "
                  (lambda (x) (if (equal x "\\ ") (string 0) x))
                  string 'fixedcase 'literal)
                 " +" t)))

(defun orderless-dispatch (dispatchers default string &rest args)
  "Run DISPATCHERS to compute matching styles for STRING.

A style dispatcher is a function that takes a string and possibly
some extra arguments.  It should either return (a) nil to
indicate the dispatcher will not handle the string, (b) a new
string to replace the current string and continue dispatch,
or (c) the matching styles to use and, if needed, a new string to
use in place of the current one (for example, a dispatcher can
decide which style to use based on a suffix of the string and
then it must also return the component stripped of the suffix).

More precisely, the return value of a style dispatcher can be of
one of the following forms:

- nil (to continue dispatching)

- a string (to replace the component and continue dispatching),

- a matching style or non-empty list of matching styles to
  return,

- a `cons' whose `car' is either as in the previous case or
  nil (to request returning the DEFAULT matching styles), and
  whose `cdr' is a string (to replace the current one).

This function tries all DISPATCHERS in sequence until one returns
a list of styles (passing any extra ARGS to every style
dispatcher).  When that happens it returns a `cons' of the list
of styles and the possibly updated STRING.  If none of the
DISPATCHERS returns a list of styles, the return value will use
DEFAULT as the list of styles."
  (cl-loop for dispatcher in dispatchers
           for result = (apply dispatcher string args)
           if (stringp result)
           do (setq string result result nil)
           else if (and (consp result) (null (car result)))
           do (setf (car result) default)
           else if (and (consp result) (stringp (cdr result)))
           do (setq string (cdr result) result (car result))
           when result return (cons result string)
           finally (return (cons default string))))

(defun orderless-pattern-compiler (pattern &optional styles dispatchers)
  "Build regexps to match the components of PATTERN.
Split PATTERN on `orderless-component-separator' and compute
matching styles for each component.  For each component the style
DISPATCHERS are run to determine the matching styles to be used;
they are called with arguments the component, the 0-based index
of the component and the total number of components.  If the
DISPATCHERS decline to handle the component, then the list of
matching STYLES is used.  See `orderless-dispatch' for details on
dispatchers.

The STYLES default to `orderless-matching-styles', and the
DISPATCHERS default to `orderless-dipatchers'.  Since nil gets you
the default, if want to no dispatchers to be run, use \\='(ignore)
as the value of DISPATCHERS."
  (unless styles (setq styles orderless-matching-styles))
  (unless dispatchers (setq dispatchers orderless-style-dispatchers))
  (cl-loop
   with components = (if (functionp orderless-component-separator)
                         (funcall orderless-component-separator pattern)
                       (split-string pattern orderless-component-separator t))
   with total = (length components)
   for component in components and index from 0
   for (newstyles . newcomp) = (orderless-dispatch
                                dispatchers styles component index total)
   when (functionp newstyles) do (setq newstyles (list newstyles))
   for regexps = (cl-loop for style in newstyles
                          for result = (funcall style newcomp)
                          when result collect `(regexp ,result))
   when regexps collect (rx-to-string `(or ,@(delete-dups regexps)))))

;;; Completion style implementation

(defun orderless--prefix+pattern (string table pred)
  "Split STRING into prefix and pattern according to TABLE.
The predicate PRED is used to constrain the entries in TABLE."
  (let ((limit (car (completion-boundaries string table pred ""))))
    (cons (substring string 0 limit) (substring string limit))))

;; Thanks to @jakanakaevangeli for writing a version of this function:
;; https://github.com/oantolin/orderless/issues/79#issuecomment-916073526
(defun orderless--anchored-quoted-regexp (regexp)
  "Determine if REGEXP is a quoted regexp anchored at the beginning.
If REGEXP is of the form \"\\(?:^q\\)\" for q = (regexp-quote u),
then return (cons REGEXP u); else return nil."
  (when (and (string-prefix-p "\\(?:^" regexp) (string-suffix-p "\\)" regexp))
    (let ((trimmed (substring regexp 5 -2)))
      (unless (string-match-p "[$*+.?[\\^]"
                              (replace-regexp-in-string
                               "\\\\[$*+.?[\\^]" "" trimmed
                               'fixedcase 'literal))
        (cons regexp
              (replace-regexp-in-string "\\\\\\([$*+.?[\\^]\\)" "\\1"
                                        trimmed 'fixedcase))))))

;;;###autoload
(defun orderless-filter (string table &optional pred)
  "Split STRING into components and find entries TABLE matching all.
The predicate PRED is used to constrain the entries in TABLE."
  (save-match-data
    (pcase-let* ((`(,prefix . ,pattern)
                  (orderless--prefix+pattern string table pred))
                 (completion-regexp-list
                  (orderless-pattern-compiler pattern))
                 (completion-ignore-case
                  (if orderless-smart-case
                      (cl-loop for regexp in completion-regexp-list
                               always (isearch-no-upper-case-p regexp t))
                    completion-ignore-case)))
      ;; If there is a regexp of the form \(?:^quoted-regexp\) then
      ;; remove the first such and add the unquoted form to the prefix.
      (pcase (cl-some #'orderless--anchored-quoted-regexp
                      completion-regexp-list)
        (`(,regexp . ,literal)
         (setq prefix (concat prefix literal)
               completion-regexp-list (delete regexp completion-regexp-list))))
      (all-completions prefix table pred))))

;;;###autoload
(defun orderless-all-completions (string table pred _point)
  "Split STRING into components and find entries TABLE matching all.
The predicate PRED is used to constrain the entries in TABLE.  The
matching portions of each candidate are highlighted.
This function is part of the `orderless' completion style."
  (let ((completions (orderless-filter string table pred)))
    (when completions
      (pcase-let ((`(,prefix . ,pattern)
                   (orderless--prefix+pattern string table pred))
                  (skip-highlighting
                   (if (functionp orderless-skip-highlighting)
                       (funcall orderless-skip-highlighting)
                     orderless-skip-highlighting)))
        (nconc
         (if skip-highlighting
             completions
           (orderless-highlight-matches pattern completions))
         (length prefix))))))

;;;###autoload
(defun orderless-try-completion (string table pred point)
  "Complete STRING to unique matching entry in TABLE.
This uses `orderless-all-completions' to find matches for STRING
in TABLE among entries satisfying PRED.  If there is only one
match, it completes to that match.  If there are no matches, it
returns nil.  In any other case it \"completes\" STRING to
itself, without moving POINT.
This function is part of the `orderless' completion style."
  (catch 'orderless--many
    (let (one)
      ;; Abuse all-completions/orderless-filter as a fast search loop.
      ;; Should be almost allocation-free since our "predicate" is not
      ;; called more than two times.
      (orderless-filter
       string table
       ;; key/value for hash tables
       (lambda (&rest args)
         (when (or (not pred) (apply pred args))
           (setq args (car args) ;; first argument is key
                 args (if (consp args) (car args) args) ;; alist
                 args (if (symbolp args) (symbol-name args) args))
           (when (and one (not (equal one args)))
             (throw 'orderless--many (cons string point)))
           (setq one args)
           t)))
      (when one
        (if (equal string one)
            t ;; unique exact match
          (setq one (concat (car (orderless--prefix+pattern string table pred))
                            one))
          (cons one (length one)))))))

;;;###autoload
(add-to-list 'completion-styles-alist
             '(orderless
               orderless-try-completion orderless-all-completions
               "Completion of multiple components, in any order."))

(defmacro orderless-define-completion-style
    (name &optional docstring &rest configuration)
  "Define an orderless completion style with given CONFIGURATION.
The CONFIGURATION should be a list of bindings that you could use
with `let' to configure orderless.  You can include bindings for
`orderless-matching-styles' and `orderless-style-dispatchers',
for example.

The completion style consists of two functions that this macro
defines for you, NAME-try-completion and NAME-all-completions.
This macro registers those in `completion-styles-alist' as
forming the completion style NAME.

The optional DOCSTRING argument is used as the documentation
string for the completion style."
  (declare (doc-string 2) (indent 1))
  (unless (stringp docstring)
    (push docstring configuration)
    (setq docstring nil))
  (let* ((fn-name (lambda (string) (intern (concat (symbol-name name) string))))
         (try-completion  (funcall fn-name "-try-completion"))
         (all-completions (funcall fn-name "-all-completions"))
         (doc-fmt "`%s' function for the %s style.
This function delegates to `orderless-%s'.
The orderless configuration is locally modified
specifically for the %s style.")
         (fn-doc (lambda (fn) (format doc-fmt fn name fn name name))))
  `(progn
     (defun ,try-completion (string table pred point)
       ,(funcall fn-doc "try-completion")
       (let ,configuration
         (orderless-try-completion string table pred point)))
     (defun ,all-completions (string table pred point)
       ,(funcall fn-doc "all-completions")
       (let ,configuration
         (orderless-all-completions string table pred point)))
     (add-to-list 'completion-styles-alist
                  '(,name ,try-completion ,all-completions ,docstring)))))

;;; Ivy integration

;;;###autoload
(defun orderless-ivy-re-builder (str)
  "Convert STR into regexps for use with ivy.
This function is for integration of orderless with ivy, use it as
a value in `ivy-re-builders-alist'."
  (or (mapcar (lambda (x) (cons x t))
              (orderless-pattern-compiler str))
      ""))

(defvar ivy-regex)
(defun orderless-ivy-highlight (str)
  "Highlight a match in STR of each regexp in `ivy-regex'.
This function is for integration of orderless with ivy."
  (orderless--highlight (mapcar #'car ivy-regex) str) str)

(provide 'orderless)
;;; orderless.el ends here
