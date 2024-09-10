;;; orderless.el --- Completion style for matching regexps in any order  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Maintainer: Omar Antolín Camarena <omar@matem.unam.mx>, Daniel Mendler <mail@daniel-mendler.de>
;; Keywords: extensions
;; Version: 1.2
;; Homepage: https://github.com/oantolin/orderless
;; Package-Requires: ((emacs "27.1") (compat "30"))

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

;; (setq completion-styles '(orderless basic))

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

(require 'compat)
(eval-when-compile (require 'cl-lib))

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

(defcustom orderless-component-separator #'orderless-escapable-split-on-space
  "Component separators for orderless completion.
This can either be a string, which is passed to `split-string',
or a function of a single string argument."
  :type `(choice (const :tag "Spaces" " +")
                 (const :tag "Spaces, hyphen or slash" " +\\|[-/]")
                 (const :tag "Escapable space"
                        ,#'orderless-escapable-split-on-space)
                 (const :tag "Quotable spaces" ,#'split-string-and-unquote)
                 (regexp :tag "Custom regexp")
                 (function :tag "Custom function")))

(defcustom orderless-match-faces
  [orderless-match-face-0
   orderless-match-face-1
   orderless-match-face-2
   orderless-match-face-3]
  "Vector of faces used (cyclically) for component matches."
  :type '(vector face))

(defcustom orderless-matching-styles
  (list #'orderless-literal #'orderless-regexp)
  "List of component matching styles.
If this variable is nil, regexp matching is assumed.

A matching style is simply a function from strings to regexps.
The returned regexps can be either strings or s-expressions in
`rx' syntax.  If the resulting regexp has no capturing groups,
the entire match is highlighted, otherwise just the captured
groups are.  Several are provided with this package: try
customizing this variable to see a list of them."
  :type 'hook
  :options (list #'orderless-regexp
                 #'orderless-literal
                 #'orderless-initialism
                 #'orderless-prefixes
                 #'orderless-flex))

(defcustom orderless-affix-dispatch-alist
  `((?% . ,#'char-fold-to-regexp)
    (?! . ,#'orderless-not)
    (?& . ,#'orderless-annotation)
    (?, . ,#'orderless-initialism)
    (?= . ,#'orderless-literal)
    (?^ . ,#'orderless-literal-prefix)
    (?~ . ,#'orderless-flex))
  "Alist associating characters to matching styles.
The function `orderless-affix-dispatch' uses this list to
determine how to match a pattern component: if the component
either starts or ends with a character used as a key in this
alist, the character is removed from the component and the rest is
matched according the style associated to it."
  :type `(alist
          :key-type character
          :value-type (choice
                       (const :tag "Annotation" ,#'orderless-annotation)
                       (const :tag "Literal" ,#'orderless-literal)
                       (const :tag "Without literal" ,#'orderless-without-literal)
                       (const :tag "Literal prefix" ,#'orderless-literal-prefix)
                       (const :tag "Regexp" ,#'orderless-regexp)
                       (const :tag "Not" ,#'orderless-not)
                       (const :tag "Flex" ,#'orderless-flex)
                       (const :tag "Initialism" ,#'orderless-initialism)
                       (const :tag "Prefixes" ,#'orderless-prefixes)
                       (const :tag "Ignore diacritics" ,#'char-fold-to-regexp)
                       (function :tag "Custom matching style"))))

(defun orderless-affix-dispatch (component _index _total)
  "Match COMPONENT according to the styles in `orderless-affix-dispatch-alist'.
If the COMPONENT starts or ends with one of the characters used
as a key in `orderless-affix-dispatch-alist', then that character
is removed and the remainder of the COMPONENT is matched in the
style associated to the character."
  (let ((len (length component))
        (alist orderless-affix-dispatch-alist))
    (when (> len 0)
      (cond
       ;; Ignore single dispatcher character
       ((and (= len 1) (alist-get (aref component 0) alist)) #'ignore)
       ;; Prefix
       ((when-let ((style (alist-get (aref component 0) alist)))
          (cons style (substring component 1))))
       ;; Suffix
       ((when-let ((style (alist-get (aref component (1- len)) alist)))
          (cons style (substring component 0 -1))))))))

(defcustom orderless-style-dispatchers (list #'orderless-affix-dispatch)
  "List of style dispatchers.
Style dispatchers are used to override the matching styles
based on the actual component and its place in the list of
components.  A style dispatcher is a function that takes a string
and two integers as arguments, it gets called with a component,
the 0-based index of the component and the total number of
components.  It can decide what matching styles to use for the
component and optionally replace the component with a different
string, or it can decline to handle the component leaving it for
future dispatchers.  For details see `orderless--dispatch'.

For example, a style dispatcher could arrange for the first
component to match as an initialism and subsequent components to
match as literals.  As another example, a style dispatcher could
arrange for a component starting with `~' to match the rest of
the component in the `orderless-flex' style.  See
`orderless-affix-dispatch' and `orderless-affix-dispatch-alist'
for such a configuration.  For more information on how this
variable is used, see `orderless-compile'."
  :type 'hook)

(defcustom orderless-smart-case t
  "Whether to use smart case.
If this variable is t, then case-sensitivity is decided as
follows: if any component contains upper case letters, the
matches are case sensitive; otherwise case-insensitive.  This
is like the behavior of `isearch' when `search-upper-case' is
non-nil.

On the other hand, if this variable is nil, then case-sensitivity
is determined by the values of `completion-ignore-case',
`read-file-name-completion-ignore-case' and
`read-buffer-completion-ignore-case', as usual for completion."
  :type 'boolean)

(defcustom orderless-expand-substring 'prefix
  "Whether to perform literal substring expansion.
This configuration option affects the behavior of some completion
interfaces when pressing TAB.  If enabled `orderless-try-completion'
will first attempt literal substring expansion.  If disabled,
expansion is only performed for single unique matches.  For
performance reasons only `prefix' expansion is enabled by default.
Set the variable to `substring' for full substring expansion."
  :type '(choice (const :tag "No expansion" nil)
                 (const :tag "Substring" substring)
                 (const :tag "Prefix (efficient)" prefix)))

;;; Matching styles

(defun orderless-regexp (component)
  "Match COMPONENT as a regexp."
  (condition-case nil
      (progn (string-match-p component "") component)
    (invalid-regexp nil)))

(defun orderless-literal (component)
  "Match COMPONENT as a literal string."
  ;; Do not use (literal component) here, such that `delete-dups' in
  ;; `orderless--compile-component' has a chance to delete duplicates for
  ;; literal input. The default configuration of `orderless-matching-styles'
  ;; with `orderless-regexp' and `orderless-literal' leads to duplicates.
  (regexp-quote component))

(defun orderless-literal-prefix (component)
  "Match COMPONENT as a literal prefix string."
  `(seq bos (literal ,component)))

(defun orderless--separated-by (sep rxs &optional before after)
  "Return a regexp to match the rx-regexps RXS with SEP in between.
If BEFORE is specified, add it to the beginning of the rx
sequence.  If AFTER is specified, add it to the end of the rx
sequence."
  (declare (indent 1))
  `(seq
    ,(or before "")
    ,@(cl-loop for (sexp . more) on rxs
               collect `(group ,sexp)
               when more collect sep)
    ,(or after "")))

(defun orderless-flex (component)
  "Match a component in flex style.
This means the characters in COMPONENT must occur in the
candidate in that order, but not necessarily consecutively."
  `(seq
    ,@(cdr (cl-loop for char across component
                    append `((zero-or-more (not ,char)) (group ,char))))))

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
  "Match strings that do *not* contain COMPONENT as a literal match.
You may prefer to use the more general `orderless-not' instead
which can invert any predicate or regexp."
  `(seq
    (group string-start)               ; highlight nothing!
    (zero-or-more
     (or ,@(cl-loop for i below (length component)
                    collect `(seq ,(substring component 0 i)
                                  (or (not (any ,(aref component i)))
                                      string-end)))))
    string-end))

(defsubst orderless--match-p (pred regexp str)
  "Return t if STR matches PRED and REGEXP."
  (and str
       (or (not pred) (funcall pred str))
       (or (not regexp)
           (let ((case-fold-search completion-ignore-case))
             (string-match-p regexp str)))))

(defun orderless-not (pred regexp)
  "Match strings that do *not* match PRED and REGEXP."
  (lambda (str)
    (not (orderless--match-p pred regexp str))))

(defun orderless--metadata ()
  "Return completion metadata iff inside minibuffer."
  (when-let (((minibufferp))
             (table minibuffer-completion-table))
    ;; Return non-nil metadata iff inside minibuffer
    (or (completion-metadata (buffer-substring-no-properties
                              (minibuffer-prompt-end) (point))
                             table minibuffer-completion-predicate)
        '((nil . nil)))))

(defun orderless-annotation (pred regexp)
  "Match candidates where the annotation matches PRED and REGEXP."
  (let ((md (orderless--metadata)))
    (if-let ((fun (compat-call completion-metadata-get md 'affixation-function)))
        (lambda (str)
          (cl-loop for s in (cdar (funcall fun (list str)))
                   thereis (orderless--match-p pred regexp s)))
      (when-let ((fun (compat-call completion-metadata-get md 'annotation-function)))
          (lambda (str) (orderless--match-p pred regexp (funcall fun str)))))))

;;; Highlighting matches

(defun orderless--highlight (regexps ignore-case string)
  "Destructively propertize STRING to highlight a match of each of the REGEXPS.
The search is case insensitive if IGNORE-CASE is non-nil."
  (cl-loop with case-fold-search = ignore-case
           with n = (length orderless-match-faces)
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
    (setq regexps (cdr (orderless-compile regexps))))
  (cl-loop with ignore-case = (orderless--ignore-case-p regexps)
           for str in strings
           collect (orderless--highlight regexps ignore-case (substring str))))

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

(defun orderless--dispatch (dispatchers default string index total)
  "Run DISPATCHERS to compute matching styles for STRING.

A style dispatcher is a function that takes a STRING, component
INDEX and the TOTAL number of components.  It should either
return (a) nil to indicate the dispatcher will not handle the
string, (b) a new string to replace the current string and
continue dispatch, or (c) the matching styles to use and, if
needed, a new string to use in place of the current one (for
example, a dispatcher can decide which style to use based on a
suffix of the string and then it must also return the component
stripped of the suffix).

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
a list of styles.  When that happens it returns a `cons' of the
list of styles and the possibly updated STRING.  If none of the
DISPATCHERS returns a list of styles, the return value will use
DEFAULT as the list of styles."
  (cl-loop for dispatcher in dispatchers
           for result = (funcall dispatcher string index total)
           if (stringp result)
           do (setq string result result nil)
           else if (and (consp result) (null (car result)))
           do (setf (car result) default)
           else if (and (consp result) (stringp (cdr result)))
           do (setq string (cdr result) result (car result))
           when result return (cons result string)
           finally (return (cons default string))))

(defun orderless--compile-component (component index total styles dispatchers)
  "Compile COMPONENT at INDEX of TOTAL components with STYLES and DISPATCHERS."
  (cl-loop
   with pred = nil
   with (newsty . newcomp) = (orderless--dispatch dispatchers styles
                                                  component index total)
   for style in (if (functionp newsty) (list newsty) newsty)
   for res = (condition-case nil
                 (funcall style newcomp)
               (wrong-number-of-arguments
                (when-let ((res (orderless--compile-component
                                 newcomp index total styles dispatchers)))
                  (funcall style (car res) (cdr res)))))
   if (functionp res) do (cl-callf orderless--predicate-and pred res)
   else if res collect (if (stringp res) `(regexp ,res) res) into regexps
   finally return
   (when (or pred regexps)
     (cons pred (and regexps (rx-to-string `(or ,@(delete-dups regexps)) t))))))

(defun orderless-compile (pattern &optional styles dispatchers)
  "Build regexps to match the components of PATTERN.
Split PATTERN on `orderless-component-separator' and compute
matching styles for each component.  For each component the style
DISPATCHERS are run to determine the matching styles to be used;
they are called with arguments the component, the 0-based index
of the component and the total number of components.  If the
DISPATCHERS decline to handle the component, then the list of
matching STYLES is used.  See `orderless--dispatch' for details
on dispatchers.

The STYLES default to `orderless-matching-styles', and the
DISPATCHERS default to `orderless-dipatchers'.  Since nil gets
you the default, if you want no dispatchers to be run, use
\\='(ignore) as the value of DISPATCHERS.

The return value is a pair of a predicate function and a list of
regexps.  The predicate function can also be nil.  It takes a
string as argument."
  (unless styles (setq styles orderless-matching-styles))
  (unless dispatchers (setq dispatchers orderless-style-dispatchers))
  (cl-loop
   with predicate = nil
   with components = (if (functionp orderless-component-separator)
                         (funcall orderless-component-separator pattern)
                       (split-string pattern orderless-component-separator t))
   with total = (length components)
   for comp in components and index from 0
   for (pred . regexp) = (orderless--compile-component
                          comp index total styles dispatchers)
   when regexp collect regexp into regexps
   when pred do (cl-callf orderless--predicate-and predicate pred)
   finally return (cons predicate regexps)))

;;; Completion style implementation

(defun orderless--predicate-normalized-and (p q)
  "Combine two predicate functions P and Q with `and'.
The first function P is a completion predicate which can receive
up to two arguments.  The second function Q always receives a
normalized string as argument."
  (cond
   ((and p q)
    (lambda (k &rest v) ;; v for hash table
      (when (if v (funcall p k (car v)) (funcall p k))
        (setq k (if (consp k) (car k) k)) ;; alist
        (funcall q (if (symbolp k) (symbol-name k) k)))))
   (q
    (lambda (k &optional _) ;; _ for hash table
      (setq k (if (consp k) (car k) k)) ;; alist
      (funcall q (if (symbolp k) (symbol-name k) k))))
   (p)))

(defun orderless--predicate-and (p q)
  "Combine two predicate functions P and Q with `and'."
  (or (and p q (lambda (x) (and (funcall p x) (funcall q x)))) p q))

(defun orderless--compile (string table pred)
  "Compile STRING to a prefix and a list of regular expressions.
The predicate PRED is used to constrain the entries in TABLE."
  (pcase-let* ((limit (car (completion-boundaries string table pred "")))
               (prefix (substring string 0 limit))
               (pattern (substring string limit))
               (`(,fun . ,regexps) (orderless-compile pattern)))
    (list prefix regexps (orderless--ignore-case-p regexps)
          (orderless--predicate-normalized-and pred fun))))

;; Thanks to @jakanakaevangeli for writing a version of this function:
;; https://github.com/oantolin/orderless/issues/79#issuecomment-916073526
(defun orderless--literal-prefix-p (regexp)
  "Determine if REGEXP is a quoted regexp anchored at the beginning.
If REGEXP is of the form \"\\`q\" for q = (regexp-quote u),
then return (cons REGEXP u); else return nil."
  (when (and (string-prefix-p "\\`" regexp)
             (not (string-match-p "[$*+.?[\\^]"
                                  (replace-regexp-in-string
                                   "\\\\[$*+.?[\\^]" "" regexp
                                   'fixedcase 'literal nil 2))))
    (cons regexp
          (replace-regexp-in-string "\\\\\\([$*+.?[\\^]\\)" "\\1"
                                    regexp 'fixedcase nil nil 2))))

(defun orderless--ignore-case-p (regexps)
  "Return non-nil if case should be ignored for REGEXPS."
  (if orderless-smart-case
      (cl-loop for regexp in regexps
               always (isearch-no-upper-case-p regexp t))
    completion-ignore-case))

(defun orderless--filter (prefix regexps ignore-case table pred)
  "Filter TABLE by PREFIX, REGEXPS and PRED.
The matching should be case-insensitive if IGNORE-CASE is non-nil."
  ;; If there is a regexp of the form \`quoted-regexp then
  ;; remove the first such and add the unquoted form to the prefix.
  (pcase (cl-loop for r in regexps
                  thereis (orderless--literal-prefix-p r))
    (`(,regexp . ,literal)
     (setq prefix (concat prefix literal)
           regexps (remove regexp regexps))))
  (let ((completion-regexp-list regexps)
        (completion-ignore-case ignore-case))
    (all-completions prefix table pred)))

(defun orderless-filter (string table &optional pred)
  "Split STRING into components and find entries TABLE matching all.
The predicate PRED is used to constrain the entries in TABLE."
  (pcase-let ((`(,prefix ,regexps ,ignore-case ,pred)
               (orderless--compile string table pred)))
    (orderless--filter prefix regexps ignore-case table pred)))

;;;###autoload
(defun orderless-all-completions (string table pred _point)
  "Split STRING into components and find entries TABLE matching all.
The predicate PRED is used to constrain the entries in TABLE.  The
matching portions of each candidate are highlighted.
This function is part of the `orderless' completion style."
  (pcase-let ((`(,prefix ,regexps ,ignore-case ,pred)
               (orderless--compile string table pred)))
    (when-let ((completions (orderless--filter prefix regexps ignore-case table pred)))
      (if completion-lazy-hilit
          (setq completion-lazy-hilit-fn
                (apply-partially #'orderless--highlight regexps ignore-case))
        (cl-loop for str in-ref completions do
                 (setf str (orderless--highlight regexps ignore-case (substring str)))))
      (nconc completions (length prefix)))))

;;;###autoload
(defun orderless-try-completion (string table pred point)
  "Complete STRING to unique matching entry in TABLE.
This uses `orderless-all-completions' to find matches for STRING
in TABLE among entries satisfying PRED.  If there is only one
match, it completes to that match.  If there are no matches, it
returns nil.  In any other case it \"completes\" STRING to
itself, without moving POINT.
This function is part of the `orderless' completion style."
  (or
   (pcase orderless-expand-substring
     ('nil nil)
     ('prefix (completion-emacs21-try-completion string table pred point))
     (_ (completion-substring-try-completion string table pred point)))
   (catch 'orderless--many
     (pcase-let ((`(,prefix ,regexps ,ignore-case ,pred)
                  (orderless--compile string table pred))
                 (one nil))
       ;; Abuse all-completions/orderless--filter as a fast search loop.
       ;; Should be almost allocation-free since our "predicate" is not
       ;; called more than two times.
       (orderless--filter
        prefix regexps ignore-case table
        (orderless--predicate-normalized-and
         pred
         (lambda (arg)
           ;; Check if there is more than a single match (= many).
           (when (and one (not (equal one arg)))
             (throw 'orderless--many (cons string point)))
           (setq one arg)
           t)))
       (when one
         ;; Prepend prefix if the candidate does not already have the same
         ;; prefix.  This workaround is needed since the predicate may either
         ;; receive an unprefixed or a prefixed candidate as argument.  Most
         ;; completion tables consistently call the predicate with unprefixed
         ;; candidates, for example `completion-file-name-table'.  In contrast,
         ;; `completion-table-with-context' calls the predicate with prefixed
         ;; candidates.  This could be an unintended bug or oversight in
         ;; `completion-table-with-context'.
         (unless (or (equal prefix "")
                     (and (string-prefix-p prefix one)
                          (test-completion one table pred)))
           (setq one (concat prefix one)))
         (or (equal string one) ;; Return t for unique exact match
             (cons one (length one))))))))

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
  (or (mapcar (lambda (x) (cons x t)) (cdr (orderless-compile str))) ""))

(defvar ivy-regex)
(defun orderless-ivy-highlight (str)
  "Highlight a match in STR of each regexp in `ivy-regex'.
This function is for integration of orderless with ivy."
  (orderless--highlight (mapcar #'car ivy-regex) t str) str)

(provide 'orderless)
;;; orderless.el ends here
