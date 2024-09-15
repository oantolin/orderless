;;; orderless-kwd.el --- Keyword dispatcher -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2024

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Provide the `orderless-kwd-dispatch' style dispatcher, which
;; recognizes input of the form `:mod:org' to filter buffers by mode
;; in `switch-to-buffer' or `:on' to only display enabled minor modes
;; in M-x.  The list of supported keywords is configured in
;; `orderless-kwd-alist'.
;;
;; The dispatcher can be enabled by adding it to
;; `orderless-style-dispatchers':
;;
;; (add-to-list 'orderless-style-dispatchers #'orderless-kwd-dispatch)
;;
;; See the customization variables `orderless-kwd-prefix' and
;; `orderless-kwd-separator' in order to configure the syntax.

;;; Code:

(require 'orderless)
(eval-when-compile (require 'cl-lib))

(defcustom orderless-kwd-prefix ?:
  "Keyword dispatcher prefix character."
  :type 'character
  :group 'orderless)

(defcustom orderless-kwd-separator ":="
  "Keyword separator characters."
  :type 'string
  :group 'orderless)

(defcustom orderless-kwd-alist
  `((ann ,#'orderless-annotation)
    (pre ,#'orderless-literal-prefix)
    (cat ,#'orderless-kwd-category)
    (con ,#'orderless-kwd-content)
    (dir ,#'orderless-kwd-directory)
    (fil ,#'orderless-kwd-file)
    (doc ,#'orderless-kwd-documentation)
    (grp ,#'orderless-kwd-group)
    (mod ,#'orderless-kwd-mode)
    (val ,#'orderless-kwd-value)
    (dif ,#'orderless-kwd-modified t)
    (key ,#'orderless-kwd-key t)
    (ro  ,#'orderless-kwd-read-only t)
    (off ,#'orderless-kwd-off t)
    (on  ,#'orderless-kwd-on t))
  "Keyword dispatcher alist.
The list associates a keyword with a matcher function and an
optional boolean flag.  If the flag is non-nil, the matcher acts
as a flag and does not require input."
  :type '(alist :key-type symbol
                :value-type (choice (list function) (list function (const t))))
  :group 'orderless)

(defsubst orderless-kwd--get-buffer (str)
  "Return buffer from candidate STR taking `multi-category' into account."
  (when-let ((cat (get-text-property 0 'multi-category str)))
    (setq str (and (eq (car cat) 'buffer) (cdr cat))))
  (and str (get-buffer str)))

(defsubst orderless-kwd--orig-buffer ()
  "Return the original buffer before miniwindow selection."
  (or (window-buffer (minibuffer-selected-window)) (current-buffer)))

(defun orderless-kwd-category (pred regexp)
  "Match candidate category against PRED and REGEXP."
  (lambda (str)
    (when-let ((cat (car (get-text-property 0 'multi-category str))))
      (orderless--match-p pred regexp (symbol-name cat)))))

(defun orderless-kwd-group (pred regexp)
  "Match candidate group title against PRED and REGEXP."
  (when-let ((fun (compat-call completion-metadata-get
                               (orderless--metadata) 'group-function)))
    (lambda (str)
      (orderless--match-p pred regexp (funcall fun str nil)))))

(defun orderless-kwd-content (_pred regexp)
  "Match buffer content against REGEXP."
  (lambda (str)
    (when-let ((buf (orderless-kwd--get-buffer str)))
      (with-current-buffer buf
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (ignore-errors (re-search-forward regexp nil 'noerror))))))))

(defun orderless-kwd-documentation (pred regexp)
  "Match documentation against PRED and REGEXP."
  (lambda (str)
    (when-let ((sym (intern-soft str)))
      (orderless--match-p
       pred regexp
       (or (ignore-errors (documentation sym))
           (cl-loop
            for doc in '(variable-documentation
                         face-documentation
                         group-documentation)
            thereis (ignore-errors (documentation-property sym doc))))))))

(defun orderless-kwd-key (pred regexp)
  "Match command key binding against PRED and REGEXP."
  (let ((buf (orderless-kwd--orig-buffer)))
    (lambda (str)
      (when-let ((sym (intern-soft str))
                 ((fboundp sym))
                 (keys (with-current-buffer buf (where-is-internal sym))))
        (cl-loop
         for key in keys
         thereis (orderless--match-p pred regexp (key-description key)))))))

(defun orderless-kwd-value (pred regexp)
  "Match variable value against PRED and REGEXP."
  (let ((buf (orderless-kwd--orig-buffer)))
    (lambda (str)
      (when-let ((sym (intern-soft str))
                 ((boundp sym)))
        (let ((print-level 10)
              (print-length 1000))
          (orderless--match-p
           pred regexp (prin1-to-string (buffer-local-value sym buf))))))))

(defun orderless-kwd-off (_)
  "Match disabled minor modes."
  (let ((buf (orderless-kwd--orig-buffer)))
    (lambda (str)
      (when-let ((sym (intern-soft str)))
        (and (boundp sym)
             (memq sym minor-mode-list)
             (not (buffer-local-value sym buf)))))))

(defun orderless-kwd-on (_)
  "Match enabled minor modes."
  (let ((buf (orderless-kwd--orig-buffer)))
    (lambda (str)
      (when-let ((sym (intern-soft str)))
        (and (boundp sym)
             (memq sym minor-mode-list)
             (buffer-local-value sym buf))))))

(defun orderless-kwd-modified (_)
  "Match modified buffers."
  (lambda (str)
    (when-let ((buf (orderless-kwd--get-buffer str)))
      (buffer-modified-p buf))))

(defun orderless-kwd-read-only (_)
  "Match read-only buffers."
  (lambda (str)
    (when-let ((buf (orderless-kwd--get-buffer str)))
      (buffer-local-value 'buffer-read-only buf))))

(defun orderless-kwd-mode (pred regexp)
  "Match buffer mode or bookmark type against PRED and REGEXP."
  (declare-function bookmark-prop-get "bookmark")
  (lambda (str)
    (if-let ((buf (orderless-kwd--get-buffer str)))
        (when-let ((mode (buffer-local-value 'major-mode buf)))
          (or (orderless--match-p pred regexp (symbol-name mode))
              (orderless--match-p pred regexp
                                  (format-mode-line
                                   (buffer-local-value 'mode-name buf)))))
      (when-let ((name (if-let ((cat (get-text-property 0 'multi-category str)))
                           (and (eq (car cat) 'bookmark) (cdr cat))
                         str))
                 (bm (assoc name (bound-and-true-p bookmark-alist)))
                 (handler (or (bookmark-prop-get bm 'handler)
                              'bookmark-default-handler))
                 ((symbolp handler)))
        (orderless--match-p pred regexp
                            (or (get handler 'bookmark-handler-type)
                                (symbol-name handler)))))))

(defun orderless-kwd-directory (pred regexp)
  "Match `default-directory' against PRED and REGEXP."
  (lambda (str)
    (when-let ((buf (orderless-kwd--get-buffer str)))
      (orderless--match-p pred regexp
                          (buffer-local-value 'default-directory buf)))))

(defun orderless-kwd-file (pred regexp)
  "Match `buffer-file-truename' against PRED and REGEXP."
  (lambda (str)
    (when-let ((buf (orderless-kwd--get-buffer str)))
      (orderless--match-p pred regexp
                          (buffer-local-value 'buffer-file-truename buf)))))

;;;###autoload
(defun orderless-kwd-dispatch (component _index _total)
  "Match COMPONENT against the keywords in `orderless-kwd-alist'."
  (when (and (not (equal component ""))
             (= (aref component 0) orderless-kwd-prefix))
    (if-let ((len (length component))
             (pos (or (string-match-p
                       (rx-to-string `(any ,orderless-kwd-separator))
                       component 1)
                      len))
             (sym (intern-soft (substring component 1 pos)))
             (style (alist-get sym orderless-kwd-alist))
             ((or (< (1+ pos) len) (cadr style))))
        (cons (car style) (substring component (min (1+ pos) len)))
      #'ignore)))

(provide 'orderless-kwd)
;;; orderless-kwd.el ends here
