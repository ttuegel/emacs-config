;;; bibtex-print.el --- Pretty-printing BibTeX entries  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Thomas Tuegel

;; Author: Thomas Tuegel <ttuegel@duo>
;; Keywords: bib

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:

(require 'bibtex)
(require 's)

(defun bibtex-print/texify-whitespace (string)
  "Replace extended whitespace as TeX would."
  (replace-regexp-in-string "[ \n\t]+" " " string))

(defconst bibtex-print/header-fields-rx
  (rx "=" (or "type" "key") "="))

(defun bibtex-print/header-fields (name value)
  nil)

(defvar bibtex-print-field-handlers
  (list (cons bibtex-print/header-fields-rx #'bibtex-print/header-fields))
  "The list of handlers to use to print a BibTeX field.

Each handler is a pair of a regular expression and a function that will be
called when the field name matches that expression. The function takes two
arguments, the name and value of the field, and should insert the field at
point.")

(defun bibtex-print-default-field (field)
  (let ((name (car field))
        (value (cdr field)))
    (insert
     "  " (s-downcase name) " = " (bibtex-print/texify-whitespace value) ",\n")))

(defun bibtex-print/run-field-handler (field handler)
  (let* ((handler-rx (car handler))
         (handler-fun (cdr handler))
         (name (car field))
         (value (cdr field))
         (matched (string-match handler-rx name)))
    (when matched
      (funcall handler-fun name value)
      matched)))

(defun bibtex-print-field (field)
  (let* ((handlers bibtex-print-field-handlers) matched)
    (progn
      (while (and (not matched) handlers)
        (setq matched (bibtex-print/run-field-handler field (pop handlers))))
      (unless matched (bibtex-print-default-field field)))))

(defun bibtex-print-entry (entry)
  "Pretty-print a parsed BibTeX entry."
  (let ((type (cdr (assoc "=type=" entry)))
        (key (cdr (assoc "=key=" entry))))
    (when (and type key)
      (insert "@" (s-downcase type) "{" key ",\n")
      (mapc #'bibtex-print-field entry)
      (insert "}\n"))))

(provide 'bibtex-print)
;;; bibtex-print.el ends here
