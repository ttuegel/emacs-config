;;; bibtex-normalize.el --- Strongly normalize BibTeX entries  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Thomas Tuegel

;; Author: Thomas Tuegel <ttuegel@duo>
;; Keywords: bib, abbrev

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

;;

;;; Code:

(require 'bibtex)
(require 's)

(defun bibtex-normalize/texify-whitespace (string)
  "Replace extended whitespace as TeX would."
  (replace-regexp-in-string "[ \n\t]+" " " string))

(defun bibtex-normalize/pprint-field (item)
  (let ((field (car item))
        (value (cdr item)))
    (unless (or (equal field "=type=")
                (equal field "=key="))
      (insert "  " (s-downcase field)
              " = " (bibtex-normalize/texify-whitespace value)
              ",\n"))))

(defun bibtex-normalize/pprint-entry (entry)
  "Pretty-print a parsed BibTeX entry."
  (let ((type (cdr (assoc "=type=" entry)))
        (key (cdr (assoc "=key=" entry))))
    (when (and type key)
      (insert "@" (s-downcase type) "{" key ",\n")
      (mapc #'bibtex-normalize/pprint-field entry)
      (insert "}\n"))))

(defun bibtex-normalize/next-entry ()
  "Jump to the beginning of the next bibtex entry."
  ;; search forward for an entry
  (if (re-search-forward bibtex-entry-head nil t)
      ;; go to beginning of the entry
      (bibtex-beginning-of-entry)
    (goto-char (point-max))))

(defun bibtex-normalize/parse-and-delete-entry ()
  "Parse a BibTeX entry at point and then delete it."
  (let* ((start (point))
         (entry (bibtex-parse-entry))
         (end (progn
                (bibtex-normalize/next-entry)
                (point))))
    (delete-region start end)
    entry))

(defun bibtex-normalize-entry ()
  "Normalize the BibTeX entry at point."
  (bibtex-normalize/pprint-entry (bibtex-normalize/parse-and-delete-entry)))

(defun bibtex-normalize-buffer ()
  "Normalize every entry in the current BibTeX buffer."
  (interactive)
  (goto-char (point-min))
  (bibtex-normalize/next-entry)
  (delete-region (point-min) (point))
  (while (< (point) (point-max)) (bibtex-normalize-entry)))

(provide 'bibtex-normalize)
;;; bibtex-normalize.el ends here
