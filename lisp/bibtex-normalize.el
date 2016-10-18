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
(require 'bibtex-print)

(defun bibtex-normalize/next-entry ()
  "Jump to the beginning of the next bibtex entry."
  ;; search forward for an entry
  (if (re-search-forward bibtex-entry-head nil t)
      ;; go to beginning of the entry
      (bibtex-beginning-of-entry)
    (goto-char (point-max))))

(defun bibtex-normalize/demangle-names (names)
  (mapcar 'bibtex-autokey-demangle-name
          (split-string names "[ \t\n]+and[ \t\n]+")))

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
  (bibtex-print-entry (bibtex-normalize/parse-and-delete-entry))
  (bibtex-beginning-of-entry))

(defun bibtex-normalize-buffer ()
  "Normalize every entry in the current BibTeX buffer."
  (interactive)
  (goto-char (point-min))
  (bibtex-normalize/next-entry)
  (delete-region (point-min) (point))
  (while (< (point) (point-max)) (bibtex-normalize-entry)))

(provide 'bibtex-normalize)
;;; bibtex-normalize.el ends here
