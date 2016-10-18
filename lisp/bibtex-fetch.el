;;; bibtex-fetch.el --- Fetch BibTeX entries and documents from common sources  -*- lexical-binding: t; -*-

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
(require 'rx)
(require 'xml)

(defconst bibtex-fetch/arxiv-entry-rx
  (rx string-start
      "http" (opt "s") "://arxiv.org/abs/"
      (submatch (one-or-more (any "A-Z" "a-z" "0-9" "./"))))
  "A regular expression to match the arXiv identifier from a URL.")

(defun bibtex-fetch/arxiv-id-from-url (url)
  (string-match bibtex-fetch/arxiv-entry-rx url)
  (match-string 1 url))

(defun bibtex-fetch/arxiv-query-url (arxiv-id)
  "The URL to GET to fetch bibliographic data for an ARXIV-ID."
  (format "http://export.arxiv.org/api/query?id_list=%s" arxiv-id))

(defun bibtex-fetch/beginning-of-xml ()
  "The point of the beginning of the XML document in the current buffer."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (search-forward "<?xml")
      (match-beginning 0))))

(defun bibtex-fetch/xml-get-child (node name)
  (car (xml-get-children node name)))

(defun bibtex-fetch/year-of-date (date-string)
  "Parse DATE-STRING and return the year."
  (elt (timezone-parse-date date-string) 0))

(defun bibtex-fetch/arxiv-entry-title (entry)
  (caddr (bibtex-fetch/xml-get-child entry 'title)))

(defun bibtex-fetch/arxiv-entry-year (entry)
  (bibtex-fetch/year-of-date
   (caddr (bibtex-fetch/xml-get-child entry 'published))))

(defun bibtex-fetch/arxiv-author-name (author)
  (caddr (bibtex-fetch/xml-get-child author 'name)))

(defun bibtex-fetch/arxiv-entry-authors (entry)
  (mapcar #'bibtex-fetch/arxiv-author-name (xml-get-children entry 'author)))

(defun bibtex-fetch/arxiv-entry-doi (entry)
  (caddr (bibtex-fetch/xml-get-child entry 'arxiv:doi)))

(defun bibtex-fetch/arxiv-entry (url)
  "Fetch the BibTeX info from an arXiv identifier ID."
  (let* ((arxiv-id (match-string 1 url))
         (arxiv-query-url (bibtex-fetch/arxiv-query-url arxiv-id)))
    (with-current-buffer
      (url-retrieve-synchronously arxiv-query-url t)
      (let* ((feed (car (xml-parse-region)))
             (entry (bibtex-fetch/xml-get-child feed 'entry))
             (doi (bibtex-fetch/arxiv-entry-doi entry)))
        (let* ((title (bibtex-fetch/arxiv-entry-title entry))
               (year (bibtex-fetch/arxiv-entry-year entry))
               (authors (bibtex-fetch/arxiv-entry-authors entry)))
          (list (cons "=type=" "article")
                (cons "title" title)
                (cons "year" year)
                (cons "archiveprefix" "{arXiv}")
                (cons "eprint" arxiv-id)
                (cons "author" (s-join " and " authors))))))))

(defvar bibtex-fetch-entry-handlers
  (list (cons bibtex-fetch/arxiv-entry-rx #'bibtex-fetch/arxiv-entry))
  "The list of handlers to use to fetch a BibTeX entry from a URL.

Each handler is a pair of a regular expression and a function that will be
called when the URL matches the regular expression. The function takes no
arguments, but it may assume that `match-data' is set.")

(defun bibtex-fetch/run-entry-handler (url handler)
  (let* ((handler-rx (car handler))
         (handler-fun (cdr handler)))
    (when (string-match handler-rx url)
      (funcall handler-fun url))))

(defun bibtex-fetch-entry (url)
  "Fetch the BibTeX entry for the document at URL."
  (let* ((handlers bibtex-fetch-entry-handlers)
         handler entry)
    (while (and (not entry) (setq handler (pop handlers)))
      (setq entry (bibtex-fetch/run-entry-handler url handler)))
    entry))

(provide 'bibtex-fetch)
;;; bibtex-fetch.el ends here
