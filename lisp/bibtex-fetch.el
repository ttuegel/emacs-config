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
(require 'helm-utils)
(require 'rx)
(require 'select)
(require 'xml)

(defun bibtex-fetch/url-retrieve-callback (status callback cbargs)
  (let ((ready t))
    (progn
      (while status
        (pcase (pop status)
          (`(:redirect ,redir)
           (progn
             (setq ready nil)
             (bibtex-fetch/url-retrieve redir callback cbargs)))
          (`(:error (,err ,data))
           (progn
             (setq ready nil)
             (signal err data)))
          (_ nil)))
      (when ready (apply callback cbargs)))))


(defun bibtex-fetch/url-retrieve (url callback &optional cbargs)
  "Asynchronously retrieve URL and then CALLBACK.

HTTP redirects are processed automatically. CALLBACK is not called if errors
occur."
  (url-retrieve url #'bibtex-fetch/url-retrieve-callback (list callback cbargs) t))

(defun bibtex-fetch/remove-delimiters (s)
  "Remove the outer-most string delimiters around a BibTeX field."
  (s-chop-suffix "\""
  (s-chop-suffix "}"
  (s-chop-prefix "{"
  (s-chop-prefix "\"" s)))))

(defun bibtex-fetch/names (entry)
  "Get contents of the name field of ENTRY.
Do some modifications based on `bibtex-autokey-name-change-strings'.
Return the names as a concatenated string obeying `bibtex-autokey-names'
and `bibtex-autokey-names-stretch'."
  (let ((names (bibtex-fetch/remove-delimiters
                (cdr (or (assoc "author" entry)
                         (assoc "editor" entry))))))
    ;; Some entries do not have a name field.
    (progn
      (dolist (pattern bibtex-autokey-name-change-strings)
        (setq names (replace-regexp-in-string (car pattern) (cdr pattern)
                                              names t)))
      (if (string= "" names)
          names
        (let* ((case-fold-search t)
               (name-list (mapcar 'bibtex-autokey-demangle-name
                                  (split-string names "[ \t\n]+and[ \t\n]+")))
               additional-names)
          (unless (or (not (numberp bibtex-autokey-names))
                      (<= (length name-list)
                          (+ bibtex-autokey-names
                             bibtex-autokey-names-stretch)))
            ;; Take `bibtex-autokey-names' elements from beginning of name-list
            (setq name-list (nreverse (nthcdr (- (length name-list)
                                                 bibtex-autokey-names)
                                              (nreverse name-list)))
                  additional-names bibtex-autokey-additional-names))
          (concat (mapconcat 'identity name-list
                             bibtex-autokey-name-separator)
                  additional-names))))))

(defun bibtex-fetch/year (entry)
  "Return year field contents as a string obeying `bibtex-autokey-year-length'."
  (let ((yearfield (bibtex-fetch/remove-delimiters
                    (cdr (assoc "year" entry)))))
    (substring yearfield (max 0 (- (length yearfield)
                                   bibtex-autokey-year-length)))))

(defun bibtex-fetch/title (entry)
  "Get title field contents up to a terminator.
Return the result as a string"
  (let ((case-fold-search t)
        (titlestring (bibtex-fetch/remove-delimiters
                      (cdr (assoc "title" entry)))))
    (progn
      (dolist (pattern bibtex-autokey-titleword-change-strings)
        (setq titlestring (replace-regexp-in-string (car pattern) (cdr pattern)
                                                    titlestring t)))
      ;; ignore everything past a terminator
      (if (string-match bibtex-autokey-title-terminators titlestring)
          (setq titlestring (substring titlestring 0 (match-beginning 0))))
      ;; gather words from titlestring into a list.  Ignore
      ;; specific words and use only a specific amount of words.
      (let ((counter 0)
            (ignore-re (concat "\\`\\(?:"
                               (mapconcat 'identity
                                          bibtex-autokey-titleword-ignore "\\|")
                               "\\)\\'"))
            titlewords titlewords-extra word)
        (while (and (or (not (numberp bibtex-autokey-titlewords))
                        (< counter (+ bibtex-autokey-titlewords
                                      bibtex-autokey-titlewords-stretch)))
                    (string-match "\\b\\w+" titlestring))
          (setq word (match-string 0 titlestring)
                titlestring (substring titlestring (match-end 0)))
          ;; Ignore words matched by one of the elements of
          ;; `bibtex-autokey-titleword-ignore'.  Case is significant.
          (unless (let (case-fold-search)
                    (string-match ignore-re word))
            (setq counter (1+ counter))
            (if (or (not (numberp bibtex-autokey-titlewords))
                    (<= counter bibtex-autokey-titlewords))
                (push word titlewords)
              (push word titlewords-extra))))
        ;; Obey `bibtex-autokey-titlewords-stretch':
        ;; If by now we have processed all words in titlestring, we include
        ;; titlewords-extra in titlewords.  Otherwise, we ignore titlewords-extra.
        (unless (string-match "\\b\\w+" titlestring)
          (setq titlewords (append titlewords-extra titlewords)))
        (mapconcat 'bibtex-autokey-demangle-title (nreverse titlewords)
                   bibtex-autokey-titleword-separator)))))

(defun bibtex-fetch/generate-key (entry)
  "Generate automatically a key for a BibTeX entry.
Use the author/editor, the year and the title field.
The algorithm works as follows.

The name part:
 1. Use the author or editor field to generate the name part of the key.
    Expand BibTeX strings if `bibtex-autokey-expand-strings' is non-nil.
 2. Change the content of the name field according to
    `bibtex-autokey-name-change-strings' (see there for further detail).
 3. Use the first `bibtex-autokey-names' names in the name field.  If there
    are up to `bibtex-autokey-names' + `bibtex-autokey-names-stretch' names,
    use all names.
 4. Use only the last names to form the name part.  From these last names,
    take at least `bibtex-autokey-name-length' characters (truncate only
    after a consonant or at a word end).
 5. Convert all last names using the function
    `bibtex-autokey-name-case-convert-function'.
 6. Build the name part of the key by concatenating all abbreviated last
    names with the string `bibtex-autokey-name-separator' between any two.
    If there are more names in the name field than names used in the name
    part, append the string `bibtex-autokey-additional-names'.

The year part:
 1. Build the year part of the key by truncating the content of the year
    field to the rightmost `bibtex-autokey-year-length' digits (useful
    values are 2 and 4).
 2. If the year field (or any other field required to generate the key)
    is absent, but the entry has a valid crossref field and
    `bibtex-autokey-use-crossref' is non-nil, use the field of the
    crossreferenced entry instead.

The title part
 1. Change the content of the title field according to
    `bibtex-autokey-titleword-change-strings' (see there for further detail).
 2. Truncate the title before the first match of
    `bibtex-autokey-title-terminators' and delete those words which appear
    in `bibtex-autokey-titleword-ignore'.  Build the title part using the
    first `bibtex-autokey-titlewords' words from this truncated title.
    If the truncated title ends after up to `bibtex-autokey-titlewords' +
    `bibtex-autokey-titlewords-stretch' words, use all words from the
    truncated title.
 3. For every title word that appears in `bibtex-autokey-titleword-abbrevs'
    use the corresponding abbreviation (see documentation of this variable
    for further detail).
 4. From every title word not generated by an abbreviation, take at least
    `bibtex-autokey-titleword-length' characters (truncate only after
    a consonant or at a word end).
 5. Convert all title words using the function
    `bibtex-autokey-titleword-case-convert-function'.
 6. Build the title part by concatenating all abbreviated title words with
    the string `bibtex-autokey-titleword-separator' between any two.

Concatenate the key:
 1. Concatenate `bibtex-autokey-prefix-string', the name part, the year
    part and the title part.  If the name part and the year part are both
    non-empty insert `bibtex-autokey-name-year-separator' between the two.
    If the title part and the year (or name) part are non-empty, insert
    `bibtex-autokey-year-title-separator' between the two.
 2. If `bibtex-autokey-before-presentation-function' is non-nil, it must be
    a function taking one argument.  Call this function with the generated
    key as the argument.  Use the return value of this function (a string)
    as the key.
 3. If `bibtex-autokey-edit-before-use' is non-nil, present the key in the
    minibuffer to the user for editing.  Insert the key given by the user."
  (let* ((names (bibtex-fetch/names entry))
         (year (bibtex-fetch/year entry))
         (title (bibtex-fetch/title entry))
         (autokey (concat bibtex-autokey-prefix-string
                          names
                          (unless (or (equal names "")
                                      (equal year ""))
                            bibtex-autokey-name-year-separator)
                          year
                          (unless (or (and (equal names "")
                                           (equal year ""))
                                      (equal title ""))
                            bibtex-autokey-year-title-separator)
                          title)))
    (if bibtex-autokey-before-presentation-function
        (funcall bibtex-autokey-before-presentation-function autokey)
      autokey)))

(defconst bibtex-fetch/arxiv-rx
  (rx string-start
      "http" (opt "s") "://arxiv.org/abs/"
      (submatch (one-or-more (any "A-Z" "a-z" "0-9" "./"))))
  "A regular expression to match the arXiv identifier from a URL.")

(defun bibtex-fetch/arxiv-query-url (arxiv-id)
  "The URL to GET to fetch bibliographic data for an ARXIV-ID."
  (s-concat "http://export.arxiv.org/api/query?id_list=" arxiv-id))

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
  "Get the title of a parsed (XML) arXiv ENTRY."
  (let ((title (caddr (bibtex-fetch/xml-get-child entry 'title))))
    (s-concat "{" title "}")))

(defun bibtex-fetch/arxiv-entry-year (entry)
  "Get the year of a parsed (XML) arXiv ENTRY."
  (let ((year (bibtex-fetch/year-of-date
               (caddr (bibtex-fetch/xml-get-child entry 'published)))))
    (s-concat "{" year "}")))

(defun bibtex-fetch/arxiv-author-name (author)
  (caddr (bibtex-fetch/xml-get-child author 'name)))

(defun bibtex-fetch/arxiv-entry-authors (entry)
  "Get the authors of a parsed (XML) arXiv ENTRY."
  (let ((authors (mapcar #'bibtex-fetch/arxiv-author-name
                         (xml-get-children entry 'author))))
    (s-concat "{" (s-join " and " authors) "}")))

(defun bibtex-fetch/arxiv-entry-doi (entry)
  "Get the DOI of a parsed (XML) arXiv ENTRY."
  (caddr (bibtex-fetch/xml-get-child entry 'arxiv:doi)))

(defun bibtex-fetch/arxiv-entry (url)
  "Fetch the BibTeX info from an arXiv URL."
  (let* ((arxiv-id (match-string 1 url))
         (arxiv-query-url (bibtex-fetch/arxiv-query-url arxiv-id)))
    (with-current-buffer
        (url-retrieve-synchronously arxiv-query-url t)
      (let* ((feed (car (xml-parse-region)))
             (entry (bibtex-fetch/xml-get-child feed 'entry))
             (doi (bibtex-fetch/arxiv-entry-doi entry))
             (title (bibtex-fetch/arxiv-entry-title entry))
             (year (bibtex-fetch/arxiv-entry-year entry))
             (authors (bibtex-fetch/arxiv-entry-authors entry))
             (bib (list (cons "=type=" "article")
                        (cons "author" authors)
                        (cons "title" title)
                        (cons "year" year)
                        (cons "archiveprefix" "{arXiv}")
                        (cons "eprint" (s-concat "{" arxiv-id "}"))
                        (cons "url" (s-concat "{" url "}")))))
        (add-to-list 'bib (cons "=key=" (bibtex-fetch/generate-key bib)))))))

(defun bibtex-fetch/retrieve-bibtex (url)
  "Retrieve a BibTeX entry from URL."
  (let ((url-mime-accept-string "text/bibliography;style=bibtex, application/x-bibtex"))
    (with-current-buffer
        (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (save-match-data (re-search-forward bibtex-entry-head))
      (bibtex-fetch/parse-entry))))

(defconst bibtex-fetch/doi-rx
  (rx string-start
      "http" (opt "s") "://" (opt "dx.") "doi.org/"
      (submatch (one-or-more (any "A-Z" "a-z" "0-9" "./"))))
  "A regular expression to match the DOI from a URL.")

(defun bibtex-fetch/doi-query-url (doi)
  (s-concat "https://doi.org/" doi))

(defun bibtex-fetch/crossref-doi-query-url (doi)
  (format
   "http://crosscite.org/citeproc/format?doi=%s&style=bibtex&lang=en-US"
   doi))

(defun bibtex-fetch/doi-entry (url)
  "Fetch the BibTeX info from an DOI URL."
  (let* ((doi (match-string 1 url))
         (entry
          (or (bibtex-fetch/retrieve-bibtex (bibtex-fetch/doi-query-url doi))
              (bibtex-fetch/retrieve-bibtex
               (bibtex-fetch/crossref-doi-query-url doi))))
         (key-cell (assoc "=key=" entry))
         (new-key (bibtex-fetch/generate-key entry)))
    (setcdr key-cell new-key)
    entry))

(defvar bibtex-fetch-entry-handlers
  (list (cons bibtex-fetch/arxiv-rx #'bibtex-fetch/arxiv-entry)
        (cons bibtex-fetch/doi-rx #'bibtex-fetch/doi-entry))
  "The list of handlers to use to fetch a BibTeX entry from a URL.

Each handler is a pair of a regular expression and a function that will be
called when the URL matches the regular expression. The function takes no
arguments, but it may assume that `match-data' is set.")

(defun bibtex-fetch/run-entry-handler (url handler)
  (let* ((handler-rx (car handler))
         (handler-fun (cdr handler)))
    (when (string-match handler-rx url)
      (funcall handler-fun url))))

(defun bibtex-fetch-entry-from-url (url)
  "Fetch the BibTeX entry for the document at URL."
  (interactive "MURL: ")
  (let* ((handlers bibtex-fetch-entry-handlers) handler entry)
    (while (and (not entry) (setq handler (pop handlers)))
      (setq entry (bibtex-fetch/run-entry-handler url handler)))
    (bibtex-print-entry entry)))

(defun bibtex-fetch-entry ()
  "Fetch the BibTeX entry for the URL on the system clipboard."
  (interactive)
  (bibtex-fetch-entry-from-url (gui-get-selection)))

(defun bibtex-fetch/arxiv-document-url (id)
  "The URL of the document associated with arXiv identifier ID."
  (s-concat "https://arxiv.org/pdf/" id))

(defun bibtex-fetch/arxiv-document-callback (dest)
  (write-file dest)
  (kill-buffer))

(defun bibtex-fetch/arxiv-document (url dest)
  "Fetch the document (PDF) corresponding to an arXiv URL and write it to DEST."
  (let* ((arxiv-id (match-string 1 url))
         (arxiv-pdf-url (bibtex-fetch/arxiv-document-url arxiv-id)))
    (bibtex-fetch/url-retrieve arxiv-pdf-url #'bibtex-fetch/arxiv-document-callback (list dest))))

(defvar bibtex-fetch-document-handlers
  (list (cons bibtex-fetch/arxiv-rx #'bibtex-fetch/arxiv-document))
  "The handlers used to fetch a document from a URL stored in a BibTeX entry.

Each handler is a pair of a regular expression and a function that will be
called when the URL matches the regular expression. The function takes two
arguments, the URL and the destination for the file.")

(defun bibtex-fetch/run-document-handler (url dest handler)
  (let* ((handler-rx (car handler))
         (handler-fun (cdr handler))
         (final-dest (expand-file-name dest)))
    (when (string-match handler-rx url)
      (make-directory (file-name-directory final-dest) t)
      (funcall handler-fun url final-dest)
      t)))

(defun bibtex-fetch/parse-entry ()
  "Parse the BibTeX entry at point.

If point is inside or at the beginning of an entry, parse and return that entry.
Restore point when finished."
  (save-excursion
    (bibtex-beginning-of-entry)
    (bibtex-parse-entry)))

(defun bibtex-fetch-document ()
  "Fetch the document corresponding to the BibTeX entry at point."
  (interactive)
  (let* ((entry (bibtex-fetch/parse-entry))
         (url (bibtex-fetch/remove-delimiters
               (cdr (assoc "url" entry))))
         (key (cdr (assoc "=key=" entry)))
         (dest (s-concat "doc/" key ".pdf"))
         (handlers bibtex-fetch-document-handlers) matched)
    (while (and (not matched) handlers)
      (setq matched
            (bibtex-fetch/run-document-handler url dest (pop handlers))))))

(defun bibtex-capture ()
  (interactive)
  (progn
    (bibtex-fetch-entry)
    (bibtex-fetch-document)))

(defun bibtex-open-document ()
  "Open the document associated with the BibTeX entry at point."
  (interactive)
  (let* ((entry (bibtex-fetch/parse-entry))
         (key (cdr (assoc "=key=" entry)))
         (document (expand-file-name (s-concat "doc/" key ".pdf"))))
    (if (file-readable-p document)
        (helm-open-file-with-default-tool document)
      (message "Could not open %s" document))))

(defun bibtex-open-url ()
  "Open the URL associated with the BibTeX entry at point."
  (interactive
   (let* ((entry (bibtex-fetch/parse-entry))
          (url (bibtex-fetch/remove-delimiters
                (cdr (assoc "url" entry)))))
     (if url
         (browse-url url)
       (message "No URL for this entry.")))))

(bind-key "C-c o" #'bibtex-open-document bibtex-mode-map)
(bind-key "C-c M-o" #'bibtex-open-url bibtex-mode-map)
(bind-key "C-c C-c" #'bibtex-capture bibtex-mode-map)

(provide 'bibtex-fetch)
;;; bibtex-fetch.el ends here
