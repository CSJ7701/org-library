;; -*- lexical-binding: t -*-
;;; org-library.el --- Organize and track a personal library in Org-mode  -*- lexical-binding: t; -*-

;; Author: CJ
;; Maintainer: CJ
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (request "0.3.3") (org "9.4"))
;; Keywords: org, reading, books
;; URL: https://github.com/yourusername/org-library

;;; Commentary:

;; org-library helps you organize and track your reading list in Org-mode.
;; You can fetch book metadata, track progress, and update notes.

;;; Code:

(require 'org)
(require 'request)
(require 'cl-lib)

(defgroup org-library nil
  "Settings for the Org-Library package."
  :prefix "org-library-"
  :group 'org)

(defcustom org-library-file "~/org/books.org"
  "The org file where books are stored."
  :type 'file)

(defcustom org-library-status-options '("Unread" "Reading" "Completed" "Abandoned" "Wishlist" "Next")
  "Possible statuses a book can have."
  :type '(repeat string))

(defcustom org-library-progress-format "%d%%"
  "Format for displaying book progress."
  :type 'string)

(defconst org-library-api-recognized-fields
  '("key" "redirects" "title" "subtitle" "alternative_title" "alternative_subtitle"
    "cover_i" "ebook_access" "edition_count" "edition_key" "format" "by_statement"
    "publish_date" "lccn" "lexile" "ia" "oclc" "isbn" "contributor" "publish_place"
    "publisher" "first_sentence" "author_key" "author_name" "author_alternative_name"
    "subject" "person" "place" "time" "has_fulltext" "title_suggest" "publish_year"
    "language" "number_of_pages_median" "ia_count" "publisher_facet" "author_facet"
    "first_publish_year" "ratings_count" "readinglog_count" "want_to_read_count"
    "currently_reading_count" "already_read_count"
    ;; Subjects
    "subject_key" "person_key" "place_key" "time_key"
    ;; Classifications
    "lcc" "ddc" "lcc_sort" "ddc_sort" "osp_count")
  "List of valid OpenLibrary API fields.")

(defcustom org-library-book-fields
  '("title" "author_name" "first_publish_year" "isbn" "subject" "edition_count")
  "List of fields to fetch from OpenLIbrary. Defines the fields created in a new headline. Must be valid API fields, defined in =org-library-api-recognized-fields=."
  :type `(repeat (choice ,@(mapcar (lambda (f) `(const ,f))
				   org-library-api-recognized-fields))))

(defcustom org-library-custom-fields
  '(("STATUS" . "Unread")
    )
  "List of additional properties to be included in book entries, with default value."
  :type '(alist :key-type string :value-type string))

(defcustom org-library-field-to-property
  '(("title" . "TITLE")
    ("author_name" . "AUTHOR")
    ("publish_date" . "PUBLISHED")
    ("isbn" . "ISBN")
    ("subject" . "GENRE"))
  "Mapping from OpenLibrary API fields to Org property names."
  :type '(alist :key-type string :value-type string))

(defgroup org-library-search nil
  "Customization options for OpenLibrary search queries."
  :group 'org-library)

(defcustom org-library-language "en"
  "Preferred language for book searches (ISO 639-1 format)."
  :type 'string
  :group 'org-library-search)

(defcustom org-library-title-search-url
  "https://openlibrary.org/search.json?title=%s"
  "URL template for searching books by title."
  :type 'string
  :group 'org-library-search)

(defcustom org-library-author-search-url
  "https://openlibrary.org/search.json?author=%s"
  "URL template for searching books by author."
  :type 'string
  :group 'org-library-search)

(defcustom org-library-general-search-url
  "https://openlibrary.org/search.json?q=%s"
  "URL template for general books searches."
  :type 'string
  :group 'org-library-search)

(defun org-library-validate-fields ()
  "Check of `org-library-book-fields` contains only recognized fields."
  (dolist (field org-library-book-fields)
    (unless (member field org-library-api-recognized-fields)
      (error "Invalid field in `org-library-book-fields`: %s" field))))

(defun org-library-make-book (&rest properties)
  "Create a book as a hash table with given PROPERTIES.
PROPERTIES should be a plist (e.g. :title \"My Book\" :author \"John Doe\") that overrides defaults.
For any API field missing from the fetched data, the property is set to \"Unknown\"."
  (org-library-validate-fields)
  (let ((book (make-hash-table :test 'equal)))
    (dolist (field org-library-book-fields)
      (let ((org-prop (cdr (assoc field org-library-field-to-property))))
	(when org-prop
	  (puthash org-prop "Unknown" book))))
    (while properties
      (let* ((key (substring (symbol-name (car properties)) 1)) ; Remove leading colon
	     (upkey (upcase key))
	     (val (cadr properties)))
	(puthash upkey val book))
      (setq properties (cddr properties)))
    (dolist (cf org-library-custom-fields)
      (let ((prop (car cf))
	    (defval (cdr cf)))
	(unless (gethash prop book)
	  (puthash prop defval book))))
    book))
	  

(defun org-library-add-book (book)
  "Insert a BOOK (hash table) as an org-mode entry in 'org-library-file'."
  (with-current-buffer (find-file-noselect org-library-file)
    (goto-char (point-max))
    (org-insert-heading t)
    (insert (gethash "TITLE" book))
    (newline)
    (org-set-property "TITLE" (gethash "TITLE" book))
    (maphash (lambda (key value)
	       (unless (string= key "TITLE")
		 (org-set-property key value)))
	     book)
    (save-buffer)))

(defun org-library-get-books ()
  "Return a list of books stored in 'org-library-file'."
  (let (books)
    (with-current-buffer (find-file-noselect org-library-file)
      (org-map-entries
       (lambda ()
	 (let ((book (make-hash-table :test 'equal)))
	   (org-entry-properties nil 'standard) ; ensure properties are up to date
	   (puthash "TITLE" (nth 4 (org-heading-components)) book)
	   (dolist (field org-library-book-fields)
	     (let ((org-prop (cdr (assoc field org-library-field-to-property))))
	       (when org-prop
		 (puthash org-prop (or (org-entry-get nil org-prop) "Unknown") book))))
	   (dolist (cf org-library-custom-fields)
	     (let* ((prop (car cf))
		   (val (or (org-entry-get nil prop) (cdr cf))))
	       (puthash prop val book)))
	   (push book books)))))
    books))

;; === API Search ===

(defun org-library-api-search (query &optional type callback)
  "Search OpenLibrary for QUERY using TYPE ('title, 'author, 'general).
Uses 'org-library-book-fields' for field selection and 'org-library-language' for language.
Calls CALLBACK with the parsed JSON result."
  (org-library-validate-fields)
  (let* ((url-template (pcase type
			 ('title org-library-title-search-url)
			 ('author org-library-author-search-url)
			 ('general org-library-general-search-url)
			 (_ org-library-general-search-url)))
	 (fields (mapconcat #'identity org-library-book-fields ","))
	 (url (format "%s&fields=%s,editions&lang=%s&page=1"
		      (format url-template (url-encode-url query))
		      (url-encode-url fields)
		      org-library-language))

	 (cb callback))
    (request url
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (when cb
		    (funcall cb data))))
      :error (lambda (&rest _)
	       (message "Failed to retrieve data from OpenLibrary.")))))

;; === Display Search Results

(defun org-library-display-search (data)
  "Display book search results in a tabulated list."
  (let ((docs (append (alist-get 'docs data) nil)) ;; Convert vector to list
        (headers [("Relevancy" 10 t)
		  ("Title" 50 t)
		  ("Author" 30 t)
		  ("Year" 4 t)])
        (rows '())
	(index 0))
    (dolist (doc docs)
      (let* ((title (or (alist-get 'title doc) "Unknown"))
	     (authors (alist-get 'author_name doc))
             (author (cond
		      ((vectorp authors) (mapconcat #'identity (append authors nil) ", "))
		      ((listp authors) (mapconcat #'identity authors ", "))
		      (t authors)))
             (year (or (alist-get 'first_publish_year doc) "####"))
	     (year-str (format "%s" year))
	     (relevancy-str (format "%04d" index)))
        (push (list doc (vector relevancy-str title author year-str)) rows))
      (setq index (1+ index)))
    (setq rows (nreverse rows))  ;; Preserve API order
    (with-current-buffer (get-buffer-create "*Library Search Results*")
      (org-library-search-mode)
      (setq tabulated-list-entries rows
            tabulated-list-format headers)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (pop-to-buffer (current-buffer)))))

(defun org-library-search ()
  "Prompt user for search method and query, then display results."
  (interactive)
  (let* ((method (completing-read "Search by: " '("general" "title" "author") nil t))
	 (query (read-string (format "Enter %s search query: " method))))
    (org-library-api-search query (intern method) #'org-library-display-search)))

(define-derived-mode org-library-search-mode tabulated-list-mode "Library Search"
  "Major mode for displaying and interacting with book search results."
  (setq tabulated-list-padding 2)
  (setq tabulated-list-printer #'org-library-search-printer)
  (tabulated-list-init-header))

(defun org-library-search-printer (id cols)
  "Insert a Tabulated List entry at point with custom formatting.
ID is a Lisp object identifying the entry to print, and COLS is a vector
of column descriptors."
  (message "IT WORKED")
  (let ((beg (point))
        (x (max tabulated-list-padding 0))
        (ncols (length tabulated-list-format))
        (inhibit-read-only t)
        (cols-copy (copy-sequence cols))) ; Create a copy of cols to modify
    ;; Padding for first column if needed
    (if (> tabulated-list-padding 0)
        (insert (make-string x ?\s)))
    (let ((tabulated-list--near-rows  ;; For performance: binding the variable for reusability
           (or (bound-and-true-p tabulated-list--near-rows)
               (list (or (tabulated-list-get-entry (pos-bol 0))
                         cols-copy)
                     cols-copy))))
      ;; Apply face properties to the copy
      (when (> (length cols-copy) 0)
        (aset cols-copy 0 (propertize (aref cols-copy 0) 'face '(:foreground "gray6"))))
      (when (> (length cols-copy) 1)
        (aset cols-copy 1 (propertize (aref cols-copy 1) 'face '(:foreground "sky blue"))))
      (when (> (length cols-copy) 2)
        (aset cols-copy 2 (propertize (aref cols-copy 2) 'face '(:foreground "lightcyan4"))))
      (when (> (length cols-copy) 3)
	(aset cols-copy 3 (propertize (aref cols-copy 3) 'face '(:foreground "gray"))))
      ;; Now print using the modified copy
      (dotimes (n ncols)
        (setq x (tabulated-list-print-col n (aref cols-copy n) x))))
    (insert ?\n)
    ;; Add text properties for tabulated list functionality but use the original cols
    ;; to maintain data integrity
    (add-text-properties beg (point)
                         `(tabulated-list-id ,id tabulated-list-entry ,cols))))

(provide 'org-library)

;;; org-library.el ends here


;; TODO
;; -- Add interaction to the search mode
;;    * Should be able to add the selected book
;;    * Rerun search, or change query
;;    * Open the same search in a browser
