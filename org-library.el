




;; === Variables ===

(defgroup org-library nil
  "Personal library tracking system for Org-mode."
  :group 'org)

(defcustom org-library-file "~/org/books.org"
  "Path to the org-file containing your book entries."
  :type 'file)

(defcustom org-library-id-property "ID"
  "Property used to identify each book uniquely."
  :type 'string)

(defcustom org-library-default-tags '("have" "want" "read")
  "Default tags offered when adding new books."
  :type '(repeat string))

(defcustom org-library-book-entry-template
  '((:title) (:author) (:year) (:publisher))
  "Template controlling which properties are prompted for or displayed."
  :type '(repeat symbol))

(defcustom org-library-fetch-metadata-function #'org-library-fetch-openlibrary
  "Function to fetch metadata for a given ISBN."
  :type 'function)

(defcustom org-library-fetch-metadata-functions
  '(#'org-library-fetch-openlibrary)
  "List of all metadata functions."
  :type '(repeat function))

(defcustom org-library-fetch-metadata-fields
  '(("Field" . "JSON_key"))
  "Fields to fetch from api response."
  :type '(repeat symbol))


;; === Commands ===
(defun org-library-initialize ()
  "Initialize org-library file if it doesn't exist."
  (interactive)
  (unless (file-exists-p org-library-file)
    (with-current-buffer (find-file-noselect org-library-file)
      (insert "#+TITLE: My Book Library\n")
      (insert "#+PROPERTY: header-args :exports both\n")
      (save-buffer)
      (message "Created new library file at %s" org-library-file))))

(defun org-library-remove-book ()
  "Remove a book from the library after confirmation"
  (interactive)
  (let* ((books (org-library--get-all-books))
	 (titles (mapcar (lambda (book) (alist-get :title book)) books))
	 (selected (completing-read "Remove book: " titles nil t))
	 (book (seq-find (lambda (b) (string= (alist-get :title b) selected)) books)))
    (when (and book (yes-or-no-p (format "Really remove '%s'? " selected)))
      (with-current-buffer (find-file-noselect org-library-file)
	(org-id-goto (alist-get :id book))
	(org-mark-subtree)
	(delete-region (region-beginning) (region-end))
	(save-buffer)
	(message "Book removed: %s" selected)))))

;; Search
;; Want to generalize in the future.
;; Search fields are too static currently.
(defun org-library-search (&optional property value)
  "Search for books in the library.
If PROPERTY and VALUE are provided, search for books with matching property."
  (interactive)
  (let* ((props '("Any" "Title" "Author" "ISBN" "Publisher" "Year"))
	 (prop (if property property
		  (completing-read "Search by property: " props nil t)))
	 (val (if value value
		(read-string (format "Search for %s: " (downcase prop)))))
	 (results (org-library--search prop val)))
    (if (called-interactively-p 'any)
	(org-library--display-results results)
      results)))

;; List all books (tabulated list mode?)
(defun org-library-list ()
  "Display a list of all books in the library."
  (interactive)
  (let ((books (org-library--get-all-books)))
    (org-library--display-results books)))

;; Metadata (requires ISBN)
;; ;; Fetch from specified endpoint, if called with universal argument, prompt for endpoint using list of all metadata functions
(defun org-library-fetch-metadata (isbn)
  "Fetch metadata for a book with ISBN."
  (interactive "sISBN: ")
  (let ((metadata (funcall org-library-fetch-metadata-function isbn)))
    (if metadata
	(message "Found %s by %s"
		 (alist-get :title metadata)
		 (alist-get :author metadata))
      (message "No metadata found for ISBN %s" isbn))
    metadata))

;; Export
;; ;; CSV, JSON, others?
;; Generalize in the future using dedicated export functions, similar to metadata approach
(defun org-library-export (format)
  "Export library in specified format (csv or json)."
  (interactive (list (completing-read "Export format: " '("csv" "json") nil t)))
  (let ((books (org-library--get-all-books))
	(output-file (read-file-name "Export to: " nil nil nil
				     (format "library.%s" format))))
    (with-temp-file output-file
      (cond
       ((string= format "csv")
	(insert "Title,Author,ISBN,Publisher,Year\n")
	(dolist (book books)
	  (insert (format "\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"\n"
			  (alist-get :title book "")
			  (alist-get :author book "")
			  (alist-get :isbn book "")
			  (alist-get :publisher book "")
			  (alist-get :year book "")))))
       ((string= format "json")
	(require 'json)
	(insert (json-encode books)))))
    (message "Library exported to %s" output-file)))

;; === Metadata ===

(defun org-library-fetch-openlibrary (isbn)
  "Fetch book metadata from OpenLibrary API for ISBN."
  (let* ((url (format "https://openlibrary.org/api/books?bibkeys=ISBN:%s&format=json&jscmd=data" isbn))
	 (json-object-type 'alist)
	 (json-array-type 'list)
	 (json-key-type 'symbol)
	 (json-response (with-current-buffer
			    (url-retrieve-synchronously url)
			  (goto-char url-http-end-of-headers)
			  (json-read)))
	 (book-data (alist-get (intern (format "ISBN:%s" isbn)) json-response)))
    (when book-data
      `((:title . ,(alist-get 'title book-data))
	(:author . ,(mapconcat (lambda (author)
				 (alist-get 'name author))
			       (alist-get 'authors book-data) ", "))
	(:year . ,(alist-get 'publish_date book-data))
	(:publisher . ,(alist-get 'publishers book-data))))))

;; === Helpers ===

(defun org-library--get-all-books ()
  "Return a list of all books in the library.
Each book is represented as an alist of properties."
  (with-current-buffer (find-file-noselect org-library-file)
    (org-map-entries
     (lambda ()
       (let* ((props (org-entry-properties))
	      (title (substring-no-properties (org-get-heading t t t t)))
	      (id (cdr (assoc org-library-id-property props))))
	 (cons `(:id . ,id)
	       (cons `(:title . ,title)
		     (mapcar (lambda (prop)
			       (cons (intern (concat ":" (downcase (car prop))))
				     (cdr prop)))
			     props)))))
     t 'file)))

(defun org-library--search (property value)
  "Search for books with PROPERTY matching VALUE."
  (let ((books (org-library--get-all-books))
	(property-key (if (string= property "Any")
			  nil
			(intern (concat ":" (downcase property))))))
    (seq-filter
     (lambda (book)
       (if property-key
	   (let ((prop-val (alist-get property-key book)))
	     (and prop-val (string-match-p (regexp-quote value) prop-val)))
	 (seq-some (lambda (prop)
		     (and (stringp (cdr prop))
			  (string-match-p (regexp-quote value) (cdr prop))))
		   book)))
     books)))

;; Tabulated list mode for more functionality?
(defun org-library--display-results (results)
  "Display search RESULTS in a buffer."
  (with-current-buffer (get-buffer-create "*Org Library Results*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Library Search Results\n\n")
      (if results
	  (dolist (book results)
	    (insert (format "* %s\n" (alist-get :title book)))
	    (dolist (prop book)
	      (when (and (not (eq (car prop) :title))
			 (stringp (cdr prop)))
		(insert (format " - %s: %s\n"
				(substring (symbol-name (car prop)) 1)
				(cdr prop)))))
	    (insert "\n"))
	(insert "No matching books found.\n")))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;; TODO
;;    1. Generalize the property setup. Currently, we hardcode the properties we want from the api, export, and search by.
;;       We should allow some level of customizablility there.
;;    2. Generalize the 'export' functionality. We should implement something similar to the metadata fetching, where we call a formatter function.
;;    3. Improve the 'list' functionality. Creating an org-mode list is somewhat useless. We might as well just open the library org file.
