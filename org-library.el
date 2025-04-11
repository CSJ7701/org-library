




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

(defcustom org-library-tags-property "LibraryTags"
  "Property used to stoer book tags, separated by commas."
  :type 'string)

(defcustom org-library-id-generator #'org-id-new
  "Function used to generate IDs for new book entries."
  :type 'function)

(defcustom org-library-required-properties '("ID")
  "Properties that must exist for a book entry."
  :type '(repeat string))

(defcustom org-library-default-properties '("Author" "Status")
  "Properties automatically added to new book entries."
  :type '(repeat string))

(defcustom org-library-supported-properties
  '("Author" "Genre" "Status" "Series" "Pages" "ISBN" "Publisher" "Year" "LibraryTags")
  "All properties that org-library recognizes and provides completion for."
  :type '(repeat string))

(defcustom org-library-fetch-metadata-function #'org-library-fetch-openlibrary
  "Function to fetch metadata for a given ISBN."
  :type 'function)

(defcustom org-library-fetch-metadata-functions
  '(#'org-library-fetch-openlibrary)
  "List of all metadata functions."
  :type '(repeat function))

(defcustom org-library-display-function #'org-library--display-results-tabulated
  "Function to display book results.
The function should accept a list of book alists as its argument."
  :type 'function)

(defcustom org-library-display-functions
  "List of all display functions."
  '(#'org-library--display-results-org #'org-library--display-results-tabulated)
  :type '(repeat function))

(defcustom org-library-tag-separator ","
  "Separator used when storing multiple values in the tags property."
  :type 'string)


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

(defun org-library-add-book (&optional isbn)
  "Add a new book to the library.
If ISBN provided, fetch metadata from configured sources."
  (interactive)
  (org-library-initialize)
  (with-current-buffer (find-file-noselect org-library-file)
    (goto-char (point-max))
    (let* ((metadata (when isbn (org-library-fetch-metadata isbn)))
	   (title (or (and metadata (alist-get :title metadata))
		      (read-string "Title: "))))
      ;; Insert headline
      (insert (format "\n* %s\n" title))
      (org-back-to-heading)
      (org-insert-property-drawer)
      ;; Generate and add ID
      (let ((id (funcall org-library-id-generator)))
	(org-set-property org-library-id-property id))
      ;; Add default properties with values from metadata or "unknown"
      (dolist (prop org-library-default-properties)
	(let* ((prop-key (intern (concat ":" (downcase prop))))
	       (value (or (and metadata (alist-get prop-key metadata)) "unknown")))
	  (org-set-property prop value)))
      ;; Add additional metadata properties if available
      (when metadata
	(dolist (item metadata)
	  (let ((prop-name (substring (symbol-name (car item)) 1)))
	    (when (and (not (string= prop-name "title"))
		       (member (capitalize prop-name) org-library-supported-properties))
	      (org-set-property (capitalize prop-name) (cdr item))))))
	(save-buffer)
	(message "Added book: %s" title))))

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

(defun org-library-edit-property ()
  "Edit a property of the current book entry."
  (interactive)
  (let* ((prop (completing-read "Property: " org-library-supported-properties nil t))
	 (current-val (org-entry-get (point) prop))
	 (new-val (read-string (format "%s: " prop) current-val)))
    (org-set-property prop new-val)
    (message "Updated %s to '%s'" prop new-val)))

(defun org-library-search (&optional property value)
  "Search for books in the library.
If PROPERTY and VALUE are provided, search for books with matching property."
  (interactive)
  (let* ((props (cons "Any" (cons "Title" org-library-supported-properties)))
	 (prop (if property property
		  (completing-read "Search by property: " props nil t)))
	 (val (if value value
		(read-string (format "Search for %s: " (downcase prop)))))
	 (results (org-library--search prop val)))
    (if (called-interactively-p 'any)
	(funcall org-library-display-function results)
      results)))

(defun org-library-add-tag (tag)
  "Add TAG to current book entry."
  (interactive (list (completing-read "Tag: " (org-library--get-all-tags) nil nil)))
  (let* ((current (org-entry-get (point) org-library-tags-property))
	 (tags (when current (split-string current org-library-tag-separator)))
	 (new-tags (if (member tag tags) tags (cons tag tags)))
	 (tag-string (mapconcat #'identity new-tags org-library-tag-separator)))
    (org-set-property org-library-tags-property tag-string)
    (message "Tags: %s" tag-string)))

(defun org-library-remove-tag (tag)
  "Remove TAG from the current book entry."
  (interactive
   (list (let ((current-tags (split-string
			      (or (org-entry-get (point) org-library-tags-property) "")
			      org-library-tag-separator)))
	   (completing-read "Remove tag: " current-tags nil t))))
  (let* ((current (org-entry-get (point) org-library-tags-property))
	 (tags (when current (split-string current org-library-tag-separator)))
	 (new-tags (remove tag tags))
	 (tag-string (mapconcat #'identity new-tags org-library-tag-separator)))
    (if (string-empty-p tag-string)
	(org-delete-property org-library-tags-property)
      (org-set-property org-library-tags-property tag-string))
    (message "Tags: %s" (or tag-string "none"))))
   

(defun org-library-list ()
  "Display a list of all books in the library."
  (interactive)
  (let ((books (org-library--get-all-books)))
    (funcall org-library-display-function books)))

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
;; Metadata fetchers should return an alist of property-value pairs.
;; Properties should be a member of 'org-library-supported-properties'.

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
	(property-key (cond
		       ((string= property "Any") nil)
		       ((string= property "Title") :title)
		       (t (intern (concat ":" (downcase property)))))))
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

(defun org-library--get-all-tags ()
  "Get a list of all tags used in the library."
  (let ((books (org-library--get-all-books))
	(tags '()))
    (dolist (book books)
      (let* ((tag-key (intern (concat ":" (downcase org-library-tags-property))))
	     (book-tags (alist-get tag-key book)))
	(when book-tags
	  (dolist (tag (split-string book-tags org-library-tag-separator))
	    (unless (string-empty-p (string-trim tag))
	      (push (string-trim tag) tags))))))
    (seq-uniq tags)))

;; === Display Functions ===

(defun org-library--display-results-org (results)
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

(defun org-library--display-results-tabulated (results)
  "Display RESULTS in a tabulated list buffer."
  (let ((buffer (get-buffer-create "*Org Library Results*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (tabulated-list-mode)
        ;; Determine which columns to display based on available properties
        (let* ((all-props (seq-uniq
                          (mapcan (lambda (book)
                                    (mapcar #'car book))
                                  results)))
               (display-props (seq-filter (lambda (prop)
                                           (memq prop '(:title :author :genre :status)))
                                         all-props))
               (cols (mapcar (lambda (prop)
                               (list (substring (symbol-name prop) 1)
                                     10 'identity))
                             display-props)))
          (setq tabulated-list-format (vconcat cols))
          (setq tabulated-list-entries
                (mapcar (lambda (book)
                          (let ((id (or (alist-get :id book) "unknown"))
                                (values (mapcar (lambda (prop)
                                                 (or (alist-get prop book) ""))
                                               display-props)))
                            (list id (vconcat values))))
                        results)))
        (tabulated-list-init-header)
        (tabulated-list-print t)))
    (display-buffer buffer)))

;; TODO
;;    1. Allow interactive display function selection with universal argument
;;    2. Improve metadata fetching setup. Dedicated function to fetch metadata interactively
;;    3. Allow interactive metadata function selection with universal argument for that interactive metadat function described above
;;    4. Generalize the 'export' functionality. We should implement something similar to the metadata fetching, where we call a formatter function. I'm thinking we define an alist that maps from format name to formatter function, and allow the user to select from that list when calling the 'export' function.

