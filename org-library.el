




;; === Variables ===

(defvar org-library--index nil "In-memory index of books.")

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
  '("Author" "Genre" "Status" "Series" "Pages" "Isbn" "Asin" "Publisher" "Year" "LibraryTags")
  "All properties that org-library recognizes and provides completion for."
  :type '(repeat string))

(defcustom org-library-fetch-metadata-function #'org-library-fetch-openlibrary
  "Function to fetch metadata for a given ISBN."
  :type 'function)

(defcustom org-library-fetch-metadata-functions
  '(org-library-fetch-openlibrary)
  "List of all metadata functions."
  :type '(repeat function))

(defcustom org-library-display-function #'org-library--display-results-tabulated
  "Function to display book results.
The function should accept a list of book alists as its argument."
  :type 'function)

(defcustom org-library-display-functions
  '(org-library--display-results-org org-library--display-results-tabulated)
  "List of all display functions."
  :type '(repeat function))

(defcustom org-library-export-functions
  '(("csv" . org-library--export-csv)
    ("json" . org-library--export-json))
  "Alist mapping export format names to formatter functions."
  :type '(alist :key-type string :value-type function))

(defcustom org-library-tag-separator ","
  "Separator used when storing multiple values in the tags property."
  :type 'string)

;; === Hooks ===

(defcustom org-library-validation-hook nil
  "Hook run while validating a book entry.
Each function in the hook should accept a single argument: an alist of book properties.
If any function returns nil, the save is aborted."
  :type 'hook
  :group 'org-library)
(add-hook 'org-library-validation-hook #'org-library--validate-required-properties)
(add-hook 'org-library-validation-hook #'org-library--validate-unique-isbn)
(add-hook 'org-library-validation-hook #'org-library--validate-unique-title-author)

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
  (let* ((metadata (when isbn (org-library--fetch-metadata isbn)))
	 (title (or (and metadata (alist-get :title metadata))
		    (read-string "Title: ")))
	 (book-data (append
		     `((:title . ,title))
		     (when isbn
		       `((:isbn . ,(format "%s" isbn))))
		     (when metadata metadata))))
    (let ((result (org-library--add-book book-data)))
      (if result
	  (message "Added book: %s" title)
	(message "Failed to add book: %s" title)))))

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

(defun org-library-search (&optional property value prefix)
  "Search for books in the library.
If PROPERTY and VALUE are provided, search for books with matching property.
With PREFIX argument (C-u), prompt for the display function to use."
  (interactive
   (list (completing-read "Search by property: "
			  (cons "Any" (cons "Title" org-library-supported-properties))
			  nil t)
	 (read-string "Search for value: ")
	 current-prefix-arg))
  (let* ((results (org-library--search property value))
	 (display-fn (if prefix
			 (intern (completing-read "Display function: "
						  (mapcar #'symbol-name org-library-display-functions)
						  nil t))
		       org-library-display-function)))
    (if (called-interactively-p 'any)
	(funcall display-fn results)
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
   
(defun org-library-list (&optional prefix)
  "Display a list of books in the library.
With PREFIX (C-u), prompt for the display function to use."
  (interactive "P")
  (let* ((books (org-library--get-all-books))
	 (display-fn (if prefix
			 (intern (completing-read "Display function: "
						  (mapcar #'symbol-name org-library-display-functions)
						  nil t))
		       org-library-display-function)))
    (funcall display-fn books)))
  
(defun org-library-fetch-metadata (&optional prefix)
  "Fetch/update metadata for book at point using ISBN property.
With PREFIX argument (C-u), prompt for the metadata function to use."
  (interactive "P")
  (unless (org-at-heading-p)
    (user-error "Not on a book heading"))

  (let ((isbn (org-entry-get (point) "ISBN")))
    (unless isbn
      (user-error "No ISBN property found for this book"))

    (let* ((fetch-fn (if prefix
			 (intern (completing-read "Metadata source: "
						  (mapcar #'symbol-name org-library-fetch-metadata-functions)
						  nil t))
		       org-library-fetch-metadata-function))
	   (metadata (org-library--fetch-metadata isbn fetch-fn)))
      (when metadata
	(dolist (item metadata)
	  (let ((prop-name (substring (symbol-name (car item)) 1)))
	    (when (and (not (string= prop-name "title")) ;; Don't overwrite title
		       (member (capitalize prop-name) org-library-supported-properties))
	      (org-set-property (capitalize prop-name) (cdr item)))))
	(message "Updated metadata for '%s'" (substring-no-properties (org-get-heading t t t t)))))))

(defun org-library-bulk-import (books-list &optional skip-existing)
  "Import multiple books from BOOKS-LIST.
Each item in the list should be a book-data alist.
If SKIP-EXISTING is non-nil, silently skip books that would cause duplicate ISBN or ID errors."
  (let ((success-count 0)
	(skip-count 0)
	(error-count 0))
    (dolist (book books-list)
      (condition-case err
	  (let* ((isbn (alist-get :isbn book))
		 (exists (and isbn
			      (not (string-empty-p isbn))
			      (org-library--search "ISBN" isbn))))
	    (if (and exists skip-existing)
		(progn
		  (message "Skipping existing book: %s" (alist-get :title book))
		  (cl-incf skip-count))
		;; Try to add book
		(if (org-library--add-book book)
		    (cl-incf success-count)
		  (cl-incf error-count))))
	;; Catch any errors
	(error
	 (message "Error adding book '%s': %s"
		  (alist-get :title book)
		  (error-message-string err))
	 (cl-incf error-count))))
    (message "Import completed: %d added, %d skipped, %d errors"
	     success-count skip-count error-count)))
  


(defun org-library-export (&optional format)
  "Export library in specified FORMAT."
  (interactive
   (list (completing-read "Export format: " 
                          (mapcar #'car org-library-export-functions) 
                          nil t)))
  (let* ((books (org-library--get-all-books))
         (output-file (read-file-name "Export to: " nil nil nil
                                      (format "library.%s" format)))
	 (formatter (cdr (assoc format org-library-export-functions))))
    (funcall formatter books output-file)
    (message "Library exported to %s" output-file)))


;; === Helpers ===

(defun org-library--build-index ()
  "Build an in-memory index of all books."
  (setq org-library--index
	(make-hash-table :test 'equal :size 1000))
  (let ((books (org-library--get-all-books)))
    (dolist (book books)
      (puthash (alist-get :id book) book org-library--index))))

(defun org-library--get-book-by-id (id)
  "Get a book by its ID"
  (unless org-library--index
    (org-library--build-index))
  (gethash id org-library--index))

(defun org-library--add-book (book-data &optional skip-validation)
  "Add a book to the library using BOOK-DATA alist.
Required keys in BOOK-DATA are :title and optionally :isbn.
If SKIP-VALIDATION it non-nil, bypass validation checks.
Returns the book ID if successful, nil otherwise."
  (let* ((title (alist-get :title book-data))
	 (id (or (alist-get :id book-data)
		 (funcall org-library-id-generator))))
    ;; Ensure we have a title
    (unless title
      (error "Book data must include a title."))

    ;; Add ID to book-data if not present
    (unless (alist-get :id book-data)
      (setq book-data (cons '(:id . ,id) book-data)))

    ;; Validata unless skipping
    (when (and (not skip-validation)
	       (stringp (org-library--validate book-data)))
      (if noninteractive
	  (error "Validation failed for '%s'" title)
	(message "Validation failed for '%s', skipping" title)
	(setq id nil)))
    ;; Only proceed if validation passed or was skipped
    (when id
      ;; Add the book
      (with-current-buffer (find-file-noselect org-library-file)
	(goto-char (point-max))
	(insert (format "\n* %s\n" title))
	(org-back-to-heading)
	(org-insert-property-drawer)

	;; Add ID
	(org-set-property org-library-id-property id)

	;; Add all properties from book-data
	(dolist (prop book-data)
	  (let ((prop-name (substring (symbol-name (car prop)) 1)))
	    (unless (or (string= prop-name "title") ; Skip title, its the heading
			(string= prop-name "id")) ; Already added the id
	      (when (and (cdr prop) (not (string-empty-p (format "%s" (cdr prop)))))
		(org-set-property (capitalize prop-name) (format "%s" (cdr prop)))))))

	;; Add default properties not in book-data
	(dolist (prop org-library-default-properties)
	  (let ((prop-key (intern (concat ":" (downcase prop)))))
	    (unless (alist-get prop-key book-data)
	      (org-set-property prop "unknown"))))

	(save-buffer)
	(org-library--build-index)))
    id))

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

(defun org-library--fetch-metadata (isbn &optional fetch-fn)
  "Fetch book metadata for ISBN using FETCH-FN.
If FETCH-FN is not provided, use 'org-library-fetch-metadata-function'."
  (let ((metadata (funcall (or fetch-fn org-library-fetch-metadata-function) isbn)))
    (if metadata
	(message "Found %s by %s"
		 (alist-get :title metadata)
		 (alist-get :author metadata))
      (message "No metadata found for ISBN %s" isbn))
    metadata))

;; === Validation ===

(defun org-library--validate (book-data)
  (let ((result t))
    (catch 'validation-failed
      (dolist (validator (symbol-value 'org-library-validation-hook))
	(let ((validator-result (funcall validator book-data)))
	  (when (stringp validator-result)
	    ;; Failed validation with error message
	    (setq result validator-result)
	    (throw 'validation-failed nil)))))
    ;; Check result and signal error if needed
    (when (stringp result)
      (user-error "Validation failed: %s" result))
    t))

(defun org-library--validate (book-data &optional interactive)
  "Validata BOOK-DATA against all validation hooks.
If INTERACTIVE is non-nil, show warnings for non-critical issues.
Returns t if valid, or error message string if invalid."
  (let ((critical-errors '())
	(warnings '()))
    ;; Run all validators
    (dolist (validator (symbol-value 'org-library-validation-hook))
      (let ((validator-result (funcall validator book-data)))
	(when (stringp validator-result)
	  ;; Check if this is a critical error
	  (if (or (eq validator #'org-library--validate-required-properties)
		  (eq validator #'org-library--validate-unique-isbn))
	      (push validator-result critical-errors)
	    (push validator-result warnings)))))
    ;; Process results
    (cond
     ;; Critical errors -- always return error
     (critical-errors
      (car critical-errors))
     ;; Warnings in interactive mode - prompt user
     ((and warnings interactive)
      (if (yes-or-no-p (format "Warning: %s. Add anyway? " (car warnings)))
	  t ; User wants to proceed
	(car warnings))) ; User cancelled
     ;; Warnings in non-interactive mode - just proceed
     (warnings
      (message "Warning: %s" (car warnings))
      t)
     ;; No issues
     (t t))))
     
  

(defun org-library--validate-required-properties (book-data)
  "Ensure all required properties exist in BOOK-DATA.
Returns t if valid, or a string with error message if invalid."
  (let ((missing nil))
    (dolist (prop org-library-required-properties)
      (let ((prop-key (intern (concat ":" (downcase prop)))))
	(unless (and (alist-get prop-key book-data)
		     (not (string-empty-p (format "%s" (alist-get prop-key book-data)))))
	  (push prop missing))))
    (if missing
	(format "Missing required properties: %s" (mapconcat #'identity missing ", "))
      t)))

(defun org-library--validate-unique-isbn (book-data)
  "Ensure ISBN is unique if provided.
Returns t if valid, or a string with error message if invalid."
  (let ((isbn (alist-get :isbn book-data))
	(id (alist-get :id book-data)))
    (message "%s" book-data)
    (if (or (not isbn) (string-empty-p isbn))
	t ; No ISBN, so validation passes
      (let ((books (org-library--search "ISBN" isbn)))
	(if (or (not books)
		;; Its ok if the only match is this book (for updates)
		(and (= (length books) 1)
		     (string= (alist-get :id (car books)) (format "%s" id))))
	      t
	  (format "Book with ISBN %s already exists: %s"
		  isbn (alist-get :title (car books))))))))

(defun org-library--validate-unique-title-author (book-data)
  "Check for likely duplicates based on title and author combination.
Returns t if no duplicates found, or a string with warning if possible duplicates exist."
  (let ((title (alist-get :title book-data))
	(author (alist-get :author book-data))
	(id (alist-get :id book-data)))
    (when (and title author)
      (let* ((title-matches (org-library--search "Title" title))
	     (possible-dupes
	      (seq-filter
	       (lambda (book)
		 (and (not (string= (alist-get :id book) (format "%s" id) )) ;; Not the same book
		      (string-match-p (regexp-quote author) (or (alist-get :author book) ""))))
	       title-matches)))
	(if possible-dupes
	    (format "Possible duplicate: \"%s\" by %s"
		    (alist-get :title (car possible-dupes))
		    (alist-get :author (car possible-dupes)))
	  t)))))

;; === Metadata ===
;; Metadata fetchers should return an alist of property-value pairs.
;; In general they should take a book's ISBN as an argument in the form of a string.
;; Properties should be a member of 'org-library-supported-properties'.

;; These will probably have to have the properties they fetch hardcoded in, simply because we have to parse a json response (in most cases)

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
	(:publisher . ,(mapconcat (lambda (publisher)
				    (alist-get 'name publisher))
				  (alist-get 'publishers book-data) ", "))
	))))

;; === Display Functions ===
;; Display functions should take 'results', a list of book alists.

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

;; === Export Functions ===
;; Should take an alist of books, and an output file as arguments.
;; Should add any extra export functions to the 'org-library-export-functions' list.

;; Should update these later to export more dynamic fields.
;; Perhaps a variable to define what fields to export?
(defun org-library--export-csv (books output-file)
  "Export BOOKS as CSV to OUTPUT-FILE."
  (with-temp-file output-file
    (insert "Title,Author,ISBN,Publisher,Year\n")
    (dolist (book books)
      (insert (format "\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"\n"
                      (alist-get :title book "")
                      (alist-get :author book "")
                      (alist-get :isbn book "")
                      (alist-get :publisher book "")
                      (alist-get :year book ""))))))

(defun org-library--export-json (books output-file)
  "Export BOOKS as JSON to OUTPUT-FILE."
  (require 'json)
  (with-temp-file output-file
    (insert (json-encode books))))


;; TODO
;;   1. CSV export currently hardcodes the fields to export. Should export all fields for a headline (probably too complicated to calculate csv headers if I do that, since different books can have different properties) or export a set of fields pre-defined in a variable (perhaps just use org-library-supported-properties, to export /everything/ even if value is null?)


