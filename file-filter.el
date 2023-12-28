
(defvar file-filter-mode-syntax-table nil
  "Syntax table used while in file-filter mode.")

(defvar file-filter-mode-abbrev-table nil
  "Abbrev table used while in file-filter mode.")

(defvar file-filter-mode-map () "Keymap for file-filter mode.")

(defvar-local *file-filter-hash* nil "The global scope hash which contains file names and file sizes.")

(defvar-local *file-filter-hash-total-global* 0 "Total size of all files")

(defvar-local *file-filter-hash-total-filtered* 0 "Filtered total of all files - usually not different from total size.")

(defvar-local *file-filter-hash-totals-extensions 0 "Total of all file-sizes when filtered by extension.")

(defvar-local *file-search-folder-prefix* nil "The parent folder structure above the search point, for insertion in the summary buffer.")

(unless file-filter-mode-syntax-table
      (setq file-filter-mode-syntax-table (make-syntax-table)))

(unless file-filter-mode-map
  (setq file-filter-mode-map (make-sparse-keymap))
  (define-key file-filter-mode-map "n"
	      (lambda (n)
		(interactive "P")
		(let ((current-col (current-column)))
		  (next-line n)
		  (re-search-forward "^" nil t) 
		  (move-to-column (min current-col (current-indentation))))))
  
  (define-key file-filter-mode-map "p"
	      (lambda (n)
		(interactive "P")
		(let ((current-col (current-column)))
		  (previous-line n)
		  (re-search-backward "^" nil t)
		  (move-to-column (min current-col (current-indentation))))))

  (define-key file-filter-mode-map "s" #'file-filter/insert-sorted-file-list)
  (define-key file-filter-mode-map "w" #'file-filter/browse-file-at-point)
  (define-key file-filter-mode-map "f" #'file-filter/goto-entry)
  (define-key file-filter-mode-map "v" #'file-filter/visit-entry)
  (define-key file-filter-mode-map (kbd  "<return>") #'file-filter/goto-entry)
  (define-key file-filter-mode-map (kbd  "<return>") #'file-filter/visit-entry)
  (define-key file-filter-mode-map "d" #'file-filter/delete-entry)
  (define-key file-filter-mode-map "F" #'file-filter/insert-filter-by-extension)
  (define-key file-filter-mode-map (kbd "m") (lambda () (interactive) (setq *file-filter-hash* (file-filter/make-file-hash "~" 50))))
  (define-key file-filter-mode-map [?b] 'file-filter/show-hide-build-buffer)
  )

;;; HELPER FUNCTIONS
(defun file-filter/buffer-name-test ()
  "Say whether the buffer name is *file-filter-search*, as
expected."
  (interactive)
  (if (and (equal (buffer-name) "*file-filter-search*")
	   (equal major-mode 'file-filter-mode))
      t
    nil)
  )

(defun file-filter/get-full-path (path-suffix)
  "Join *file-search-folder-prefix to the path suffix shown in
*file-filter-search* buffer."
  (setq path-suffix (replace-regexp-in-string "^\\.?/?" "" path-suffix))
  (concat (file-name-as-directory *file-search-folder-prefix*) path-suffix)
  )

(defun file-filter/get-path-suffix (full-path)
  "Extract path suffix from *file-search-folder-prefix in
*file-filter-search* buffer."
  (replace-regexp-in-string (regexp-quote *file-search-folder-prefix*) "./" full-path))

(defun file-filter/get-extension ()
  "Open file at point with default program for op system."
  (interactive)
  (let* (
	 (linetext (buffer-substring-no-properties (line-beginning-position)
						   (line-end-position)))
	 (filename (car (split-string linetext "\t")))
	 (extension (car (last (split-string filename "\\.")))))
    (message "%s" extension)))

(defun file-filter/goto-window-with-buffer (buffer-to-go-to)
  "In the current frame, go to the first window found which
has this active buffer."
  (let* (
	 (target-win nil))
    (dolist (win (window-list))
      (when (equal (buffer-name (window-buffer win)) buffer-to-go-to)
	(set target-win win)
	(cl-return)
	)
      )	;; dolist
    (when target-win
      (select-window target-win))
    target-win
    ))

(defun file-filter/sort-hash-keys (&optional hash-to-sort)
  "Sort hash into list of keys in order of value size."
  (interactive)
  (setq hash-to-sort (or hash-to-sort *file-filter-hash*))
  (sort (hash-table-keys hash-to-sort)
	(lambda (k1 k2)
          (> (gethash k1 hash-to-sort)
             (gethash k2 hash-to-sort)))))

(defun file-filter/sum-hash-values (hash-to-add)
  "Add the values of the hash and return the total."
  (let* (
	 (hashkeys (hash-table-keys hash-to-add))
	 (hashkey ())
	 (hashval ())
	 (hashtotal 0)
	 )				; letvars
    (dolist (hashkey hashkeys)
      (setq hashval (gethash hashkey hash-to-add))
      (setq hashtotal (+ hashval hashtotal))
      )
    hashtotal
    )
  )
;; 
;;; GET FILE BUFFERS
(defmacro file-filter/get-file-buffer (buffer-to-get)
  "On entry into file-filter mode, switch to or
create buffer. Ask whether to clear
if it already exists."
  (interactive)
  `(let* (
	  (search-buffer (get-buffer-create ,buffer-to-get))
	  (clear-buffer? nil)
	  (current-win (selected-window))
	  (each-win ())
	  )
     (if (> (buffer-size search-buffer) 0)
	 (setq clear-buffer? (y-or-n-p "Clear-Buffer?") )
       (if clear-buffer? (erase-buffer)))
     (dolist (each-win (window-list))
       (if (equal (window-buffer each-win) search-buffer)
	   (select-window each-win)
	 )
       )
     (unless (equal (window-buffer (selected-window)) ,buffer-to-get)
       (set-window-buffer (selected-window) ,buffer-to-get)
       )
     (switch-to-buffer ,buffer-to-get)))

;;; SEARCH FILTER BUFFER - with list of files
(defun file-filter/get-file-buffer-file-filter-search ()
  "On initialisation of file-filter-mode, make
*file-filter-search* the only buffer."
  (delete-other-windows)
  (file-filter/get-file-buffer "*file-filter-search*")
  )

;;; SUMMARISE FILTER BUFFER - to show key stats for the file filter hash
(defun file-filter/get-file-buffer-summarise-filter ()
  (split-window-below 5)
  (file-filter/get-file-buffer "*file-filter-summarise*")
  (other-window 1)
  )

;;; BUILD FILTER BUFFER - to show progress of file hash being generated as drive is searched.
(defun file-filter/get-file-buffer-build-filter ()
  "Create a buffer to monitor progress in creating a file hash to review."
  (interactive)
  (split-window-right -60)
  (other-window 1)
  (file-filter/get-file-buffer "*file-filter-build*")
  (other-window -1))

(defun file-filter/update-filter-build (text-to-insert)
  "Insert each found file or directory in the build buffer."
  (with-current-buffer "*file-filter-build*"
    (goto-char (point-max))
    (insert text-to-insert)
    (select-window (get-buffer-window (current-buffer)))
    (recenter)))

(defun file-filter/show-hide-build-buffer ()
  "Toggle visibility of *file-filter-build* buffer."
  (interactive)
  (let* (
	 (build-window (get-buffer-window "*file-filter-build*"))
	 )				; letvars
    (if build-window (delete-window build-window)
      (file-filter/get-file-buffer-build-filter)
      )
    ))
;; 

;;; CREATE FILE HASH as Drive is Searched
(defun file-filter/make-file-hash (&optional folderpath minfilesize)
  "Search folder and child folders of supplied path, and add
each file and its size to a file hash if it is above the supplied
minfilesize."
  (interactive (list
		(read-directory-name "Enter a directory to start the search from" (expand-file-name gnus-home-directory) nil nil "~/")
		(read-number "Enter a number in megabytes for the minimum file size: " 50 nil)
		))
  (let* (
	 (thispath (or folx5oderpath *file-search-folder-prefix*))
	 (directoryfiles (condition-case nil
			     (directory-files thispath t "^[^.]+")
			   (t nil)))
	 (filelist ())
	 (filehash (make-hash-table :test 'equal))
	 (subfilehash ())
	 (filesize 0)
	 (hashsize 0)
	 )
    (setq minfilesize (or minfilesize 0))
    (setq *file-filter-hash-total-global* 0)
    (dolist (fl directoryfiles filehash)
      (if (f-directory-p fl)
	  (progn
	    (file-filter/update-filter-build (format "found directory here: %s.\n" fl))
	    (unless (file-symlink-p fl) ; ignore symbolic directories
	      (setq subfilehash (file-filter/make-file-hash fl minfilesize))
	      (unless (or (hash-table-empty-p subfilehash)
			  (not (hash-table-keys subfilehash))
			  (= (hash-table-count subfilehash) 0)
			  (< (file-filter/sum-hash-values subfilehash) minfilesize)
			  )
		(file-filter/update-filter-build (format "%s is a directory with %d files in it\n" fl (hash-table-count subfilehash)))
		(sit-for 0.001)
		(setq filehash (combine-hashtables filehash subfilehash))
		)))			; progn
	(progn
	  (if (file-symlink-p fl)
	      (file-filter/update-filter-build (format "ignoring symbolic file %s" fl)) 
	    (setq filesize (/ (car (nthcdr 7 (file-attributes fl))) 1000.0)))
	  (unless (or (not filesize) (= filesize 0) (< filesize minfilesize))
	    (setq fl (file-filter/get-path-suffix fl))
	    (puthash fl filesize filehash)
	    (setq *file-filter-hash-total-global* (+ filesize *file-filter-hash-total-global*))
	    ))				;progn
	(setq hashsize (file-filter/sum-hash-values filehash))
	))				;dolist
    (if (>= hashsize minfilesize)
	    (file-filter/update-filter-build (format "hash created, with %f files and size %f.\n" (hash-table-count filehash) hashsize)))
    filehash))


;;; INSERT FILE HASH 
(defun file-filter/insert-sorted-file-list (&optional hash-to-sort)
  "Insert file details into buffer, based on sorted-hash."
  (interactive)
  (setq hash-to-sort (or hash-to-sort *file-filter-hash*))
  (if (file-filter/buffer-name-test)
      (with-current-buffer (current-buffer)
	(erase-buffer)
	(goto-char (point-min))
	(dolist (key (file-filter/sort-hash-keys hash-to-sort))
	  (insert (concat key "\t" (number-to-string (gethash key hash-to-sort)) "\n"))
	  )
	) 				; with-current-buffer
    )
  )


(defun file-filter/insert-filter-by-extension (extension-match &optional hash-to-sort)
  "Insert file details into buffer, based on sorted-hash."
  (interactive "splease enter an extension to filter by: ")
  (setq hash-to-sort (or hash-to-sort *file-filter-hash*))
  (setq *file-filter-hash-total-filtered* 0)
  (if (file-filter/buffer-name-test)
      (with-current-buffer (current-buffer)
	(erase-buffer)
	(goto-char (point-min))
	(dolist (key (file-filter/sort-hash-keys hash-to-sort))
	  (let (
		(extension (car (last (split-string key "\\."))))
		(filesize 0)
		)
	    (if (string-match extension-match extension)
		(progn
		  (setq filesize (gethash key *file-filter-hash*))
		  (insert (concat key "\t" (number-to-string filesize) "\n"))
		  (setq *file-filter-hash-total-filtered* (+ filesize *file-filter-hash-total-filtered*))
		  ))			; if
	    ) 				; let
	  )
	(message "total file size is: %fMB" (/ *file-filter-hash-total-filtered* 1000))) 				; with-current-buffer
    ))


(defun file-filter/insert-filter-list ()
  "Insert contents of hash into buffer, no sort, or
order of hash-table keys."
  (interactive)
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (dolist (key (hash-table-keys *file-filter-hash*))
      (insert (concat key "\t" (number-to-string (gethash key *file-filter-hash*)) "\n")))))


;;; ACCESS, EDIT, DELETE INDIVIDUAL FILES
(defun file-filter/delete-entry ()
  "Delete file at point, in *file-filter-search*
buffer."
  (interactive)
  (save-excursion
    (let* (
	   (linetext (buffer-substring-no-properties (line-beginning-position)
						     (line-end-position)))
	   (filename (car (split-string linetext "\t")))
	   (filepath "")
	   (delete-decision nil)
	   )
      (setq filepath (file-filter/get-full-path filename))
      (message "filename is %s" filename)
      (setq delete-decision (y-or-n-p (format  "really delete %s?" filename)))
      (if delete-decision
	  (progn
	    (delete-file filepath nil)
	    (remhash filename *file-filter-hash*)
	    (file-filter/insert-sorted-file-list)
	    ))
      (message "filename %s was %s%s." filename (if delete-decision "" "not ") "deleted")
      ))
  )

(defun file-filter/visit-entry ()
  "Go to file at point for editing in *file-filter-search*
buffer."
  (interactive)
  (let* (
	 (linetext (buffer-substring-no-properties (line-beginning-position)
						   (line-end-position)))
	 (filename (car (split-string linetext "\t")))
	 )
    (setq filename (file-filter/get-full-path filename))
    (message "filename is %s" filename)
    (find-file filename)))

(defun file-filter/goto-entry ()
  "Go to file at point for editing in *file-filter-search*
buffer."
  (interactive)
  (let* (
	 (linetext (buffer-substring-no-properties (line-beginning-position)
						   (line-end-position)))
	 (filename (car (split-string linetext "\t")))
	 )
    (setq filename (file-filter/get-full-path filename))
    (message "filename is %s" filename)
    (find-file-read-only filename)))

(defun file-filter/browse-file-at-point ()
  "Open file at point with default program for op system."
  (interactive)
  (let* (
	 (linetext (buffer-substring-no-properties (line-beginning-position)
						   (line-end-position)))
	 (filename (car (split-string linetext "\t"))))
    (setq filename (file-filter/get-full-path filename))
    (browse-url filename)))


(defun file-filter/choose-file-search-folder-prefix ()
  (interactive)
  (setq *file-search-folder-prefix* (read-string "choose a folder to start the search from: "))
  (setq *file-search-folder-prefix* (expand-file-name *file-search-folder-prefix*))
  (message "press 'm' to start the file search, which should take a few minutes depending on the size of hard drive.")
  )					;

(defun file-filter-mode ()
  "Major mode for viewing files based on a filter,
to do operations on them including delete and copy."
  (interactive)
  (file-filter/get-file-buffer-file-filter-search)
  (file-filter/get-file-buffer-summarise-filter)
  (file-filter/get-file-buffer-build-filter)
  (switch-to-buffer "*file-filter-search*")
  (kill-all-local-variables)
  (with-current-buffer "*file-filter-search*"
    (use-local-map file-filter-mode-map)
    (setq local-abbrev-table file-filter-mode-abbrev-table) 
    (set-syntax-table file-filter-mode-syntax-table)
    (setq mode-name "file-filter")
    (setq major-mode 'file-filter-mode)
    (file-filter/choose-file-search-folder-prefix)
    (run-hooks 'file-filter-hook)))

(provide 'file-filter)
