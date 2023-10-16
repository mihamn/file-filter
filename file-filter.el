
(defvar file-filter-mode-syntax-table nil
  "Syntax table used while in file-filter mode."
)

(unless file-filter-mode-syntax-table
  (setq file-filter-mode-syntax-table (make-syntax-table)))

(defvar file-filter-mode-abbrev-table nil
  "Abbrev table used while in file-filter mode.")

(defvar file-filter-mode-map ()
  "Keymap for file-filter mode."
)

(defvar-local *file-filter-hash* nil)

(defvar-local *file-filter-hash-total-global* 0)

(defvar-local *file-filter-hash-total-filtered* 0)

(defvar-local *file-filter-hash-totals-extensions 0
  "create hash key for every extension, and cumulatively add file sizes to it"
  )

(defun file-filter-make-hash-totals-extensions ()
  
  )

(unless file-filter-mode-map
  (setq file-filter-mode-map (make-sparse-keymap))
  
  (define-key file-filter-mode-map "n"
	      (lambda ()
		(interactive)
		(let ((current-col (current-column)))
		  (next-line)
		  (re-search-forward "^" nil t) 
		  (move-to-column (min current-col (current-indentation))))))
  
  (define-key file-filter-mode-map "p"
	      (lambda ()
		(interactive)
		(let ((current-col (current-column)))
		  (previous-line)
		  (re-search-backward "^" nil t)
		  (move-to-column (min current-col (current-indentation))))))
  )

(defun file-filter/make-file-hash (&optional folderpath minfilesize)
  (interactive)
  (let* (
	 (thispath (or folderpath "~"))
	 (directoryfiles (condition-case nil
			     (directory-files thispath t "^[^.]+")
			   (t nil)
			   ))
	 (filelist ())
	 (filehash (make-hash-table :test 'equal))
	 (subfilehash ())
	 (filesize)
	 )
    (setq minfilesize (or minfilesize 0))
    (setq *file-filter-hash-total-global* 0)
    (dolist (fl directoryfiles filehash)
      (if (f-directory-p fl)
	  (progn
	    (message "found directory here: %s." fl)
	    (unless (file-symlink-p fl) ; ignore symbolic directories
	      (setq subfilehash (mm/file-functions/file-sizes fl 20))
	      (unless (hash-table-empty-p subfilehash)
      		(message "%s is a directory with %d files in it" fl (hash-table-count subfilehash))
		(setq filehash (combine-hashtables filehash subfilehash))
		))
	    )				; progn
	(progn
	  (if (file-symlink-p fl)
	      (message "ignoring symbolic file %s" fl) 
	    (setq filesize (/ (car (nthcdr 7 (file-attributes fl))) 1000.0)))
	  (unless (or (not filesize) (= filesize 0) (< filesize minfilesize))
	    (puthash fl filesize filehash)
	    (setq *file-filter-hash-total-global* (+ filesize *file-filter-hash-total-global*))
	    )
	  )				;progn
	)
      )					;dolist

    filehash))

(define-key file-filter-mode-map (kbd "m") (lambda () (interactive) (setq *file-filter-hash* (file-filter/make-file-hash))))


(defun file-filter/sort-hash-keys (&optional hash-to-sort)
  "Sort hash into list of keys in order of value size."
  (interactive)
  (setq hash-to-sort (or hash-to-sort tophash))
  (sort (hash-table-keys hash-to-sort)
	(lambda (k1 k2)
          (> (gethash k1 hash-to-sort)
             (gethash k2 hash-to-sort))))
  )

(defun file-filter/insert-sorted-file-list (&optional hash-to-sort)
  "Insert file details into buffer, based on sorted-hash."
  (interactive)
  (setq hash-to-sort (or hash-to-sort tophash))
  (if (file-filter/buffer-name-test)
      (with-current-buffer (current-buffer)
	(erase-buffer)
	(goto-char (point-min))
	(dolist (key (file-filter/sort-hash-keys hash-to-sort))
	  (insert (concat key "\t" (number-to-string (gethash key tophash)) "\n"))
	  )
	) 				; with-current-buffer
    )
  )

(define-key file-filter-mode-map "s" #'file-filter/insert-sorted-file-list)

(defun file-filter/insert-filter-by-extension (extension-match &optional hash-to-sort)
  "Insert file details into buffer, based on sorted-hash."
  (interactive "splease enter an extension to filter by: ")
  (setq hash-to-sort (or hash-to-sort tophash))
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
		  (setq filesize (gethash key tophash))
		  (insert (concat key "\t" (number-to-string filesize) "\n"))
		  (setq *file-filter-hash-total-filtered* (+ filesize *file-filter-hash-total-filtered*))
		  ))			; if
	    ) 				; let
	  )
	(message "total file size is: %fMB" (/ *file-filter-hash-total-filtered* 1000))
	) 				; with-current-buffer
    )
  )

(define-key file-filter-mode-map "F" #'file-filter/insert-filter-by-extension)

(defun file-filter/insert-filter-list ()
  "Insert contents of hash into buffer, no sort."
  (interactive)
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (dolist (key (hash-table-keys tophash))
      (insert (concat key "\t" (number-to-string (gethash key tophash)) "\n"))
      )
    )
  )

(defun file-filter/delete-entry ()
  "Delete file at point"
  (interactive)
  (save-excursion
    (let* (
	   (linetext (buffer-substring-no-properties (line-beginning-position)
						     (line-end-position)))
	   (filename (car (split-string linetext "\t")))
	   (delete-decision nil)
	   )
      (message "filename is %s" filename)
      (setq delete-decision (y-or-n-p (format  "really delete %s?" filename)))
      (if delete-decision
	  (progn
	    (delete-file filename nil)
	    (remhash filename tophash)
	    (file-filter/insert-sorted-file-list)
	    ))
      (message "filename %s was %s%s." filename (if delete-decision "" "not ") "deleted")
      ))
  )

(define-key file-filter-mode-map "d" #'file-filter/delete-entry)

(defun file-filter/goto-entry ()
  "Go to file at point"
  (interactive)
  (let* (
	 (linetext (buffer-substring-no-properties (line-beginning-position)
						   (line-end-position)))
	 (filename (car (split-string linetext "\t")))
	 )
    (message "filename is %s" filename)
    (find-file filename)
    )
  )

(define-key file-filter-mode-map "f" #'file-filter/goto-entry)
(define-key file-filter-mode-map (kbd  "<return>") #'file-filter/goto-entry)

;;; TO DO


;;; 1. Sort lines by size (if cursor is on a filesize)

;;; 2. Sort lines by filename (if cursor is on a filename)

;;; Each operation - save the filename selected, to reselect it after the sort.


(defun file-filter/buffer-name-test ()
  "Say whether the buffer name is *file-buffer-search*, as
expected."
  (interactive)
  (if (and (equal (buffer-name) "*file-buffer-search*")
	   (equal major-mode 'file-filter-mode))
      t
      nil)
  )

(defun file-filter/get-file-buffer-search-buffer ()
  "On entry into file-filter mode, switch to or
create *file-buffer-search*. Ask whether to clear
if it already exists."
  (interactive)
  (let* (
	 (search-buffer (get-buffer-create "*file-buffer-search*"))
	 (clear-buffer? nil)
	 (current-win (selected-window))
	 (each-win ())
	 )
    (if (> (buffer-size search-buffer) 0)
	(setq clear-buffer? (y-or-n-p "Clear-Buffer?") )
      (if clear-buffer? (erase-buffer))
      )
    (dolist (each-win (window-list))
      (if (equal (window-buffer each-win) search-buffer)
	  (select-window each-win)
	)
      )
    (unless (equal (window-buffer (selected-window)) "*file-buffer-search*")
      (set-window-buffer (selected-window) "*file-buffer-search*")
      )
    )
  )

(defun file-filter/browse-file-at-point ()
  "Open file at point with default program for op system."
  (interactive)
  (let* (
	 (linetext (buffer-substring-no-properties (line-beginning-position)
						   (line-end-position)))
	 (filename (car (split-string linetext "\t"))))
    (browse-url filename)
    )
  )

(define-key file-filter-mode-map "w" #'file-filter/browse-file-at-point)


(defun file-filter/get-extension ()
  "Open file at point with default program for op system."
  (interactive)
  (let* (
	 (linetext (buffer-substring-no-properties (line-beginning-position)
						   (line-end-position)))
	 (filename (car (split-string linetext "\t")))
	 (extension (car (last (split-string filename "\\."))))
	 )
    (message "%s" extension)
    )
  )


(defun file-filter-mode ()
  "Major mode for viewing files based on a filter,
to do operations on them including delete and copy."
  (interactive)
  (file-filter/get-file-buffer-search-buffer)
  (kill-all-local-variables)
  (use-local-map file-filter-mode-map)
  (setq local-abbrev-table file-filter-mode-abbrev-table)
  (set-syntax-table file-filter-mode-syntax-table)
  (setq mode-name "file-filter")
  (setq major-mode 'file-filter-mode)
  (run-hooks 'file-filter-hook)
  )


