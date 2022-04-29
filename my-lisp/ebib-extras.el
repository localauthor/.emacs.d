;;; ebib-extras.el --- Extra functions for ebib      -*- lexical-binding: t; -*-

;;; Code:

(require 'ebib)

;;;###autoload
(defun ebib-open (&optional key)
  "Open ebib and set up frame."
  (interactive)
  (if (get-buffer-window "*Ebib-entry*" 'visible)
      (ebib nil key)
    (progn
      (make-frame-on-current-monitor)
      (ebib nil key)
      (set-frame-size (selected-frame) 150 46))))

(defun ebib-smart-quit ()
  "Cancels filter or quits."
  (interactive)
  (ebib--execute-when
    (filtered-db
     (progn
       (ebib-filters-cancel-filter)
       (message "Filters cancelled")))
    (default
      (if (ebib--modified-p)
          (when (yes-or-no-p "Save modified databases and quit? ")
            (progn (ebib--save-database ebib--cur-db)
                   (ebib-quit t)
                   (delete-frame)))
        (when (yes-or-no-p "Quit Ebib? ")
          (progn (ebib-quit t)
                 (delete-frame)))))))

(defun ebib-isbn-search (text)
  (interactive (list
                (if (use-region-p)
                  (buffer-substring
                   (region-beginning)
                   (region-end))
                  (read-string "Search for ISBN: "))))
  (browse-url (format "https://isbnsearch.org/search?s=%s"
                      (url-encode-url text))))

;;;###autoload
(defun ebib-filter-any (input)
  (interactive "MSearch All Fields:")
  (ebib)
  (ebib-db-set-filter `(contains "any" ,input) ebib--cur-db)
  (ebib--update-buffers))


(defun gr/bibtex-all-field-values (field bibs)
  "Return a list all FIELD entries from bibtex files (BIBS)."
  (let ((all-keys nil)
        (table (parsebib-parse bibs)))
    (maphash
     (lambda (_citekey entry)
       (push (cdr (assoc field entry)) all-keys))
     table)
    all-keys))

;; (gr/bibtex-all-field-values "=key=" gr/bibliography)

;; or: (hash-table-keys (parsebib-parse gr/bibliography))
;; or: (progn (ebib) (ebib--list-keys ebib--cur-db))
;; or: (ebib-db-list-keys ebib--cur-db)
;; or: (org-cite-basic--all-keys)

(defun gr/bibtex-generate-autokey ()
  "Generate automatically a key for a BibTeX entry.
Use the author/editor and the year.
Includes duplicate handling."
  (let* ((bibtex-autokey-name-case-convert-function #'capitalize)
         (bibtex-autokey-year-length 4)
         (names (bibtex-autokey-get-names))
         (year (bibtex-autokey-get-year))
         (autokey (concat bibtex-autokey-prefix-string
                          names
                          (unless (or (equal names "")
                                      (equal year ""))
                            bibtex-autokey-name-year-separator)
                          year))
         (suffix ?a)
         (new-key autokey)
         (key))
    (while (member new-key (gr/bibtex-all-field-values
                            "=key="
                            gr/bibliography))
      (setq new-key (concat autokey (list suffix)))
      (setq suffix (1+ suffix))
      (when (eq suffix ?z)
        (setq key (concat key "a"))
        (setq suffix ?a)))
    new-key))

(advice-add 'bibtex-generate-autokey :override #'gr/bibtex-generate-autokey)


;;; ebib-citar

(require 'citar)
(require 'embark)

;;;###autoload
(defun citar-ebib-jump-to-entry (key-entry)
  (interactive (list (citar-select-ref)))
  (ebib-open (car key-entry)))

;;;###autoload
(defun ebib-citar-open ()
  (interactive)
  (let ((key (list (ebib--get-key-at-point))))
    (citar-open (citar--ensure-entries key))))

(defun embark-target-ebib-citar-key-at-point ()
  "Target citar-key of current ebib entry."
  (when (or (derived-mode-p 'ebib-index-mode 'ebib-entry-mode))
    (let ((ebib-key (ebib--get-key-at-point)))
      `(citar-key ,ebib-key))))

(add-to-list 'embark-target-finders 'embark-target-ebib-citar-key-at-point)

(defun embark-target-ebib-citar-key-minibuffer ()
  "Target citar-key in ebib completion candidate.

Note: This target only works on default-completion candidates. It
will therefore work with 'vertico', 'MCT', and others, but not with
'selectrum' or 'ivy', since 'ebib' removes the key from
candidates displayed with those UIs."
  (when (eq embark--command 'ebib-jump-to-entry)
    (let* ((selected (cond ((bound-and-true-p vertico-mode)
                            (cdr (embark--vertico-selected)))
                           (t (thing-at-point 'line t))))
           (ebib-key (car (split-string-and-unquote selected " "))))
      `(citar-key ,ebib-key))))

(add-to-list 'embark-target-finders 'embark-target-ebib-citar-key-minibuffer)

(provide 'ebib-extras)

;;; ebib-extras.el ends here


