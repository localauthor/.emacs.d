;;; ebib-extras.el --- Extra functions for ebib      -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ebib)
(require 'citar)
(defvar gr/bibliography)

;;;###autoload
(defun ebib-open (&optional key)
  "Open ebib and set up frame.
Accepts optional KEY to go to entry."
  (interactive)
  (if (get-buffer-window "*Ebib-entry*" 'visible)
      (ebib nil key)
    (progn
      (make-frame-on-current-monitor)
      (ebib nil key)
      (set-frame-size (selected-frame) 110 46))))

(defun ebib-smart-quit ()
  "Cancel filter or quit."
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

;;;###autoload
(defun ebib-isbn-web-search (string)
  "Search 'isbnsearch.org' for ISBN associated with STRING."
  (interactive (list
                (if (use-region-p)
                  (buffer-substring
                   (region-beginning)
                   (region-end))
                  (read-string "Search for ISBN: "))))
  (browse-url (format "https://isbnsearch.org/search?s=%s"
                      (url-encode-url string))))

;;;###autoload
(defun ebib-filter-any (string)
  "Search all fields for STRING."
  (interactive "MSearch All Fields:")
  (ebib)
  (ebib-db-set-filter `(contains "any" ,string) ebib--cur-db)
  (ebib--update-buffers))

;; or: (hash-table-keys (parsebib-parse gr/bibliography)) slow
;; or: (org-cite-basic--all-keys) requires oc
;; or: (hash-table-keys (citar-get-entries)) FAST

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
    (while (member new-key (hash-table-keys (citar-get-entries)))
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
(defun citar-ebib-jump-to-entry (key)
  "Jump to selected KEY-ENTRY in Ebib."
  (interactive (list (citar-select-ref)))
  (ebib-open key))

;;;###autoload
(defun ebib-citar-open-resource ()
  "In 'ebib' buffers, call 'citar-open' on reference at point."
  (interactive)
  (if (or (derived-mode-p 'ebib-index-mode 'ebib-entry-mode))
    (let ((key (list (ebib--get-key-at-point))))
      (citar-open key))
    (user-error "Not in ebib")))

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


