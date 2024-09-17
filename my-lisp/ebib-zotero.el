;;; ebib-zotero.el --- Integrate Zotero import into ebib.el     -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; adapted from https://github.com/tshu-w/.emacs.d/lisp/lang-latex.el
;;
;; originally https://github.com/joostkremers/ebib/issues/220

;;; Code:

(require 'ebib)
(require 'ebib-extras)

(defcustom ebib-zotero-translation-server "https://translate.manubot.org"
  "The address of Zotero translation server."
  :group 'ebib
  :type 'string)

(defun ebib-zotero-translate (item server &optional export-format)
  "Convert ITEM to EXPORT-FORMAT with SERVER `ebib-zotero-translation-server'."
  (let ((export-format (or export-format
                           (downcase (symbol-name (intern-soft bibtex-dialect))))))
      (shell-command-to-string
       (format "curl -s -d '%s' -H 'Content-Type: text/plain' '%s/%s' | curl -s -d @- -H 'Content-Type: application/json' '%s/export?format=%s'" item ebib-zotero-translation-server server ebib-zotero-translation-server export-format))))

;;;###autoload
(defun ebib-zotero-import-url (URL)
    "Fetch a entry from zotero translation server via a URL.
The entry is stored in the current database."
  (interactive "MURL: ")
  (let (entry-type key)
    (kill-new URL)
    (unless (get-buffer "*Ebib-entry*") ;; check that ebib is running
      (ebib-open))
    (with-temp-buffer
      (insert (ebib-zotero-translate URL "web"))
      (goto-char (point-min))
      (setq entry-type (ebib--bib-find-next-bibtex-item))
      (setq key (cdr (assoc-string "=key=" (parsebib-read-entry entry-type))))
      (ebib-import-entries ebib--cur-db))
    (ebib-open key)
    ;; (ebib--goto-entry-in-index key)
    (ebib-generate-autokey)
    (ebib--update-entry-buffer)
    (ebib-save-all-databases)))

;;;###autoload
(defun ebib-zotero-import-identifier (identifier &optional file)
  "Fetch a entry from zotero translation server via an IDENTIFIER.
The entry is stored in the current database, and the identifier
can be DOI, ISBN, PMID, or arXiv ID."
  (interactive "MDOI or ISBN: ")
  (let ((entry (ebib-zotero-translate identifier "search"))
        entry-type
        key)
    (unless entry
      (error "No source found for identifier: %s" identifier))
    (kill-new identifier)
    (ebib--execute-when
      (no-database ;; check that database is loaded
       (ebib-open)))
    (with-temp-buffer
      (insert entry)
      (goto-char (point-min))
      (setq entry-type (ebib--bib-find-next-bibtex-item))
      (setq key (cdr (assoc-string "=key=" (parsebib-read-entry entry-type))))
      (ebib-import-entries ebib--cur-db))
    (ebib-open key)
    ;; (ebib--goto-entry-in-index key)
    (ebib-generate-autokey)
    (ebib--update-entry-buffer)
    (when (y-or-n-p "Correct entry? ")
      (ebib-save-all-databases)))
  (when file (ebib--import-pdf file)))

(defun ebib--import-pdf (file)
  (let* ((key (ebib--get-key-at-point))
         (shorttitle (ebib-zotero-cleanup-string
                      (ebib-get-field-value
                       "shorttitle" key ebib--cur-db t)))
         (title (ebib-zotero-cleanup-string
                 (ebib-get-field-value "title" key ebib--cur-db t)))
         (year (ebib-zotero-cleanup-string
                (ebib-get-field-value "year" key ebib--cur-db t)))
         (author (ebib-zotero-cleanup-string
                  (ebib-get-field-value "author"
                                        key ebib--cur-db t)))
         (editor (unless author
                   (ebib-zotero-cleanup-string
                    (ebib-get-field-value "editor"
                                          key ebib--cur-db t))))
         (author2 (when author
                    (with-temp-buffer
                      (insert author)
                      (re-search-backward ", ")
                      (delete-char 1)
                      (transpose-words 1)
                      (buffer-string))))
         (filename (read-string "Name: "
                                (concat key " - "
                                        (or author2 editor)
                                        " - "
                                        (or shorttitle title)
                                        " (" year ")"
                                        (file-name-extension file t)))))
    (rename-file file (concat "~/DT3 Academic/"
                              filename))
    (message "File added: %s" filename)))

(defun ebib-zotero-cleanup-string (string)
  (setq string
        (ignore-errors
          (replace-regexp-in-string "{\\|}" "" string)))
  (setq string
        (ignore-errors
          (replace-regexp-in-string "\"\\|\\“\\|”" "'" string)))
  (ignore-errors
    (replace-regexp-in-string ":" "-" string)))

;;;###autoload
(defun ebib-zotero-import-pdf (file)
  "Import FILE to ebib and devothink."
  (interactive (list (let ((completion-ignored-extensions
                            (append '(".localized") completion-ignored-extensions)))
                       (read-file-name "File: " "~/Downloads/"))))
  (let* ((source (completing-read "DOI/ISBN: " '(doi-search isbn-search this-entry)))
         id)
    (cond ((string= source "doi-search")
           (biblio-lookup 'biblio-crossref-backend))
          ((string= source "isbn-search")
           (call-interactively #'ebib-isbn-web-search))
          ((string= source "this-entry")
           (ebib--import-pdf file))
          (t (setq id source)))
    (unless (or (string= source "this-entry")
                (string= id source))
      (setq id (read-string "DOI/ISBN: ")))
    (ebib-zotero-import-identifier id file)))

;;;###autoload
(defalias 'ebib-import-pdf 'ebib-zotero-import-pdf)

;;;###autoload
(defalias 'ebib-auto-import 'ebib-zotero-import-identifier)

(provide 'ebib-zotero)

;;; ebib-zotero.el ends here
