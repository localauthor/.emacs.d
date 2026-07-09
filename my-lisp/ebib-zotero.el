;;; ebib-zotero.el --- Integrate Zotero import into ebib.el     -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; adapted from https://github.com/tshu-w/.emacs.d/lisp/lang-latex.el
;;
;; originally https://github.com/joostkremers/ebib/issues/220

;;; Code:

(require 'ebib)
(require 'gr-ebib-extras)

(defcustom ebib-zotero-translation-server "0.0.0.0:1969"
  "The address of Zotero translation server."
  :group 'ebib
  :type 'string)

(defun ebib-zotero-translation-select-server ()
  "Select zotero-translation server."
  (interactive)
  (setopt ebib-zotero-translation-server
          (completing-read "Server: "
                           '("0.0.0.0:1969"
                             "https://translate.manubot.org")
                           nil t)))

(defvar ebib-zotero-translation-server-dir "~/Repos/translation-server")

(defvar ebib-zotero-process "*zotero*")

(defvar ebib-zotero-process-buffer " *zotero-server*")

(defun ebib-zotero-process-start ()
  (interactive)
  (unless (or (string-match "https" ebib-zotero-translation-server)
              (get-process ebib-zotero-process))
    (let ((default-directory ebib-zotero-translation-server-dir))
      (make-process :name ebib-zotero-process
                    :buffer ebib-zotero-process-buffer
                    :command '("npm" "start")
                    :connection-type 'pipe
                    :filter #'ebib-zotero-process-filter)
      (sleep-for 4))))

(defun ebib-zotero-process-filter (proc string)
  "Insert process string into zotero buffer, check auth status."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc))))))
  (if (string-match "Listening" string)
      (message "Zotero Translation Server Ready")))

(defun ebib-zotero-translate (item server &optional export-format)
  "Convert ITEM to EXPORT-FORMAT with SERVER `ebib-zotero-translation-server'."
  (let ((export-format (or export-format
                           (downcase (symbol-name
                                      (intern-soft bibtex-dialect))))))
    (shell-command-to-string
     (format "curl -s -d '%s' -H 'Content-Type: text/plain' '%s/%s' | curl -s -d @- -H 'Content-Type: application/json' '%s/export?format=%s'" item ebib-zotero-translation-server server ebib-zotero-translation-server export-format))))

;;;###autoload
(defun ebib-zotero-import-url (URL)
  "Fetch a entry from zotero translation server via a URL.
The entry is stored in the current database."
  (interactive "sURL: ")
  (ebib-zotero-process-start)
  (let (key)
    (kill-new URL)
    (unless (get-buffer "*Ebib-entry*") ;; check that ebib is running
      (ebib-open))
    (with-temp-buffer
      (insert (ebib-zotero-translate URL "web"))
      (goto-char (point-min))
      (setq key (cdr (assoc-string "=key=" (parsebib-read-entry))))
      (ebib-import-entries ebib--cur-db))
    (ebib-open key)
    ;; (ebib--goto-entry-in-index key)
    (ebib-generate-autokey)
    (ebib--update-entry-buffer)
    (ebib-save-all-databases)))

;;;###autoload
(defun ebib-zotero-import-identifier (identifier)
  "Fetch a entry from zotero translation server via an IDENTIFIER.
The entry is stored in the current database, and the identifier
can be DOI, ISBN, PMID, or arXiv ID."
  (interactive "sDOI or ISBN: ")
  (ebib-zotero-process-start)
  (let ((entry (ebib-zotero-translate identifier "search"))
        key)
    (when (string= "Bad Request" entry)
      (when (y-or-n-p (format "No source for %s. Search web?" identifier))
        (ebib-isbn-web-search identifier)
        (ebib-open))
      (user-error "Bad Request"))
    (kill-new identifier)
    (ebib--execute-when
      (no-database ;; check that database is loaded
       (ebib-open)))
    (with-temp-buffer
      (insert entry)
      (goto-char (point-min))
      (setq key (cdr (assoc-string "=key=" (parsebib-read-entry))))
      (ebib-import-entries ebib--cur-db))
    (ebib-open key)
    ;; (ebib--goto-entry-in-index key)
    (ebib-generate-autokey)
    (ebib--update-entry-buffer)
    (when (y-or-n-p "Correct entry? ")
      (ebib-save-all-databases)
      (kill-new (ebib-zotero-formatted-file-name)))))

(defun ebib-zotero-formatted-file-name ()
  (interactive)
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
         (name (concat key " - "
                       (or author2 editor)
                       " - "
                       (or shorttitle title)
                       " (" year ")")))
    (kill-new name)
    (message "Copied: %s" name)
    name))

(defun ebib-zotero-import-file (file)
  "Import FILE to ebib and devonthink."
  (let* ((filename (read-string
                    "Name: "
                    (concat (ebib-zotero-formatted-file-name)
                            (file-name-extension file t)))))
    (if (y-or-n-p "Keep original?")
        (copy-file file (concat "~/Documents/Inbox/"
                                filename))
      (rename-file file (concat "~/Documents/Inbox/"
                                filename)))
    (message "File added: %s" filename)))

(defun ebib-zotero-cleanup-string (string)
  "Remove brackets, replace smart quotes and colons in STRING."
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
                       (read-file-name "Import PDF: " "~/Downloads/"))))
  (let* ((isbn-maybe (when (string-match ebib-zotero-isbn-regexp file)
                       (match-string 0 file)))
         (source (completing-read "DOI/ISBN: "
                                  '(doi-search isbn-search this-entry pick-entry)
                                  nil nil isbn-maybe))
         id)
    (pcase source
      ("doi-search"
       (biblio-lookup 'biblio-crossref-backend))
      ("isbn-search"
       (call-interactively #'ebib-isbn-web-search))
      ("this-entry"
       (ebib-zotero-import-file file))
      ("pick-entry"
       (ebib-init)
       (ebib-open)
       (call-interactively #'ebib-jump-to-entry)
       (ebib-zotero-import-file file))
      (_ (setq id source)))
    (unless (or (string= source "this-entry")
                (string= source "pick-entry")
                (string= source id))
      (setq id (read-string "DOI/ISBN: ")))
    (when id
      (ebib-zotero-import-identifier id)
      (ebib-zotero-import-file file))))

(defvar ebib-zotero-isbn-regexp
  "97[89][- ]?.[- ]?.\\{2\\}[- ]?.\\{6\\}[- ]?.")

;;;###autoload (autoload 'ebib-import-pdf "ebib-zotero")
(defalias 'ebib-import-pdf #'ebib-zotero-import-pdf
  "Import FILE to ebib and devothink.")

;;;###autoload
(defalias 'ebib-import-from-doi-or-isbn #'ebib-zotero-import-identifier
  "Fetch a entry from zotero translation server via an IDENTIFIER.
The entry is stored in the current database, and the identifier
can be DOI, ISBN, PMID, or arXiv ID.")

(provide 'ebib-zotero)

;;; ebib-zotero.el ends here
