;;; ebib-zotero.el --- Integrate Zotero import into ebib.el     -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; adapted from https://github.com/tshu-w/.emacs.d/lisp/lang-latex.el
;;
;; originally https://github.com/joostkremers/ebib/issues/220

;;; Code:

(require 'ebib)
(require 'ebib-extras)
(require 'citar)

(defcustom ebib-zotero-translation-server "https://translate.manubot.org"
  "The address of Zotero translation server."
  :group 'ebib
  :type 'string)

(defun ebib-zotero-translate (item server-path &optional export-format)
  "Convert ITEM to EXPORT-FORMAT entry through `ebib-zotero-translation-server'."
  (let ((export-format (or export-format
                           (downcase (symbol-name (intern-soft bibtex-dialect))))))
      (shell-command-to-string
       (format "curl -s -d '%s' -H 'Content-Type: text/plain' '%s/%s' | curl -s -d @- -H 'Content-Type: application/json' '%s/export?format=%s'" item ebib-zotero-translation-server server-path ebib-zotero-translation-server export-format))))

(defun ebib-zotero-import-url (identifier)
    "Fetch a entry from zotero translation server via a URL.
The entry is stored in the current database."
  (interactive "MURL: ")
  (let (entry-type key)
    (kill-new identifier)
    (unless (get-buffer "*Ebib-entry*") ;; check that ebib is running
      (ebib-open))
    (with-temp-buffer
      (insert (ebib-zotero-translate identifier "web"))
      (goto-char (point-min))
      (setq entry-type (ebib--bib-find-next-bibtex-item))
      (setq key (cdr (assoc-string "=key=" (parsebib-read-entry entry-type))))
      (ebib-import-entries ebib--cur-db))
    (ebib nil key)
    ;; (ebib--goto-entry-in-index key)
    (ebib-generate-autokey)
    (ebib--update-entry-buffer)
    (ebib-save-all-databases)
    (citar-refresh)))

(defun ebib-zotero-import-identifier (identifier)
  "Fetch a entry from zotero translation server via an IDENTIFIER.
The entry is stored in the current database, and the identifier
can be DOI, ISBN, PMID, or arXiv ID."
  (interactive "MDOI or ISBN: ")
  (let (entry-type key)
    (kill-new identifier)
    (unless (get-buffer "*Ebib-entry*") ;; check that ebib is running
      (ebib-open))
    (with-temp-buffer
      (insert (ebib-zotero-translate identifier "search"))
      (goto-char (point-min))
      (setq entry-type (ebib--bib-find-next-bibtex-item))
      (setq key (cdr (assoc-string "=key=" (parsebib-read-entry entry-type))))
      (ebib-import-entries ebib--cur-db))
    (ebib nil key)
    ;; (ebib--goto-entry-in-index key)
    (ebib-generate-autokey)
    (ebib--update-entry-buffer)
    (ebib-save-all-databases)
    (citar-refresh)))

(provide 'ebib-zotero)

;;; ebib-zotero.el ends here
