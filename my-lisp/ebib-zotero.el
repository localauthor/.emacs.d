;;; ebib-zotero.el --- Integrate Zotero import into ebib.el     -*- lexical-binding: t; -*-

;; adapted from https://github.com/tshu-w/.emacs.d/lisp/lang-latex.el
;; originally https://github.com/joostkremers/ebib/issues/220

(defcustom ebib-zotero-translation-server "https://translate.manubot.org"
  "The address of Zotero translation server."
  :group 'ebib
  :type 'string)

(defun ebib-zotero-server-p ()
  (unless (string-empty-p (shell-command-to-string "pgrep 'node'"))
    t))

(defun ebib-zotero-translate (item server-path &optional export-format)
  "Convert item to EXPORT-FORMAT entry through `ebib-zotero-translation-server'."
  (let ((export-format (or export-format
                           (downcase (symbol-name (intern-soft bibtex-dialect))))))
      (shell-command-to-string
       (format "curl -s -d '%s' -H 'Content-Type: text/plain' '%s/%s' | curl -s -d @- -H 'Content-Type: application/json' '%s/export?format=%s'" item ebib-zotero-translation-server server-path ebib-zotero-translation-server export-format))))

  (defun ebib-zotero-import-url (url)
    "Fetch a entry from zotero translation server via a URL.
The entry is stored in the current database."
    (interactive "MURL: ")
    (ebib)
    (with-temp-buffer
      (insert (ebib-zotero-translate url "web"))
      (ebib-import-entries ebib--cur-db)))

  (defun ebib-zotero-import-identifier (identifier)
    "Fetch a entry from zotero translation server via an IDENTIFIER.
The entry is stored in the current database,
and the identifier can be DOI, ISBN, PMID, or arXiv ID."
    (interactive "MDOI or ISBN: ")
    (unless (get-buffer "*Ebib-entry*") ;; check that ebib is running
      (ebib))
    (with-temp-buffer
      (insert (ebib-zotero-translate identifier "search"))
      (ebib-import-entries ebib--cur-db)))
  ;; (progn
  ;;   (ebib)
  ;;   (ebib-goto-last-field)))

(provide 'ebib-zotero)
