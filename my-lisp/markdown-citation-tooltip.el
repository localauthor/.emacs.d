(defun markdown-citation-activate (limit)
  "Activate font-lock on markdown-citations up to LIMIT."
  (when (re-search-forward citar-markdown-citation-key-regexp limit t)
    (let ((beg (match-beginning 1))
          (end (match-end 1))
          (key (match-string-no-properties 1)))
      (funcall 'markdown-citation-fontify key beg end)
      t)))

(defun markdown-citation-fontify (key beg end)
  "Fontify markdown-citation KEY, from BEG to END."
  (if (member key (hash-table-keys (citar-get-entries)))
      (add-text-properties beg end
                           '(font-lock-face org-cite
                                            help-echo
                                            markdown-citation-tooltip))
    (add-text-properties beg end
                         '(font-lock-face font-lock-warning-face
                                          help-echo "No record"))))

(font-lock-add-keywords 'markdown-mode
                        '((markdown-citation-activate)))

(defun markdown-citation-formatted ()
  "Format markdown-citation tooltip."
  (let* ((key (thing-at-point 'symbol t))
         (author (or (citar-get-value "author" key)
                     (citar-get-value "editor" key)))
         (title (or (citar-get-value "title" key)
                    (citar-get-value "booktitle" key)))
         (year (citar-get-value "year" key))
         (publisher (or (citar-get-value "publisher" key)
                        (citar-get-value "journal" key))))
    (format "%s (%s) %s, %s." author year title publisher)))

(defun markdown-citation-tooltip (_win _obj pos)
  "Generate tooltip for markdown-citation at POS."
  (save-excursion
    (goto-char pos)
    (markdown-citation-formatted)))
