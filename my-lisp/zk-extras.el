;;; zk-extras.el --- Extra functions for zk.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'zk)
(require 'zk-index)
;; (require 'zk-desktop)
(require 'zk-luhmann)
(require 'link-hint)

;;; zk daily note

;;;###autoload
(defun zk-daily-note (&optional p)
  "Create new daily note or visit today’s note.
With prefix arg, select date for new daily note."
  (interactive "P")
  (let* ((time (when p
                 (date-to-time
                  (org-read-date))))
         (id (concat (format-time-string "%Y%m%d" time) "0000"))
         (title (format-time-string "%F %A" time))
         (file (if (zk-id-p id)
                   (zk--parse-id 'file-path id)
                 (zk--note-file-path id title))))
    (if (file-exists-p file)
        (progn
          (find-file file)
          (goto-char (point-max)))
      (find-file file)
      (funcall zk-new-note-header-function title id "201805121020")
      (re-search-backward "tags:")
      (end-of-line)
      (insert "#dailynote #journalentry")
      (save-buffer)
      (goto-char (point-max)))
    file)) ;; return file for gr-intial-buffer

;;;###autoload
(defun zk-index-daily-notes ()
  "List all daily notes."
  (interactive)
  (zk-index
   (zk--grep-file-list "#journalentry")
   nil
   #'zk-index--sort-created
   nil
   "{Daily Notes}"))

;; "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"

(keymap-set zk-index-mode-map "d" #'zk-index-daily-notes)

;;; zk header line

(defun zk-index--format-header-line (right left)
  (let* ((margin-space (or (car (window-margins))
                           3))
         (spaces
          (+ (* 2 margin-space)
             (- (window-width)
                (length right)
                (length left)))))
    (concat
     (make-string margin-space ? )
     " "
     left
     (make-string spaces ? )
     " "
     right)))

(defun zk-index--set-header-line (&optional mode-line)
  (let ((left mode-name)
        (right (or mode-line
                   "")))
    (setq header-line-format
          (zk-index--format-header-line right left))))


;;; zk citar mentions

(defun zk-citar--lit-refs-list (id)
  (when-let* ((title (zk--parse-id 'title id))
              (key (when (string-match zk-citar-citekey-regexp title)
                     (match-string 0 title))))
    (when (citar-get-entry key)
      (setq-local zk-citar-note-citekey key)
      (zk--grep-file-list key))))

(defun zk-citar-lit-refs ()
  "Select from notes that mention current note’s citekey.
Must be called from a zk note with a citekey in its title."
  (interactive)
  (when-let ((files (zk-citar--lit-refs-list (zk--current-id))))
    (find-file (funcall zk-select-file-function
                        (format "Files containing \"%s\": " zk-citar-note-citekey) files))
    (user-error "No citekey in title")))

(defun zk-index-citar-mentioned ()
  (interactive)
  (zk-index (zk-citar--lit-refs-list (zk--current-id))))

(defun zk-citar-lit-network ()
  "Find `zk-backlinks' and `zk-citar-lit-refs' for current or selected note.
Backlinks and Lit-Refs are grouped separately."
  (interactive)
  (let* ((id (zk--current-id))
         (backlinks (zk--backlinks-list id))
         (lit-refs (zk-citar--lit-refs-list id))
         (resources))
    (if (or backlinks lit-refs)
        (progn
          (dolist (file lit-refs)
            ;; abbreviate-file-name allows a file to be in both groups
            (push (propertize (abbreviate-file-name file) 'type 'lit) resources))
          (dolist (file backlinks)
            (push (propertize file 'type 'backlink) resources))
          (find-file (funcall zk-select-file-function
                              "Links: "
                              resources
                              'zk--citar-lit-network-group-function
                              'identity)))
      (user-error "No links found"))))

(defun zk--citar-lit-network-group-function (file transform)
  "Group FILE by type and TRANSFORM."
  (if transform
      (progn
        (string-match (zk-file-name-regexp) file)
        (match-string 2 file))
    (cond
     ((eq 'backlink (get-text-property 0 'type file)) "Backlinks")
     ((eq 'lit (get-text-property 0 'type file)) "Lit-Refs"))))


;; (defun zk-citar-lit-backlinks ()
;;   "Select from notes that mention current note’s citekey.
;; Must be called from a zk note with a citekey in its title."
;;   (interactive)
;;   (if-let ((title (zk--parse-id 'title (zk--current-id)))
;;            (key (when (string-match zk-citar-citekey-regexp title)
;;                   (match-string 0 title))))
;;       (when (citar-get-entry key)
;;         (zk-find-file-by-full-text-search key))
;;     (user-error "No citekey in title")))


;; (citar-register-notes-source 'zk-mentions
;;                              '( :name "zk-mentions"
;;                                 :category zk-file
;;                                 :items zk-citar--get-mention-notes
;;                                 :hasitems zk-citar--has-notes
;;                                 :open find-file
;;                                 :create nil
;;                                 :transform file-name-nondirectory))

;; (defun zk-citar--get-mention-notes (&optional keys)
;;   "Return hash-table with KEYS with file notes."
;;   (let* ((zk--no-gc t)
;;          (keys (hash-table-keys (citar-get-entries)))
;;          (files (zk--directory-files t))
;;          (ht (make-hash-table :test 'equal)))
;;     (dolist (key (hash-table-keys (citar-get-entries)) ht)
;;       (when-let ((files (zk--grep-file-list key)))
;;         (puthash key files ht)))))


;;; recents index

(defun zk-index-recent ()
  "Open ZK-Index with latest 10 zk notes."
  (interactive)
  (let ((zk--no-gc t)
        (files (zk-index--sort-modified (zk--directory-files t))))
    (zk-index (take 10 files))))

;;; copy with backlink

(defvar zk-copy-with-backlink nil)

(defun zk-copy-with-backlink (beg end append)
  "Copy region or paragraph at point with backlink appended.
With prefix arg, append to previously copied text."
  (interactive "r\nP")
  (unless (use-region-p)
    (save-excursion
      (mark-paragraph)
      (setq beg (region-beginning)
            end (region-end))
      (deactivate-mark)
      (pulse-momentary-highlight-region beg end)))
  (let* ((zk-id (ignore-errors (zk--current-id)))
         (backlink (if zk-id
                       (concat " Source: "
                               (with-temp-buffer
                                 ;; accounts for luhmann ids
                                 (zk--insert-link zk-id)
                                 (buffer-string)))
                     (format " [[%s::%d][Source]]"
                             buffer-file-name (line-number-at-pos beg))))
         (text (concat (buffer-substring-no-properties beg end)
                       backlink)))
    (if append
        (kill-new
         (concat text "\n" zk-copy-with-backlink))
      (setq zk-copy-with-backlink text)
      (kill-new text))
    (deactivate-mark)
    (message "Copied with backlink")))

;;; backlinks index

(defun zk-index-backlinks ()
  (interactive)
  (if-let* ((id (zk--current-id))
            (files (zk--backlinks-list id)))
      (zk-index files nil nil (format "ZK-Index: Backlinks"))
    (user-error "No backlinks found")))

(setq display-buffer-alist
      (append
       display-buffer-alist
       '(("ZK-Index: Backlinks"
          (display-buffer-in-direction)
          (direction . bottom)
          (post-command-select-window t)
          (dedicated . t)
          (window-height . 0.2)))))

;;; zk-index-stored

;; keep the same list of notes
;; turn off functions that would change the index
;; except sort functions

(defvar zk-index-save-file "~/Dropbox/ZK/zk-index-save-file.eld")

(defvar zk-index-save--alist nil)

(defun zk-index-save--alist ()
  "Return variable `zk-index-save--alist’, loading if needed."
  (unless zk-index-save--alist
    (zk-index-save--load-from-file))
  zk-index-save--alist)

(defun zk-index-save--load-from-file ()
  "Load saved zk-indexes from file."
  (let ((file zk-index-save-file))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((indexes (when (search-forward "(" nil t)
                         (forward-char -1)
                         (read (current-buffer)))))
          (unless (and indexes
                       (listp indexes))
            (error "No zk-indexes found in `%s'" file))
          (setq zk-index-save--alist indexes))))))

(defun zk-index-save--all-indexes ()
  "Return list of saved zk-indexes."
  (unless zk-index-save--alist
    (zk-index-save--load-from-file))
  (mapcar #'car zk-index-save--alist))

(defun zk-index-save--check-name (name)
  "Check index NAME to avoid duplicates."
  (while (member name (zk-index-save--all-indexes))
    (if (y-or-n-p
         (format "ZK-Index ‘%s’ already exists. Overwrite?" name))
        (zk-index-save-delete name)
      (setq name (read-string "Duplicate name. Choose another: "))))
  name)

(defun zk-index-save (index)
  "Save tab-set of current frame as INDEX name."
  (interactive
   (list (zk-index-save--select "Save zk-index as: ")))
  (let* ((index (zk-index-save--check-name index))
         (ids (list (zk-index--current-id-list (current-buffer)))))
    (push (cons index ids) zk-index-save--alist)
    (zk-index-save-to-file)
    (message "ZK-Index ‘%s’ saved to file." index)))

(defun zk-index-save-open (&optional index)
  "Open saved zk-index INDEX."
  (interactive
   (list (zk-index-save--select "Open zk-index: ")))
  (let* ((ids (car (assoc-default index zk-index-save--alist)))
         (files (zk--parse-id 'file-path ids)))
    (setq zk-index-query-mode-line nil)
    (zk-index files nil nil index)))

(defun zk-index-save-delete (index)
  "Delete zk-index INDEX."
  (interactive
   (list (zk-index-save--select "Delete zk-index: ")))
  (when (y-or-n-p (format "Really delete zk-index ‘%s’?" index))
    (let ((index-info (cons index
                            (assoc-default index zk-index-save--alist))))
      (setq zk-index-save--alist
            (delete index-info zk-index-save--alist))
      (zk-index-save-to-file))))

(defun zk-index-save--select (&optional prompt initial)
  "Completing read function for selecting a tab-set.
  With optional PROMPT and INITIAL value."
  (let ((indexes (zk-index-save--alist)))
    (completing-read
     (or prompt "Select: ")
     (lambda (string predicate action)
       (if (eq action 'metadata)
           `(metadata
             (category . zk-index)
             ;; (annotation-function . zk-index-save-annotate)
             )
         (complete-with-action action indexes string predicate)))
     nil (not (eq this-command 'zk-index-save))
     initial)))

(defun zk-index-save-to-file ()
  "Write variable `zk-index-save--alist' to `zk-index-save-file’."
  (interactive)
  (let ((file zk-index-save-file))
    (if (file-writable-p file)
        (with-temp-buffer
          (let ((print-length nil)
                (print-level nil)
                (print-circle nil))
            (insert ";; -*- lisp-data -*-\n\n")
            (insert (format ";; zk-index-save file\n;; Saved on %s\n\n"
                            (format-time-string "%Y.%m.%d %R")))
            (pp zk-index-save--alist (current-buffer))
            (write-region (point-min) (point-max) file)))
      (error "Could not write to filters file `%s'" file))))


;; name and save index

;; add note to saved index

;; save custom order

;; custom order

(defun zk-index-move-line-up ()
  (interactive)
  (read-only-mode -1)
  (move-text-line-up)
  (read-only-mode 1))

(defun zk-index-move-line-down ()
  (interactive)
  (read-only-mode -1)
  (move-text-line-down)
  (read-only-mode 1))

(define-key zk-index-mode-map (kbd "C-S-n") #'zk-index-move-line-down)

(define-key zk-index-mode-map (kbd "C-S-p") #'zk-index-move-line-up)

;;; custom note capture

;; (defun zk-capture ()
;;   (interactive)
;;   ;; why not use org-capture?
;;   (let* ((region (when (use-region-p)
;;                    (buffer-substring (region-beginning)
;;                                      (region-end))))
;;          (buffer (org-get-indirect-buffer
;;                   (find-file-noselect
;;                    (zk--parse-id 'file-path "201801190001")))))
;;     (pop-to-buffer buffer '(display-buffer-in-side-window
;;                             (window-height . 0.4)
;;                             (post-command-select-window t)
;;                             (side . bottom)))
;;     (goto-char (point-min))
;;     (widen)
;;     (re-search-forward "* New Notes")
;;     (forward-line 2)
;;     (insert "** # New note\n\n\n\n")
;;     (forward-line -2)
;;     (org-narrow-to-subtree)
;;     (when region (insert region))
;;     (popper-toggle-type)))

(defun zk-capture ()
  (interactive)
  (org-capture nil "z"))

;;; xref

;;;###autoload
(defun zk-xref (string)
  "Use `xref' to search for STRING in all notes.
Opens search results in an `xref' buffer."
  (interactive "szk-xref: ")
  (if-let* ((matches (xref-matches-in-files
                      string
                      (zk--directory-files t))))
      (xref--show-xrefs matches 'display-buffer-at-bottom)
    (error "No matches")))

;; (setq xref-show-xrefs-function 'consult-xref)

;;; tag completion

(defun zk-tag-completion-at-point ()
  "Completion at point function for tags."
  (let* ((case-fold-search t)
         (end (point))
         (begin (when (and (zk-file-p)
                           (save-excursion
                             (re-search-backward "\\#\\+tags\\:" (line-beginning-position) t)))
                  (if (looking-back " ")
                      (point)
                    (save-excursion
                      (re-search-backward "\\#.*" nil t))
                    (match-beginning 0))))
         (candidates (zk--grep-tag-list)))
    (when (and begin (<= begin end))
      (list begin
            end
            (completion-table-dynamic
             (lambda (_)
               candidates))))))

(add-hook 'completion-at-point-functions 'zk-tag-completion-at-point)

;;; clickable tags

;;;###autoload
(defun zk-tag-font-lock (limit)
  "Activate font-lock on zk-tags up to LIMIT."
  (when (and (zk-file-p)
             (re-search-forward "[[:space:]]\\(#[a-zA-Z0-9-]+\\)" limit t))
    (let ((beg (match-beginning 1)) ;; -1 to match the #
          (end (match-end 1))
          (tag (match-string 1))
          (map (make-sparse-keymap)))
      (define-key map (kbd "<mouse-3>")
                  `(lambda () (interactive)
                     (funcall zk-tag-click-function ,tag)))
      (funcall 'zk-tag-fontify tag beg end map)
      t)))

;;;###autoload
(defun zk-tag-fontify (tag beg end map)
  (add-text-properties beg end
                       `(face link
                              mouse-face highlight
                              help-echo "Right click to search tag"
                              keymap ,map)))

(defvar zk-tag-click-function nil)

(setq zk-tag-click-function #'zk-tag-search)

(defun zk-tag-index (tag)
  "Clicking a tag narrows index to files containing that tag."
  (interactive (list (completing-read "Find tag: " (zk--grep-tag-list))))
  (zk-index-search tag))

(font-lock-add-keywords 'org-mode
                        '((zk-tag-font-lock)))


;;; General Utilities

;;;###autoload
(defun link-hint-other-tab ()
  "Use avy to open a link in other-tab."
  (interactive)
  (avy-with link-hint-other-tab
    (link-hint--one :other-tab)))

(link-hint-define-type 'zk-link
  :other-tab #'zk-follow-link-other-tab)

;;;###autoload
(defun zk-follow-link-other-tab (&optional id)
  "Open note that corresponds with the zk ID at point."
  (interactive)
  (if-let* ((id (or (zk--id-at-point)
                    id)))
      (gr/zk-find-file-other-tab id)
    (error "No zk-link at point")))


;;;###autoload
(defun zk-index-aw-select ()
  (interactive)
  (if-let* (;;(aw-ignore-current t)
            (aw-ignored-buffers link-hint-aw-select-ignored-buffers)
            (id (zk-index--button-at-point-p)))
      (link-hint--aw-select-zk-link id)
    (message "No zk-button at point")))


;;;###autoload
(defun zk-copy-link-to-current-note ()
  "Copy link to current note."
  (interactive)
  (let* ((id (zk--current-id))
         (title (zk--parse-id 'title id)))
    (kill-new
     (format-spec zk-link-and-title-format
                  `((?i . ,id)(?t . ,title)))))
  (message "Copied link to current buffer"))


;;;###autoload
(defun zk-word-count (&optional files)
  "Report word count of all files in 'zk-directory'.
Optionally takes list of FILES."
  (interactive)
  (let* ((files (or files
                    (zk--directory-files t)))
         (wc 0))
    (mapc
     (lambda (x)
       (let ((str (shell-command-to-string (concat "wc -w "
                                                   (shell-quote-argument x)))))
         (string-match "[0-9]+" str )
         (setq wc (+ wc (string-to-number (match-string 0 str))))))
     files)
    (message "Words: %s" wc)
    wc))

;;;###autoload
(defun zk-lit-notes ()
  "Find literature note."
  (interactive)
  (find-file (zk--select-file "Lit notes: " (zk-lit-notes-list))))

;;;###autoload
(defun zk-lit-notes-list ()
  "Return list of literature notes."
  (interactive)
  (let* ((ed-notes (zk--directory-files t gr/dickinson-ref-regexp)))
    (remq nil (mapcar
               (lambda (x)
                 (unless (member x ed-notes)
                   x))
               (zk--directory-files t "[a-z]+[0-9]\\{4\\}[a-z]?")))))

;;;###autoload
(defun zk-lit-notes-count ()
  (length (zk-lit-notes-list)))

;;;###autoload
(defun zk-luhmann-notes-count ()
  (length (zk-luhmann-files)))

;;;###autoload
(defun zk-lit-notes-index ()
  "List lit notes in ZK-Index, by size."
  (interactive)
  (zk-index (zk-lit-notes-list) nil #'zk-index--sort-modified)
  (zk-index--reset-mode-line))

;;;###autoload
(defun zk-luhmann-word-count ()
  (interactive)
  (zk-word-count (zk--directory-files t "{"))) ;; not general

;;;###autoload
(defun gr/zk-word-count ()
  "Report word count for notes, various categories."
  (interactive)
  (let* ((all-notes (zk--directory-files t))
         (ed-notes (zk--directory-files t gr/dickinson-ref-regexp))
         (lit-notes (remq nil (mapcar
                               (lambda (x)
                                 (unless (member x ed-notes)
                                   x))
                               (zk--directory-files t "[a-z]+[0-9]\\{4\\}"))))
         (journal (zk--grep-file-list "journalentry"))
         (poem (zk--grep-file-list "mypoem"))
         (non-notes
          (append ed-notes lit-notes journal poem))
         (notes
          (remq nil (mapcar
                     (lambda (x)
                       (unless (member x non-notes)
                         x))
                     all-notes))))
    (zk-word-count notes)))

;;;###autoload
(defun zk-non-luhmann-list ()
  "Index listing of non-Luhmann notes.
Also excludes, journal, poem, Dickinson, and literature notes."
  (let* ((all-notes (zk--directory-files t))
         (ed-notes (zk--directory-files t gr/dickinson-ref-regexp))
         (luhmann-notes (zk--directory-files t "{"))
         (lit-notes (zk--directory-files t "[a-z]+[0-9]\\{4\\}"))
         (list (zk--grep-file-list (string-join '("\\#creative"
                                                  "\\#personal"
                                                  "\\#song"
                                                  "\\#booknote"
                                                  "\\#exclude"
                                                  "journalentry"
                                                  "mypoem"
                                                  "filmnotes")
                                                "\\|")))
         (notes
          (append ed-notes
                  luhmann-notes
                  lit-notes
                  list)))
    (remq nil (mapcar
               (lambda (x)
                 (unless (member x notes)
                   x))
               all-notes))))

;; these take too long
;; (defvar zk-luhmann-notes-count (length (zk-luhmann-files)))
;; (defvar zk-lit-notes-count (length (zk-lit-notes-list)))
;; (defvar zk-core-notes-count (length (zk-non-luhmann-list)))

(defvar gr/dickinson-ref-regexp "\\(Fr[0-9]\\{1,4\\}\\)")

;;;###autoload
(defun zk-stats (&optional arg)
  "Report number of notes, various categories.
Optional ARG to inhibit message, for resetting counts."
  (interactive)
  (unless arg
    (message (format "Luhmann: %s | Lit: %s"
                     (length (zk-luhmann-files))
                     (length (zk-lit-notes-list))))))

;;;###autoload
(defun zk-non-luhmann-index ()
  (interactive)
  (zk-index (zk-non-luhmann-list)))

;;;###autoload
(defun zk-non-luhmann-word-count ()
  (interactive)
  (zk-word-count (zk-non-luhmann-list)))

;;;###autoload
(defun zk-core-list ()
  "Index listing of core notes.
Also excludes, journal, poem, Dickinson, and literature notes."
  (let* ((all-notes (zk--directory-files t))
         (ed-notes (zk--directory-files t gr/dickinson-ref-regexp))
         (film (zk--grep-file-list "filmnotes"))
         (personal (zk--grep-file-list "#personal"))
         (creative (zk--grep-file-list "#creative"))
         (songs (zk--grep-file-list "#song"))
         (journal (zk--grep-file-list "journalentry"))
         (poem (zk--grep-file-list "mypoem"))
         (notes
          (append ed-notes
                  film
                  songs
                  creative
                  personal
                  journal
                  poem)))
    (message "%s" (length (delete-dups notes)))
    (remq nil (mapcar
               (lambda (x)
                 (unless (member x notes)
                   x))
               all-notes))))

;;;###autoload
(defun zk-core-index ()
  (interactive)
  (zk-index (zk-core-list)))

;;;###autoload
(defun zk-core-count ()
  (interactive)
  (zk-word-count (zk-core-list)))

;;; Unlinked Notes

;;;###autoload
(defun gr/zk--unlinked-notes-list ()
  "Return list of IDs for notes that no notes link to.
Takes ZK-ALIST."
  (let* ((all-link-ids (zk--grep-link-id-list))
         (all-ids (zk--id-list)))
    (remq nil (mapcar
               (lambda (x)
                 (when (and (not (member x all-link-ids))
                            ;; filter Dickinson notes
                            (not (string-match "^20201210" x))
                            ;; filter daily notes
                            (not (string-match "0000$" x)))
                   x))
               all-ids))))

;;;###autoload
(defun gr/zk-unlinked-notes ()
  "Find unlinked notes, minus ED and lit notes."
  (interactive)
  (let* ((zk-alist (zk--alist))
         (ids (gr/zk--unlinked-notes-list))
         (lit-notes (zk-lit-notes-list)))
    (if-let* ((notes (zk--parse-id 'file-path ids zk-alist)))
        (progn
          (mapc (lambda (x)
                  (when (member x lit-notes)
                    (delq x notes)))
                notes)
          (find-file (zk--select-file "Unlinked notes: " notes)))
      (user-error "No unlinked notes found"))))

(provide 'zk-extras)
;;; zk-extras.el ends here

;; (defun zk-luhmann-index ()
;;   "Precursor to zk-index."
;;   (interactive)
;;   (let ((luhmann "*Luhmann-Index*")
;;         (line))
;;     (if (get-buffer luhmann)
;;         (with-current-buffer luhmann
;;           (setq line (line-number-at-pos))
;;           (read-only-mode -1)
;;           (erase-buffer)
;;           (zk-luhmann-insert-index)
;;           (goto-char (point-min))
;;           (forward-line (1- line))
;;           (read-only-mode))
;;       (progn
;;         (generate-new-buffer luhmann)
;;         (with-current-buffer luhmann
;;           (zk-luhmann-insert-index)
;;           (local-set-key (kbd "n") 'next-line)
;;           (local-set-key (kbd "p") 'previous-line)
;;           (local-set-key (kbd "f") 'consult-focus-lines)
;;           (local-set-key (kbd "g") 'zk-luhmann-index)
;;           (local-set-key (kbd "q") 'delete-window)
;;           (read-only-mode 1)
;;           (toggle-truncate-lines)
;;           (goto-char (point-min)))))
;;     (pop-to-buffer luhmann)))

;; (defun zk-luhmann-insert-index ()
;;   (let ((files (zk--luhmann--function
;;                 (zk--luhmann-candidates))))
;;     (dolist (file files)
;;       (string-match zk-id-regexp file)
;;       (insert-text-button file
;;                           'follow-link t
;;                           'face 'default
;;                           'action
;;                           `(lambda (_)
;;                              (progn
;;                                (view-file-other-window
;;                                 (zk--parse-id 'file-path
;;                                               ,(match-string 0 file))))))
;;       (newline))))
