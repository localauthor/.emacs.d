(defun zk-index-format-candidates (files format)
  "Format zk-index candidates."
  (let* ((output))
    (dolist (file files)
      (progn
        (string-match (concat "\\(?1:"
                              zk-id-regexp
                              "\\).\\(?2:.*?\\)\\."
                              zk-file-extension
                              ".*")
                      file)
        (let ((id (match-string 1 file))
              (title (match-string 2 file)))
          (when id
            (push (format-spec format
                               `((?i . ,id)(?t . ,title)))
                  output)))))
    output))


(defun zk--completion-at-point-candidates (&optional files)
  "Return a list of formatted candidates for 'zk-completion-at-point'.
If FILES is nil, return formatted candidates for all files in
'zk-directory'; if non-nil, it should be a list of filepaths to
be formatted as completion candidates."
  (let* ((list (if files files
                 (zk--directory-files)))
         (output))
         (dolist (file list)
      (progn
        (string-match (concat "\\(?1:"
                              zk-id-regexp
                              "\\).\\(?2:.*?\\)\\."
                              zk-file-extension
                              ".*")
                      file)
        (let ((id (match-string 1 file))
              (title (match-string 2 file)))
          (when id
            (push (format-spec zk-completion-at-point-format
                               `((?i . ,id)(?t . ,title)))
                  output)))))
    output))


;;; zk-extras.el --- Experimental or Extra functions for zk.el  -*- lexical-binding: t; -*-


;;; Commentary:
;; 

;;; Code:
(require 'zk)
(require 'dickinson)
(require 'vertico)

;;; TODO
;; set button keymap, so C-RET opens above without switching

;;; ZK Desktop

(defun zk-index-desktop ()
  (interactive)
  ;; make new desktop, or add to existing
  ;; list existing desktops
  (let ((buffer "*ZK-Desktop*"))
    (when (string= (buffer-name) "*ZK-Index*")
        (progn
          (read-only-mode -1)
          (let ((items (if (use-region-p)
                           (buffer-substring (save-excursion
                                               (goto-char (region-beginning))
                                               (line-beginning-position))
                                             (save-excursion
                                               (goto-char (region-end))
                                               (line-end-position)))
                         (buffer-substring (line-beginning-position)(line-end-position)))))
            (read-only-mode)
            (unless (get-buffer buffer)
              (generate-new-buffer buffer))
            (with-current-buffer buffer
              (goto-char (point-max))
              (newline)
              (insert items)
              (newline)
              (unless (bound-and-true-p truncate-lines)
                (toggle-truncate-lines))
              (goto-char (point-max))))))
      (unless (get-buffer-window buffer 'visible)
        (progn
          (display-buffer buffer
                          '(display-buffer-pop-up-frame
                            (inhibit-switch-frame . t)))
          (set-frame-position (selected-frame) 850 80)))))

;;; ZK Index View

;; a semi-persistent curated selection of notes
;; for more persistent selections, use ZK Desktop

;; make focus and search case-insensitive
;; sort by latest modified?
;; but how to stack sort functions? luhmann and modified, for example

;; change everything to take files, and format only upon inserting

(defvar zk-index-last-sort-function nil
  "Set to the name of the last sort function usedn.
If no sort function, gets set to nil.")

(defun zk-index-luhmann ()
  (interactive)
  (zk-index (zk--luhmann-files) 'zk--luhmann-sort-function))

(defun zk-index (&optional candidates sort-fn)
  (interactive)
  (setq zk-index-last-sort-function sort-fn)
  (let ((buffer "*ZK-Index*")
        (list (if candidates candidates
                      (zk--completion-at-point-candidates))))
    (unless (get-buffer buffer)
      (progn
        (zk-find-file-by-id zk-default-backlink)
        (generate-new-buffer buffer)
        (with-current-buffer buffer
          (zk-index-insert list sort-fn)
          (local-set-key (kbd "n") 'next-line)
          (local-set-key (kbd "p") 'previous-line)
          (local-set-key (kbd "o") 'link-hint-aw-select)
          (local-set-key (kbd "f") 'zk-index-focus)
          (local-set-key (kbd "l") 'zk-index-luhmann)
          (local-set-key (kbd "s") 'zk-index-search)
          (local-set-key (kbd "d") 'zk-index-desktop)
          (local-set-key (kbd "g") 'zk-index-refresh)
          (local-set-key (kbd "q") 'delete-window)
          (read-only-mode 1)
          (toggle-truncate-lines)
          (goto-char (point-min)))))
    (when candidates
      (zk-index-refresh candidates sort-fn))
    (pop-to-buffer buffer)))

(defun zk-index-refresh (&optional candidates sort-fn)
  (interactive)
  (let ((candidates (if candidates candidates
                      (zk--completion-at-point-candidates)))
        (sort-fn (if sort-fn sort-fn
                   nil))
        (line))
  (with-current-buffer "*ZK-Index*"
    (setq line (line-number-at-pos))
    (read-only-mode -1)
    (erase-buffer)
    (zk-index-insert candidates sort-fn)
    (goto-char (point-min))
    (unless (zk-index-narrowed-p)
      (forward-line line))
    (read-only-mode))))

(defun zk-index-insert (candidates &optional sort-fn)
  (let* ((sort-fn (if sort-fn sort-fn
                    nil))
         (files (if sort-fn (funcall sort-fn candidates)
                  candidates)))
    (dolist (file files)
      (string-match zk-id-regexp file)
      (insert-button file
                     'follow-link t
                     'face 'default
                     'action
                     `(lambda (_)
                        (progn
                          (view-file-other-window
                           (zk--parse-id 'file-path
                                         ,(match-string 0 file))))))
      (newline))))

(defun zk-index-quit ()
  (interactive)
  (if (zk-index-narrowed-p)
      (if (y-or-n-p "Refresh index? ")
          (zk-index)
        (delete-window))
    (delete-window)))

(defun zk-index-narrowed-p ()
  (with-current-buffer "*ZK-Index*"
    (if (< (count-lines (point-min) (point-max))
           (length (zk--id-list)))
        t nil)))

;;;; Index Narrowing Functions

(defun zk-index-narrowed-candidates ()
  "Return narrowed list of candidates.
Asks whether to search all files or only those in current index."
  (let* ((command this-command)
         (scope (if (zk-index-narrowed-p)
                    (if (y-or-n-p "Query current subset (y), or all (n)? ")
                        (zk-current-index-id-list)
                      (zk--id-list))
                  (zk--id-list)))
         (string (read-string "Search: "))
         (query (cond
                 ((eq command 'zk-index-focus)
                  (mapcar
                   (lambda (x)
                     (zk--parse-file 'id x))
                   (directory-files zk-directory t (regexp-quote string))))
                 ((eq command 'zk-index-search)
                  (zk--grep-id-list string))))
         (focus
          (mapcar
           (lambda (x)
             (when (member x scope)
               x))
           query))
         (files
          (mapcar
           (lambda (x)
             (zk--parse-id 'file-path x))
           (remq nil focus))))
    (if files
        (zk--completion-at-point-candidates files)
      (error "No matches for \"%s\"" string))))

;;; sort functions

;; the latest modified sort functions take a list of files
;; the above are all candidates

(defun zk-index--sort-latest-modified (list)
  "Sort LIST for latest modification."
  (sort list
        (lambda (a b)
          (let ((one
                 (file-attribute-access-time (file-attributes a)))
                (two
                 (file-attribute-access-time (file-attributes b))))
            (time-less-p two one)))))

;;;; zk-index-focus
;; narrow index based on search of note titles (case sensitive)
;; an alternative to consult-focus-lines

(defun zk-index-focus ()
  "Narrow index based on search of note titles."
  (interactive)
  (zk-index-refresh (zk-index-narrowed-candidates) zk-index-last-sort-function))

;;;; zk-index-search
;; narrow index based on search of notes' full text

(defun zk-index-search ()
  "Narrow index based on search of note titles."
  (interactive)
  (zk-index-refresh (zk-index-narrowed-candidates) zk-index-last-sort-function))

(defun zk-current-index-id-list ()
  "Return list of IDs for current index, as filepaths."
  (let (ids)
    (with-current-buffer "*ZK-Index*"
      (save-excursion
        (goto-char (point-min))
        (save-match-data
          (while (re-search-forward zk-id-regexp nil t)
            (push (match-string-no-properties 0) ids)))
        ids))))

(defun zk--grep-id-list (str)
  "Return a list of IDs for files containing STR."
  (let ((files (zk--grep-file-list str)))
    (mapcar
     (lambda (x)
       (zk--parse-file 'id x))
     files)))

;;; General Utilities

(defun zk-copy-link-to-current-note ()
  "Copy link to current note."
  (interactive)
  (let* ((id (zk--current-id))
         (title (zk--parse-id 'title id)))
    (kill-new
     (format-spec zk-link-and-title-format
                  `((?i . ,id)(?t . ,title)))))
  (message "Copied link to current buffer"))

(defun zk-lit-notes ()
  "Find literature note."
  (interactive)
  (let* ((ed-notes (zk--directory-files t gr/dickinson-ref-regexp))
         (lit-notes (remq nil (mapcar
                               (lambda (x)
                                 (unless (member x ed-notes)
                                   x))
                               (zk--directory-files t "[a-z]+[0-9]\\{4\\}")))))
    (find-file (zk--select-file "Lit notes: " lit-notes))))

(defun zk-stats ()
  "Report number of notes, various categories."
  (interactive)
  (let* ((ed-notes (zk--directory-files nil gr/dickinson-ref-regexp))
         (all-notes (zk--directory-files))
         (luhmann-notes (zk--directory-files nil "{"))
         (lit-notes (remq nil (mapcar
                               (lambda (x)
                                 (unless (member x ed-notes)
                                   x))
                               (zk--directory-files nil "[a-z]+[0-9]\\{4\\}"))))
         (journal (length (zk--grep-file-list "#journalentry")))
         (poem (length (zk--grep-file-list "#mypoem")))
         (notes (- (length all-notes)
                   (+ journal poem (length lit-notes) (length ed-notes)))))
    (message (format "Notes: %s | Lit: %s | Luhmann: %s" notes (length lit-notes) (length luhmann-notes)))))


;;; Luhmann IDs

(defun zk-luhmann ()
  "Find note with Luhmann-style ID."
  (interactive)
  (let* ((vertico-count 15)
         (list (directory-files zk-directory t "{"))
         (file
          (completing-read
           "Select File: "
           (lambda (string predicate action)
             (if (eq action 'metadata)
                 `(metadata
                   (category . zk-file)
                   (group-function . zk--luhmann-group-function)
                   (display-sort-function . zk--luhmann-sort-function))
               (complete-with-action action list string predicate))))))
    (find-file file)))

(defun zk--luhmann-group-function (cand transform)
  "TRANSFORM each CAND for 'zk-luhmann'."
  (if transform
      (progn
        (string-match (concat "\\(?1:"
                            zk-id-regexp
                            "\\).\\(?2:.*?\\."
                            zk-file-extension
                            ".*\\)")
                      cand)
        (match-string 2 cand))
    "Luhmann Notes"))

(defun zk--luhmann-sort-function (list)
  "Sort LIST for 'zk-luhmann'."
  (sort list
        (lambda (a b)
          (let ((one
                 (progn
                   (string-match "{[^ ]*" a)
                   (match-string 0 a)))
                (two
                 (progn
                   (string-match "{[^ ]*" b)
                   (match-string 0 b))))
            (string< one two)))))

(defun zk-luhmann-completion-at-point ()
  "Completion at point function for notes with Luhmann-style IDs."
  (let ((case-fold-search t)
        (pt (point)))
    (save-excursion
      (save-match-data
        (when (re-search-backward "{" nil t)
          (list (match-beginning 0)
                pt
                (zk--luhmann-files)
                :exclusive 'no))))))

(defun zk--luhmann-files ()
  "List notes with Luhmann-style IDs."
  (let* ((files (directory-files zk-directory t "{"))
         (output))
    (dolist (file files)
      (progn
        (string-match (concat "\\(?1:"
                              zk-id-regexp
                              "\\).\\(?2:.*?\\)\\."
                              zk-file-extension
                              ".*")
                      file)
        (let ((id (match-string 1 file))
              (title (match-string 2 file)))
          (when id
            (push (format-spec "%t [[%i]]"
                               `((?i . ,id)(?t . ,title)))
                  output)))))
    output))

(add-hook 'completion-at-point-functions #'zk-luhmann-completion-at-point 'append)

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
;;   (let ((files (zk--luhmann-sort-function
;;                 (zk--luhmann-files))))
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


;;; Backlinks and Forward Links Together

(defun zk-network ()
  "Find `zk-backlinks' and `zk-links-in-note' for current or selected note.
Backlinks and Links-in-Note are grouped separately."
  (interactive)
  (let* ((id (ignore-errors (zk--current-id)))
         (backlinks (ignore-errors (zk--backlinks-list id)))
         (links-in-note (ignore-errors (zk--links-in-note-list id)))
         (resources))
    (dolist (file backlinks)
      (push (propertize file 'type 'backlink) resources))
    (dolist (file links-in-note)
      ;; abbreviate-file-name allows a file to be in both groups
      (push (propertize (abbreviate-file-name file) 'type 'link) resources))
    (find-file
     (completing-read
      "Links: "
      (lambda (string predicate action)
        (if (eq action 'metadata)
            `(metadata
              (group-function . zk--network-group-function)
              (display-sort-function . zk--network-sort-function)
              (category . zk-file))
          (complete-with-action action resources string predicate)))))))

(defun zk--network-group-function (file transform)
  "Group FILE by type or TRANSFORM."
  (if transform
      (file-name-nondirectory file)
    (cond
     ((eq 'backlink (get-text-property 0 'type file)) "Backlinks")
     ((eq 'link (get-text-property 0 'type file)) "Links-in-Note"))))

(defun zk--network-sort-function (list)
  "Sort LIST of links so Backlinks group is first."
  (sort list
        (lambda (a _b)
          (when (eq 'backlink (get-text-property 0 'type a))
              t))))

;;; Find Dead Links and Orphan Notes

(defun zk--grep-link-list ()
  "Return list of all ids that appear as links in zk directory."
  (let* ((files (shell-command-to-string (concat
                                          "grep -ohir -e "
                                          (shell-quote-argument
                                           "\\[[0-9]\\{12\\}]")
                                          " "
                                          zk-directory " 2>/dev/null")))
         (list (split-string files "\n" t "[][]")))
    (delete-dups list)))

(defun zk--dead-link-list ()
  "Return list of all links with no corresponding note."
  (let* ((all-links (zk--grep-link-list))
         (all-ids (zk--id-list)))
    (delete-dups (remq nil (mapcar
                            (lambda (x)
                              (string-match zk-id-regexp x)
                              (when (not (member (match-string-no-properties 0 x) all-ids))
                                x))
                            all-links)))))

(defun zk-grep-dead-links ()
  "Search for dead links using 'zk-search-function'."
  (interactive)
  (let ((dead-links (zk--dead-link-list)))
    (if dead-links
        (funcall zk-grep-function (mapconcat
                                     'identity
                                     dead-links
                                     "\\|"))
      (user-error "No dead links found"))))

(defun zk--unlinked-notes-list ()
  "Return list of IDs for notes that no notes link to."
  (let* ((all-links (zk--grep-link-list))
         (all-ids (zk--id-list)))
    (remq nil (mapcar
               (lambda (x)
                 (when (and (not (member x all-links))
                            ;; filter Dickinson notes
                            (not (string-match "^20201210" x)))
                   x))
               all-ids))))

(defun zk-unlinked-notes ()
  "Find unlinked notes."
  (interactive)
  (let ((notes
        (mapcar
         (lambda (id)
           (zk--parse-id 'file-path id))
         (zk--unlinked-notes-list))))
    (if notes
        (find-file (zk--select-file "Unlinked notes: " notes))
      (user-error "No unlinked notes found"))))

(provide 'zk-extras)
;;; zk-extras.el ends here
