;;; zk-extras.el --- Extra functions for zk.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'zk)
(require 'zk-index)
(require 'zk-desktop)
(require 'zk-luhmann)
(require 'link-hint)

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
  (if-let (matches (xref-matches-in-files
                    string
                    (zk--directory-files t)))
      (xref--show-xrefs matches 'display-buffer-at-bottom)
    (error "No matches")))

;; (setq xref-show-xrefs-function 'consult-xref)


;;; clickable tags

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
                     (zk-tag-search ,tag)))
      (funcall 'zk-tag-fontify tag beg end map)
      t)))

(defun zk-tag-fontify (tag beg end map)
  (add-text-properties beg end
                       `(face link
                              mouse-face highlight
                              help-echo "Right click to search tag"
                              keymap ,map)))

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

(defun zk-follow-link-other-tab (&optional id)
  "Open note that corresponds with the zk ID at point."
  (interactive)
  (let ((id (or (zk--id-at-point)
                id)))
    (if id
        (find-file-other-tab (zk--parse-id 'file-path id))
      (error "No zk-link at point"))))


;;;###autoload
(defun zk-index-aw-select ()
  (interactive)
  ;; use link-hint--aw-select-button instead??
  (let ((aw-ignore-current t)
        (aw-ignored-buffers link-hint-aw-select-ignored-buffers)
        (id (zk-index--button-at-point-p)))
    (link-hint--aw-select-zk-link id)))


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
(defun gr/zk--unlinked-notes-list (zk-alist)
  "Return list of IDs for notes that no notes link to.
Takes ZK-ALIST."
  (let* ((all-link-ids (zk--grep-link-id-list))
         (all-ids (zk--id-list)))
    (remq nil (mapcar
               (lambda (x)
                 (when (and (not (member x all-link-ids))
                            ;; filter Dickinson notes
                            (not (string-match "^20201210" x)))
                   x))
               all-ids))))

;;;###autoload
(defun gr/zk-unlinked-notes ()
  "Find unlinked notes, minus ED and lit notes."
  (interactive)
  (let* ((zk-alist (zk--alist))
         (ids (gr/zk--unlinked-notes-list zk-alist))
         (lit-notes (zk-lit-notes-list)))
    (if-let (notes (zk--parse-id 'file-path ids zk-alist))
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
