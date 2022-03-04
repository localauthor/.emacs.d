;;; zk-extras.el --- Extra functions for zk.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'zk)
(require 'zk-index)
(require 'dickinson)
(require 'vertico)

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

(defun zk-word-count (&optional files)
  "Report word count of all files in 'zk-directory'.
Optionally takes list of FILES."
  (interactive)
  (let* ((files (if files files
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

(defun zk-lit-notes ()
  "Find literature note."
  (interactive)
  (find-file (zk--select-file "Lit notes: " (zk-lit-notes-list))))

(defun zk-lit-notes-list ()
  "Return list of literature notes."
  (interactive)
  (let* ((ed-notes (zk--directory-files t gr/dickinson-ref-regexp)))
    (remq nil (mapcar
               (lambda (x)
                 (unless (member x ed-notes)
                   x))
               (zk--directory-files t "[a-z]+[0-9]\\{4\\}")))))

(defun zk-lit-notes-index ()
  "List lit notes in ZK-Index"
  (interactive)
  (zk-index (zk-lit-notes-list)))

(defun zk-luhmann-word-count ()
  (interactive)
  (zk-word-count (zk--directory-files t "{"))) ;; not general

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
         (journal (length (zk--grep-file-list "journalentry")))
         (poem (length (zk--grep-file-list "mypoem")))
         (notes (- (length all-notes)
                   (+ journal poem
                      (length luhmann-notes)
                      (length lit-notes)
                      (length ed-notes)))))
    (message (format "Notes: %s | Luhmann: %s | Lit: %s"
                     notes
                     (length luhmann-notes)
                     (length lit-notes)))))

(defun zk-non-luhmann-list ()
  "Index listing of non-Luhmann notes.
Also excludes, journal, poem, Dickinson, and literature notes."
  (let* ((all-notes (zk--directory-files t))
         (ed-notes (zk--directory-files t gr/dickinson-ref-regexp))
         (luhmann-notes (zk--directory-files t "{"))
         (lit-notes (zk--directory-files t "[a-z]+[0-9]\\{4\\}"))
         (film (zk--grep-file-list "filmnotes"))
         (personal (zk--grep-file-list "#personal"))
         (creative (zk--grep-file-list "#creative"))
         (songs (zk--grep-file-list "#song"))
         (booknote (zk--grep-file-list "#booknote"))
         (journal (zk--grep-file-list "journalentry"))
         (poem (zk--grep-file-list "mypoem"))
         (notes
          (append ed-notes
                  booknote
                  luhmann-notes
                  film
                  songs
                  creative
                  personal
                  lit-notes
                  journal
                  poem)))
    (message "%s" (length (delete-dups notes)))
    (remq nil (mapcar
               (lambda (x)
                 (unless (member x notes)
                   x))
               all-notes))))

(defun zk-non-luhmann-index ()
  (interactive)
  (zk-index (zk-non-luhmann-list)))

(defun zk-non-luhmann-word-count ()
  (interactive)
  (zk-word-count (zk-non-luhmann-list)))

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

(defun zk-core-index ()
  (interactive)
  (zk-index (zk-core-list)))

(defun zk-core-count ()
  (interactive)
  (zk-word-count (zk-core-list)))

;;; Unlinked Notes

(defun gr/zk--unlinked-notes-list ()
  "Return list of IDs for notes that no notes link to."
  (let* ((all-link-ids (zk--grep-link-id-list))
         (all-ids (zk--id-list)))
    (remq nil (mapcar
               (lambda (x)
                 (when (and (not (member x all-link-ids))
                            ;; filter Dickinson notes
                            (not (string-match "^20201210" x)))
                   x))
               all-ids))))

(defun gr/zk-unlinked-notes ()
  "Find unlinked notes, minus ED notes."
  (interactive)
  (let* ((ids (gr/zk--unlinked-notes-list))
         (notes (zk--parse-id 'file-path ids)))
    (if notes
        (find-file (zk--select-file "Unlinked notes: " notes))
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
