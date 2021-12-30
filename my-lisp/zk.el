;;; zk.el --- Functions to setup a Zettelkasten with no backend    -*- lexical-binding: t; -*-


;;; Variables

(defvar zk-directory "~/Dropbox/Zettelkasten/Zettels")
(defvar zk-file-extension "md")
(defvar zk-id-regexp "[0-9]\\{12\\}")
(defvar zk-id-format "%Y%m%d%H%M")
(defvar zk-insert-link-format "[%s] [[%s]]")
(defvar zk-link-format "[[%s]]")
(defvar zk-tag-regexp "[#][[:alnum:]_-]+")


;;; Low-Level Functions

(defun zk--generate-id () ;; why these arguments?
  "Generate and return a note ID.
The ID is created using `zk-id-format'."
  (let ((id (format-time-string zk-id-format)))
    (while (zk--id-unavailable-p id)
      (setq id (1+ (string-to-number id)))
      (setq id (number-to-string id)))
    id))

(defun zk--id-list ()
  "Return list ids for all notes in 'zk-directory'."
  (let* ((files (directory-files zk-directory t zk-id-regexp))
         (all-ids))
    (dolist (file files)
      (progn
        (string-match zk-id-regexp file)
        (push (match-string 0 file) all-ids)))
    all-ids))

(defun zk--id-unavailable-p (str)
  "Return t if provided string STR is already in use as an id."
  (let ((all-ids (zk--id-list)))
    (member str all-ids)))

(defun zk--current-id ()
  "Return id of current note."
  (if (not (string=
            default-directory
            (expand-file-name (concat zk-directory "/"))))
      (error "Not a zk file")
    (string-match zk-id-regexp buffer-file-name))
  (match-string 0 buffer-file-name))

(defun zk--grep-file-list (str)
  "Return a list of files containing STR."
  (let* ((files (shell-command-to-string (concat
                                          "grep -lir -e "
                                          (regexp-quote str)
                                          " "
                                          zk-directory
                                          " 2>/dev/null")))
         (list (split-string files "\n" t)))
    (if (null list)
        (error (format "No results for \"%s\"" str))
      (mapcar
       (lambda (x)
         (abbreviate-file-name x))
       list))))

(defun zk--grep-link-list ()
  (let* ((files (directory-files zk-directory t zk-id-regexp))
         (all-ids)
         (file-count 1)
         (link-count 1))
    (dolist (file files)
      (let ((small-list
             (delete-dups
              (split-string
               (shell-command-to-string
                (concat
                 "tail -n +2 " (shell-quote-argument file)
                 " | grep -oh "(regexp-quote zk-id-regexp)
                 " 2>/dev/null"))))))
        (dolist (x small-list)
          (push x all-ids)
          (setq link-count (+ 1 link-count))))
      (when (zerop (% file-count 500))
        (message (format "Files processed: %s" file-count)))
      (setq file-count (+ 1 file-count)))
    (message (format "Complete. Files: %s. Links: %s" file-count link-count))
    all-ids))

(defun zk--grep-tag-list ()
  (let* ((files (shell-command-to-string (concat
                                          "grep -ohir -e '#[a-z0-9]\\+' "
                                          zk-directory " 2>/dev/null")))
         (list (split-string files "\n" t)))
    (delete-dups list)))

(defun zk--select-file (&optional list)
  "Wrapper around completing-read to select zk-file from LIST."
  (let* ((list (if list list
                 (directory-files zk-directory t zk-id-regexp)))
         (files (mapcar
                 (lambda (x)
                   (abbreviate-file-name x))
                 list)))
    (completing-read
     "Select File: "
     (lambda (string predicate action)
       (if (eq action 'metadata)
           `(metadata
             (category . zk-file))
         (complete-with-action action files string predicate))))))

(defun zk--parse-id (target id)
  "Return TARGET, either 'file-path, 'file-name, or 'title, from file with ID."
  (let ((file (car (directory-files zk-directory nil id)))
        (return (pcase target
                  ('file-path '0)
                  ('file-name '0)
                  ('title '2))))
    (if file
        (progn
          (string-match (concat "\\(?1:"
                              zk-id-regexp
                              "\\).\\(?2:.*?\\)\\..*")
                        file)
          (if (eq target 'file-path)
              (concat zk-directory "/" (match-string return file))
            (match-string return file)))
      (error (format "No file associated with %s" id)))))

(defun zk--parse-file (target file)
  "Return TARGET, either 'id or 'title, from file.

A note's title is understood to be the portion of its filename
following the ID, in the format 'zk-id-regexp', and preceding the
file extension."
  (let ((return (pcase target
                  ('id '1)
                  ('title '2))))
    (string-match (concat "\\(?1:"
                          zk-id-regexp
                          "\\).\\(?2:.*?\\)\\..*")
                  file)
    (match-string return file)))



;;; Find File

(defun zk-find-file-by-title ()
  "Search for title and open file in 'zk-directory'."
  (interactive)
  (find-file (zk--select-file)))

(defun zk-find-file-by-id (id)
  "Open file associated with ID."
  (find-file (zk--parse-id 'file-path id)))

(defun zk-find-file-by-full-text-search (str)
  "Search for and open file containing STR."
  (interactive
   (list (read-string "Search string: ")))
  (let ((choice 
         (completing-read
          (format "Files containing \"%s\": " str)
          (zk--grep-file-list str) nil t)))
    (find-file choice)))



;;; Note Functions

(defun zk-new-note (&optional title)
  "Create a new note, insert link at point, and backlink."
  (interactive)
  (let* ((orig-id (ignore-errors (zk--current-id)))
         (text (when (use-region-p)
                 (buffer-substring
                  (region-beginning)
                  (region-end))))
         (new-title (when (use-region-p)
                      (with-temp-buffer
                        (insert text)
                        (goto-char (point-min))
                        (push-mark)
                        (goto-char (line-end-position))
                        (buffer-substring
                         (region-beginning)
                         (region-end)))))
         (body (when (use-region-p)
                 (with-temp-buffer
                   (insert text)
                   (goto-line 3)
                   (push-mark)
                   (goto-char (point-max))
                   (buffer-substring
                    (region-beginning)
                    (region-end))))))
    (cond ((and (not title) (not new-title))
           (setq title (read-string "Note title: ")))
          (new-title
           (setq title new-title)))
    (setq new-id (zk--generate-id))
    (when (use-region-p)
      (kill-region (region-beginning) (region-end)))
    (when orig-id
      (insert (format zk-insert-link-format title new-id)))
    (find-file (concat (format "%s/%s %s.%s"
                               zk-directory
                               new-id
                               title
                               zk-file-extension)))
    (insert (format "# [[%s]] %s \n===\ntags: \n===\n<- " new-id title))
    (if orig-id
        (zk-insert-link orig-id t)
      (zk-insert-link "201801190001" t))
    (insert "\n===\n\n")
    (when body (insert body))
    (save-buffer)))


(defun zk-rename-note ()
  (interactive)
  ;; set id with cond to account for diff starting points
  ;; current file; minibuffer?; id at point?
  (let* ((id (zk--current-id))
         (orig-title (zk--parse-id 'title id))
         (new-title (read-string "New title: "))
         (new-file (concat
                    zk-directory "/"
                    id " "
                    new-title
                    "." zk-file-extension)))
    (save-excursion
      (rename-file buffer-file-name new-file t)        
      (goto-char (point-min))
      (while (re-search-forward orig-title nil t 1)
        (progn
          (replace-match new-title nil nil)
          (goto-char (point-max))))
      (set-visited-file-name new-file t t))))



;;; Insert Link

(defun zk-insert-link (id &optional incl-title)
  "Insert link to file.
With prefix-argument, or when INCL-TITLE is non-nil, include the title
without prompting."
  (interactive (list (zk--parse-file 'id (zk--select-file))))
  (let* ((pref-arg current-prefix-arg)
         (title (zk--parse-id 'title id)))
    (if (or incl-title
            (unless pref-arg
              (y-or-n-p "Include title? ")))
        (insert (format zk-insert-link-format title id))
      (insert (format zk-link-format id)))))



;;; Search

(defvar zk-search-function 'zk--grep)

(defun zk--grep (string)
  (lgrep string (concat "*." zk-file-extension) zk-directory))

(defun zk-search (string)
  (interactive "sSearch: ")
  (funcall zk-search-function string))



;;; List Backlinks

(defun zk-backlinks ()
  "Completing read list of files with links to current file."
  (interactive)
  (let* ((id (zk--current-id))
         (files (zk--grep-file-list id))
         (choice (zk--select-file files)))
    (find-file choice)))



;;; Tags

(defun zk-tag-search ()
  (interactive)
  (let ((tag (completing-read "Tag: " (zk--grep-tag-list))))
    (zk--grep tag)))

(defun zk-tag-insert ()
  (interactive)
  (let ((tag (completing-read "Tag: " (zk--grep-tag-list))))
    (insert tag)))


    
;;; Find Dead Links and Orphan Notes

(defun zk-list-dead-links ()
  (let* ((all-links (zk--grep-link-list))
         (all-ids (zk--id-list)))
    (remq nil (mapcar
               (lambda (x)
                 (when (not (member x all-ids))
                   x))
               all-links))))

(defun zk-list-orphan-notes ()
  (let* ((all-links (zk--grep-link-list))
         (all-ids (zk--id-list)))
    (remq nil (mapcar
               (lambda (x)
                 (when (and (not (member x all-links))
                            ;; removes Dickinson poem notes
                            (not (eq 0 (string-match "^20201210" x))))
                   x))
               all-ids))))



;;; Org-Link Integration
;; ie, click to follow ZK style links in org-mode

(defun zk-try-follow-id (orig-org-open-at-point &optional arg)
  (condition-case nil
      (apply orig-org-open-at-point arg)
    (error (zk-follow-id))))

(advice-add 'org-open-at-point :around #'zk-try-follow-id)

(defun zk-follow-id ()
  (interactive)
  (when (thing-at-point-looking-at zk-id-regexp)
    (find-file (zk--parse-id 'file-path (match-string-no-properties 0)))))



;;; Embark Integration

(defun embark-target-zk-id-at-point ()
  "Target zk-id at point."
  (when (thing-at-point-looking-at zk-id-regexp)
    (let ((zk-id (thing-at-point 'symbol t)))
      `(zk-id ,zk-id . ,(bounds-of-thing-at-point 'symbol)))))

(add-to-list 'embark-target-finders 'embark-target-zk-id-at-point)

(embark-define-keymap embark-zk-id-map
  "Keymap for Embark comment actions."
  ("RET" zk-find-file-by-id)
  ("r" zk-consult-ripgrep))

(add-to-list 'embark-keymap-alist '(zk-id . embark-zk-id-map))

(embark-define-keymap embark-zk-file-map
  "Keymap for Embark comment actions."
  ("i" zk-insert-link))

(add-to-list 'embark-keymap-alist '(zk-file . embark-zk-file-map))



;;; Link-Hint Integration

(defun link-hint--zk-id-at-point-p ()
  (thing-at-point-looking-at zk-id-regexp))

(defun link-hint--next-zk-id (&optional bound)
  (link-hint--next-regexp zk-id-regexp bound))

(defun link-hint--open-zk-id ()
  (zk-follow-id))

(define-link-hint-aw-select zk-id zk-follow-id)

(link-hint-define-type 'zk-id
  :next #'link-hint--next-zk-id
  :at-point-p #'link-hint--zk-id-at-point-p
  :open #'link-hint--open-zk-id
  :copy #'kill-new
  :aw-select #'link-hint--aw-select-zk-id)  ;; not working

(push 'link-hint-zk-id link-hint-types)



;;; Consult Functions

;; separate into zk-consult.el

(defun zk-consult-ripgrep (&optional key)
  (interactive "sRipgrep: ")
  (if key
      (consult-ripgrep zk-directory (format "%s" key))
    (consult-ripgrep zk-directory)))

(defun zk-consult-grep-search-tag ()
  (interactive)
  (let* ((tag (completing-read "Tag: " (zk--grep-tag-list))))
    (consult-grep zk-directory tag)))

(provide 'zk)
