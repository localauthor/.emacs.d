;;; zk.el --- Functions to deal with link-connected notes, with no backend -*- lexical-binding: t; -*-

;;; Commentary:

;; This set of functions aims to implement many (but not all) of the features
;; of the package 'Zetteldeft' while circumventing and eliminating that
;; package's dependency on 'Deft'. It also eschews the use of any backend
;; cache or database, preferring instead to query a directory of notes
;; directly, thereby treating and utiliziing that directory as a sufficient
;; database unto itself.

;; To that end, these functions rely, at the lowest level, on simple calls to
;; 'grep', which returns lists of files, links, and tags to
;; 'completing-read', from which existing files can be opened and links and
;; tags can be inserted into an open buffer.

;; The primary connector between notes is the simple link, which takes the
;; form of an ID number enclosed in double-brackets, eg, [[202012091130]]. A
;; note's ID number, by default, is a twelve-digit string corresponding to
;; the date and time the note was originally created. For example, a note
;; created on December 9th, 2020 at 11:30 would have the ID "202012091130".
;; Linking to such note involves nothing more than placing the string
;; [[202012091130]] into another note in the directory.

;; A note's filename is constructed as follows: the ID number followed by the
;; title of the note followed by the file extension, e.g. "202012091130 On
;; the origin of species.txt". A key consequence of this ID/linking scheme is
;; that a note's title can change without any existing links to the note
;; being broken, wherever they might be throughout the directory.

;; The directory is a single folder containing all notes.

;; The structural simplicity of this set of functions is---one hopes---in
;; line with the structural simplicity of the so-called "Zettelkasten
;; method," of which much can be read in many places, including at
;; https://www.zettelkasten.de.

;; There are several ways to follow links. The most basic way, which works in
;; any mode, is to simply call the function 'zk-follow-id-at-point' with the
;; point on an ID. This function could be bound to a convenient key. Other
;; ways of following links rely on external packages. If notes are in
;; 'org-mode', load the file 'zk-org.el' to enable click-to-follow links. If
;; 'embark' is installed, load 'zk-embark.el' to enable 'embark-act' to
;; target links at point as well as filenames in a completion interface. If 'link-hint' is installed, load 'zk-link-hint.el' 


;;; Code:

(require 'grep)


;;; Variables

(defvar zk-directory nil)
(defvar zk-file-extension nil)
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
  "Return list of all links matching 'zk-id-regexp' in all files in 'zk-directory'."
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
  "Return list of tags from all notes in zk directory."
  (let* ((files (shell-command-to-string (concat
                                          "grep -ohir -e '#[a-z0-9]\\+' "
                                          zk-directory " 2>/dev/null")))
         (list (split-string files "\n" t)))
    (delete-dups list)))

(defun zk--select-file (&optional list)
  "Wrapper around `completing-read' to select zk-file from LIST."
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
  "Return TARGET, either 'id or 'title, from FILE.

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

;;;###autoload
(defun zk-find-file-by-title ()
  "Search for title and open file in 'zk-directory'."
  (interactive)
  (find-file (zk--select-file)))

;;;###autoload
(defun zk-find-file-by-id (id)
  "Open file associated with ID."
  (find-file (zk--parse-id 'file-path id)))

;;;###autoload
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

;;;###autoload
(defun zk-new-note (&optional title)
  "Create a new note, insert link at point, and backlink.
Optional argument TITLE ."
  (interactive)
  (let* ((new-id (zk--generate-id))
         (orig-id (ignore-errors (zk--current-id)))
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
                   (goto-char (point-min))
                   (forward-line 2)
                   (push-mark)
                   (goto-char (point-max))
                   (buffer-substring
                    (region-beginning)
                    (region-end))))))
    (cond ((and (not title) (not new-title))
           (setq title (read-string "Note title: ")))
          (new-title
           (setq title new-title)))
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
  "Rename current note and replace original title in header, if found."
  (interactive)
  (let* ((id (zk--current-id))
         (orig-title (zk--parse-id 'title id))
         (new-title (read-string "New title: " orig-title))
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
          (replace-match new-title)
          (goto-char (point-max))))
      (set-visited-file-name new-file t t))))


;;; Insert Link

;;;###autoload
(defun zk-insert-link (id &optional incl-title)
  "Insert ID link to note using 'completing-read', with prompt to include title.
With prefix-argument, or when INCL-TITLE is non-nil, include the
title without prompting."
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
  "Wrapper around 'lgrep' to search for STRING in all notes.
Opens search results in grep buffer."
  (lgrep string (concat "*." zk-file-extension) zk-directory nil))

(defun zk-search (string)
  "Search for STRING using function set in 'zk-search-function'."
  (interactive "sSearch: ")
  (funcall zk-search-function string))

;;; List Backlinks

(defun zk-backlinks ()
  "Select from list of all notes that link to the current note."
  (interactive)
  (let* ((id (zk--current-id))
         (files (zk--grep-file-list id))
         (choice (zk--select-file (remove (zk--parse-id 'file-path id) files))))
    (find-file choice)))

;;; Tag Functions

(defun zk-tag-search (tag)
  "Open grep buffer containing results of search for TAG.
Select TAG, with completion, from list of all tags in zk notes."
  (interactive (list (completing-read "Tag: " (zk--grep-tag-list))))
  (zk--grep tag))

(defun zk-tag-insert (tag)
  "Insert TAG at point.
Select TAG, with completion, from list of all tags in zk notes."
  (interactive (list (completing-read "Tag: " (zk--grep-tag-list))))
  (insert tag))

    
;;; Find Dead Links and Orphan Notes

(defun zk-list-dead-links ()
  "Return list of all links with no corresponding note."
  (let* ((all-links (zk--grep-link-list))
         (all-ids (zk--id-list)))
    (remq nil (mapcar
               (lambda (x)
                 (when (not (member x all-ids))
                   x))
               all-links))))

(defun zk-list-orphan-notes ()
  "Return list of notes that no other notes link to."
  (let* ((all-links (zk--grep-link-list))
         (all-ids (zk--id-list)))
    (remq nil (mapcar
               (lambda (x)
                 (when (and (not (member x all-links))
                            ;; removes ED poem notes
                            (not (eq 0 (string-match "^20201210" x))))
                   x))
               all-ids))))


;;; Follow ID at Point

(defun zk-follow-id-at-point ()
  (interactive)
  (when (thing-at-point-looking-at zk-id-regexp)
    (find-file (zk--parse-id 'file-path (match-string-no-properties 0)))))



;;; Org-Link Integration

;; adds click-to-follow links in org-mode

(require 'org)

(defun zk-try-to-follow-id (orig-org-open-at-point &optional arg)
  (condition-case nil
      (apply orig-org-open-at-point arg)
    (error (zk-follow-id-at-point))))

(advice-add 'org-open-at-point :around #'zk-try-to-follow-id)



;;; Embark Integration

(require 'embark)

;;;###autoload
(defun embark-target-zk-id-at-point ()
  "Target zk-id at point."
  (when (thing-at-point-looking-at zk-id-regexp)
    (let ((zk-id (thing-at-point 'symbol t)))
      `(zk-id ,zk-id . ,(bounds-of-thing-at-point 'symbol)))))

(add-to-list 'embark-target-finders 'embark-target-zk-id-at-point)

(embark-define-keymap embark-zk-id-map
  "Keymap for Embark zk-id actions
To be used on zk-ids at point in buffers."
  ("RET" zk-follow-id-at-point)
  ("r" zk-consult-ripgrep))

(add-to-list 'embark-keymap-alist '(zk-id . embark-zk-id-map))

(embark-define-keymap embark-zk-file-map
  "Keymap for Embark zk-file actions.
To be used in the context of filename completion, as in the minibuffer."
  ("i" zk-insert-link)
  ("f" zk-find-file-by-title))

(add-to-list 'embark-keymap-alist '(zk-file . embark-zk-file-map))



;;; Link-Hint Integration

(require 'link-hint)

(defun link-hint--zk-id-at-point-p ()
  (thing-at-point-looking-at zk-id-regexp))

(defun link-hint--next-zk-id (&optional bound)
  (link-hint--next-regexp zk-id-regexp bound))

(defun link-hint--open-zk-id ()
  (zk-follow-id-at-point))

(link-hint-define-type 'zk-id
  :next #'link-hint--next-zk-id
  :at-point-p #'link-hint--zk-id-at-point-p
  :open #'link-hint--open-zk-id
  :copy #'kill-new)

(push 'link-hint-zk-id link-hint-types)


;;; Consult Functions

(require 'consult)

;; separate into zk-consult.el

(defun zk-consult-ripgrep (&optional initial)
  "Search 'zk-directory' with 'consult-ripgrep'.
With option for INITIAL input when called non-interactively."
  (interactive "sRipgrep: ")
  (if initial
      (consult-ripgrep zk-directory (format "%s" initial))
    (consult-ripgrep zk-directory)))

(defun zk-consult-grep-search-tag (tag)
  "Search for TAG in 'zk-directory' using 'consult-grep'.
Select TAG, with completion, from list of all tags in zk notes."
  (interactive (list (completing-read "Tag: " (zk--grep-tag-list))))
  (consult-grep zk-directory tag))



(provide 'zk)
;;; zk.el ends here
