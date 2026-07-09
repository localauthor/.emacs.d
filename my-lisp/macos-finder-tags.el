;;; macos-finder-tags.el --- Search for files by finder tags     -*- lexical-binding: t; -*-


;;; Commentary:

;; Requires the command line tool called tag, available
;; at https://github.com/jdberry/tag

;; options to add:
;; - specify directory (with C-u)
;; - directory recursive
;; - tag multiple files

;; features to add:
;; - confirmation message

;; issues:
;; - “tag --find” doesn’t look in hidden directories
;; - so a tagged elisp file isn’t really registered
;; - tag command line tool is abandonware, and doesn’t work on MacOS Tahoe

;;; Code:

(require 'consult)

(defvar macos-finder-tags-history nil)

;;; utilities

(defun macos-finder-tags--group-function (file transform)
  "TRANSFORM completion candidate FILE."
  (if transform
      (file-name-nondirectory file)
    (file-name-extension file)))

(defun macos-finder-tags-list ()
  "Return a list of all unique Finder tags on macOS."
  (let* ((cmd "tag --find '*' --tags --no-name -g")
         (output (shell-command-to-string cmd))
         (tags (split-string output "\n" t)))
    tags))

(defun macos-finder-tags--shell-format (tags)
  "Format TAGS list as shell-quoted, comma-separated string."
  (mapconcat
   (lambda (tag)
     (format "\"%s\"" tag))
   tags ","))

(defun macos-finder-tags--prompt-format (tags)
  "Format TAGS list as comma-separated string.
Suitable for initial value of `macos-finder-tags--prompt’."
  (mapconcat
   (lambda (tag)
     (format "%s" tag))
   tags ", "))

(defvar crm-separator)

(defun macos-finder-tags--prompt (&optional initial)
  "Completing-read prompt for all tags, with optional INITIAL."
  (let ((candidates (macos-finder-tags-list))
        (crm-separator "[ 	]*,[ 	]*")
        ;;(crm-prompt "%p")
        (completion-show-inline-help nil))
    (completing-read-multiple
     "Tags: "
     (completion-table-with-metadata
      (completion-table-dynamic
       (lambda (_)
         candidates))
      '((category . macos-tag)))
     nil nil initial 'macos-finder-tags-history)))

;;;###autoload
(defun macos-finder-tags (tags)
  "Find files with MacOS finder TAGS."
  (interactive
   (list (macos-finder-tags--prompt)))
  (let ((files (split-string
                (shell-command-to-string
                 (concat "tag --find "
                         (macos-finder-tags--shell-format tags)
                         " '*'"))
                "\n" t)))
    (find-file (completing-read
                (format "Tagged %s: "
                        (macos-finder-tags--shell-format tags))
                (completion-table-with-metadata
                 (completion-table-dynamic
                  (lambda (_)
                    files))
                 '((category . file)
                   (group-function . macos-finder-tags--group-function)))
                nil nil nil 'macos-finder-tags-history))))

;;; set add remove tags

(defun macos-finder-tags--file-tags (file)
  "Return list of tags for FILE."
  (split-string
   (shell-command-to-string
    (concat "tag --no-name "
            (shell-quote-argument (expand-file-name file))))
   "," t "\n"))

;;;###autoload
(defun macos-finder-tags-set-tags (file)
  "Interactively set tags for FILE."
  (interactive
   (list (read-file-name "File: ")))
  (let* ((file-tags (macos-finder-tags--file-tags file))
         (set-tags (macos-finder-tags--prompt
                    (concat
                     (macos-finder-tags--prompt-format file-tags)
                     (when file-tags ",")))))
    (shell-command
     (concat (if (null set-tags)
                 "tag --remove \\* "
               "tag --set ")
             (macos-finder-tags--shell-format set-tags)
             " "
             (shell-quote-argument (expand-file-name file))))))

(defun macos-finder-tags-select-tags (file &optional prompt)
  "List tags for FILE, with optional PROMPT."
  (completing-read-multiple
   (or prompt "Tags: ")
   (macos-finder-tags--file-tags file)))

(defun macos-finder-tags-add-tags (file &optional tags)
  "Add tag to FILE.
See `macos-finder-tags-set-tags’ for more interactive UI."
  (interactive
   (list (read-file-name "File: ")))
  (let ((tags (or tags
                  (macos-finder-tags--prompt))))
    (shell-command
     (concat "tag --add "
             (macos-finder-tags--shell-format tags)
             " "
             (shell-quote-argument (expand-file-name file))))))

(defun macos-finder-tags-remove-tags (file)
  "Remove tag from FILE."
  (interactive
   (list (read-file-name "File: ")))
  (let ((tags (macos-finder-tags-select-tags file "Remove: ")))
    (shell-command
     (concat "tag --remove "
             (macos-finder-tags--shell-format tags)
             " "
             (shell-quote-argument (expand-file-name file))))))


;;; embark integration

(defvar embark-file-map)

(define-key embark-file-map (kbd "T") #'macos-finder-tags-set-tags)
(define-key embark-file-map (kbd "R") #'macos-finder-tags-remove-tags)


;;; old

;; ;;;###autoload
;; (defun macos-finder-tags (tags)
;;   "Find files with MacOS finder TAGS."
;;   (interactive
;;    (list (macos-finder-tags--prompt)))
;;   (let ((files (split-string
;;                 (shell-command-to-string
;;                  (concat "tag --find "
;;                          (macos-finder-tags--shell-format tags)
;;                          " '*'"))
;;                 "\n" t)))
;;     (find-file (consult--read
;;                 files
;;                 :prompt "Select: "
;;                 :require-match nil
;;                 :group 'macos-finder-tags--group-function
;;                 :category 'file
;;                 :state (consult--file-preview)
;;                 :history 'macos-finder-tags-history))))

;; (defun consult-macos-tags--builder (input)
;;   "Build command line from INPUT."
;;   (list (append (consult--build-args "mdfind")
;;                 (list (format "kMDItemUserTags == '%s'" input)))))

;; (defun consult--mdfind-find (prompt builder initial)
;;   "Run find command in current directory.

;; The function returns the selected file.
;; The filename at point is added to the future history.

;; BUILDER is the command line builder function.
;; PROMPT is the prompt.
;; INITIAL is initial input."
;;   (consult--read
;;    (consult--process-collection builder
;;      :transform (consult--async-map (lambda (x) (string-remove-prefix "./" x)))
;;      :highlight t :file-handler t) ;; allow tramp
;;    :prompt prompt
;;    :sort nil
;;    :require-match t
;;    :initial initial
;;    :group #'consult-macos-tags--group-function
;;    :add-history (thing-at-point 'filename)
;;    :category 'file
;;    :history '(:input consult--find-history)))


;; (defun consult-macos-tags (&optional input)
;;   "Search with `mdfind' for files with MacOS finder tag INPUT."
;;   (interactive (list (completing-read "Tag: " (consult-macos-tags-list) nil t)))
;;   (let ((default-directory "~/"))
;;     (find-file (consult--mdfind-find "Files: " #'consult-macos-tags--builder (shell-quote-argument input)))))


;; (consult-macos-tags--prompt "")

;; (split-string-shell-command "banana sandwich blog")

;; (shell-command-to-string (consult-macos-tags--builder "Database"))

;; (defun consult-macos-tags (input)
;;   "Search with `mdfind' for files with MacOS finder tag INPUT."
;;   (interactive (list (completing-read-multiple "Tag: " (consult-macos-tags-list) nil t)))
;;   (find-file (consult--find "Files: " #'consult-macos-tags--builder (string-join (ensure-list input) " "))))

;; (defun consult-macos-tags (input)
;;   "Search with `mdfind' for files with MacOS finder tag INPUT."
;;   (interactive
;;    (list (consult-macos-tags--prompt "")))
;;   (find-file (consult--find "Files: " #'consult-macos-tags--builder (string-join (ensure-list input) " "))))

;;; with mdfind

;; (defun consult-macos-tags-list ()
;;   "Return a list of all unique Finder tags on macOS."
;;   (let* ((cmd "/usr/bin/mdfind -0 'kMDItemUserTags == \"*\"' | \
;; xargs -0 mdls -name kMDItemUserTags | \
;; awk '/^    /{print substr($0,5)}' | \
;; cut -d, -f1 | \
;; sed 's/^ *//;s/ *$//' | \
;; sort -u")
;;          (output (shell-command-to-string cmd))
;;          (tags (split-string output "\n" t)))
;;     tags))

;; (defun consult-macos-tags--builder (input)
;;   "Build command line from INPUT."
;;   (format "mdfind 'kMDItemUserTags == %s'"
;;           (mapconcat
;;            (lambda (tag)
;;              (format "%s" (consult-macos-tags--escape-chars tag)))
;;            input " ")))

;; (defvar consult-macos-tags--escape-chars '(("'" . "'\\''"))
;;   "Alist of characters to replace with escaped versions")

;; (defun consult-macos-tags--escape-chars (tag)
;;   "Replace special characters in TAG."
;;   (with-temp-buffer
;;     (insert tag)
;;     (format-replace-strings
;;      consult-macos-tags--escape-chars
;;      nil (point-min) (line-end-position))
;;     (buffer-substring-no-properties
;;      (point-min)
;;      (line-end-position))))

;; (defun consult-macos-tags--prompt ()
;;   (let ((candidates (consult-macos-tags-list))
;;         ;;(crm-separator "[ \w]+")
;;         (crm-prompt "%p")
;;         (completion-show-inline-help nil))
;;     (completing-read-multiple
;;      "Tags: "
;;      (completion-table-with-metadata
;;       (completion-table-dynamic
;;        (lambda (_)
;;          candidates))
;;       '((category . macos-tag)))
;;      nil t nil 'consult-macos-tags-history)))

;; ;;;###autoload
;; (defun consult-macos-tags (input)
;;   "Search with `mdfind' for files with MacOS finder tag INPUT."
;;   (interactive
;;    (list (consult-macos-tags--prompt)))
;;   (let ((files (split-string
;;                 (shell-command-to-string
;;                  (consult-macos-tags--builder input))
;;                 "\n" t)))
;;     (find-file (consult--read
;;                 files
;;                 :prompt "Select: "
;;                 :require-match nil
;;                 :group 'consult-macos-tags--group-function
;;                 :category 'file
;;                 :state (consult--file-preview)
;;                 :history 'consult-macos-tags-history))))


(provide 'macos-finder-tags)
;;; macos-finder-tags.el ends here
