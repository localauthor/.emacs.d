;;; consult-macos-finder-tags.el --- Search for files by finder tags     -*- lexical-binding: t; -*-


;;; Commentary:

;; Comment

;;; Code:

(require 'consult)

(defun consult-macos-finder-tags-list ()
  "Return a list of all unique Finder tags on macOS."
  (let* ((cmd "/usr/bin/mdfind -0 'kMDItemUserTags == \"*\"' | \
xargs -0 mdls -name kMDItemUserTags | \
awk '/^    /{print substr($0,5)}' | \
cut -d, -f1 | \
sed 's/^ *//;s/ *$//' | \
sort -u")
         (output (shell-command-to-string cmd))
         (tags (split-string output "\n" t "\"")))
    tags))

(defun consult--macos-finder-tags-builder (input)
  "Build command line from INPUT."
  (list (append (consult--build-args "mdfind")
                (list (format "kMDItemUserTags == '%s'" input)))))

(defun consult-macos-finder-tags--group-function (file transform)
  "TRANSFORM completion candidate FILE."
  (if transform
      (file-name-nondirectory file)
    (file-name-extension file)))

(defun consult--mdfind-find (prompt builder initial)
  "Run find command in current directory.

The function returns the selected file.
The filename at point is added to the future history.

BUILDER is the command line builder function.
PROMPT is the prompt.
INITIAL is initial input."
  (consult--read
   (consult--process-collection builder
     :transform (consult--async-map (lambda (x) (string-remove-prefix "./" x)))
     :highlight t :file-handler t) ;; allow tramp
   :prompt prompt
   :sort nil
   :require-match t
   :initial initial
   :group #'consult-macos-finder-tags--group-function
   :add-history (thing-at-point 'filename)
   :category 'file
   :history '(:input consult--find-history)))

;;;###autoload
(defun consult-macos-finder-tags (&optional initial)
  "Search with `mdfind' for files with MacOS finder tag INITIAL."
  (interactive (list (completing-read "Tag: " (consult-macos-finder-tags-list) nil t)))
  (let ((default-directory "~/"))
    (find-file (consult--mdfind-find "Files: " #'consult--macos-finder-tags-builder (shell-quote-argument initial)))))


;;; multiple tags

;; (defun consult--macos-finder-tags-builder (input)
;;   "Build command line from INPUT."
;;   (list (append (consult--build-args "mdfind")
;;                 (list (format "kMDItemUserTags == %s" (mapconcat (lambda (tag) (format "'%s'" tag)) (split-string input) " "))))))

;; (defun consult-macos-finder-tags (input)
;;   "Search with `mdfind' for files with MacOS finder tag INPUT."
;;   (interactive (list (completing-read-multiple "Tag: " (consult-macos-finder-tags-list) nil t)))
;;   (find-file (consult--find "Files: " #'consult--macos-finder-tags-builder (string-join (ensure-list input) " "))))

(provide 'consult-macos-finder-tags)
;;; consult-macos-finder-tags.el ends here
