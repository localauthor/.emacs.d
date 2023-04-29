;;; misc-file-handling.el --- Misc. file and alist handling functions  -*- lexical-binding: t; -*-

(defun list-dirs-recursively (dir &optional include-symlinks)
  "Return list of all subdirectories of DIR recursively. Return absolute paths.
Optionally call recursively on symlinks when INCLUDE-SYMLINKS is `t`."
  (let ((result nil)
        (tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (file-name-all-completions "" dir))
      (when (and (directory-name-p file) (not (member file '("./" "../"))))
        (setq result (nconc result (list (expand-file-name file dir))))
        (let* ((leaf (substring file 0 (1- (length file))))
               (full-file (expand-file-name leaf dir)))
          ;; Don't follow symlinks to other directories.
          (unless (and (file-symlink-p full-file) (not include-symlinks))
            (setq result
                  (nconc result (list-dirs-recursively full-file)))))))
    result))

(defvar gr/pdf-directory)

(setq gr/pdf-directory "~/Databases/Academic Work.dtBase2/Files.noindex/pdf/")

(defun gr/pdf-directory-open-file (key-entry)
  (interactive (list (citar-select-ref)))
  (kill-new (or (car key-entry) ""))
  (minibuffer-with-setup-hook
      'yank
    (define-key embark-file-map (kbd "RET") 'find-file)
    (consult-find gr/pdf-directory)))

(defun gr/pdf-directory-open-file-externally (keys-entries)
  "Open pdf from 'gr/pdf-directory' in external application.
Alternative to using `consult-find', to allow built in open-externally function,
since `consult-find' passes a buffer name, not a file path."
  (interactive (list (citar-select-refs :rebuild-cache current-prefix-arg)))
  (if-let ((key (car (citar--extract-keys keys-entries)))
           (lst (directory-files-recursively gr/pdf-directory key)))
      (gr/open-file-externally (gr/pretty-file-alist lst) key)
    (let* ((lst (directory-files-recursively gr/pdf-directory "")))
      (gr/open-file-externally (gr/pretty-file-alist lst) key))))

(defun gr/select-from-alist (alist &optional input)
  "Use 'completing-read' to return a value from a list of keys ALIST.
Optional initial INPUT."
  (let* ((select (completing-read "Select File: " alist nil t input))
         (value (cdr (assq (intern select) alist))))
    (format "%s" value)))

(defun gr/open-file-externally (alist &optional input)
  (embark-open-externally (gr/select-from-alist alist input)))

(defun gr/pretty-file-alist (list)
;; embark actions don't work on files in this list, bc they are not full paths
  (cl-pairlis (mapcar
               (lambda (arg)
                 (file-name-nondirectory arg))
               list)
              list))

(defun gr/pretty-file-alist (list)
;; embark actions don't work on files in this list, bc they are not full paths
  (cl-pairlis (mapcar 'file-name-nondirectory list) list))



;; (defun gr/find-file-recursively (dir)
;;   "Open file from DIR in external application.
;; Ignores .git directories and .DS_STORE files."
;;   (interactive "DSelect directory: ")
;;   (let ((lst (directory-files-recursively dir "")))
;;     (setq lst (cl-remove-if (lambda (k)
;;                                (string-match "\\.git\\|\\.DS_STORE" k))
;;                              lst))
;;     (gr/open-file-externally (gr/pretty-file-alist lst))))

(defun gr/find-file-recursively (dir)
  "Open file from DIR in external application.
Ignores .git directories and .DS_STORE files."
  (interactive "DSelect directory: ")
  (let* ((full-list (cl-remove-if
                     (lambda (k)
                       (string-match "\\.git\\|\\.DS_STORE" k))
                     (directory-files-recursively dir "")))
         (list (mapcar
                (lambda (file)
                  (abbreviate-file-name file))
                full-list))
         (choice (completing-read
             "Select File: "
             (lambda (string predicate action)
               (if (eq action 'metadata)
                   `(metadata
                     (category . file))
                 (complete-with-action action list string predicate)))))))
    (find-file choice))

(provide 'misc-file-handling)
