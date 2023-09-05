;;; devonthink-dir.el --- Support for accessing files in Devonthink  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar devonthink-dir "~/Databases/Academic.dtBase2/Files.noindex/")

(defvar devonthink-pdf-dir "~/Databases/Academic.dtBase2/Files.noindex/pdf/")

(defun devonthink-dir-find-file (&optional initial)
  "Search devonthink archive for file, using `consult-find'."
  (interactive)
  (when (equal current-prefix-arg '(4))
    (devonthink-set-dir))
  (consult-find devonthink-dir initial))

(defun devonthink-set-dir ()
  "Set DEVONthink DB."
  (interactive)
  (let ((db (completing-read "Choose: " '(Academic Personal) nil t)))
    (setq devonthink-dir (format "~/Databases/%s.dtBase2/Files.noindex/" db))))

(defun consult-ripgrep-devonthink ()
  (interactive)
  (let ((consult-ripgrep-args "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number .")
        (vertico-count 15)
        (consult-preview-key nil))
    (consult-ripgrep devonthink-pdf-dir)))

(defun org-devonthink-setup ()
  "Register org-devonthink link type"
  (interactive)
  (org-link-set-parameters "x-devonthink-item"
                           :follow 'org-devonthink-item-open))

(defun org-devonthink-item-open (uid)
  "Open the given UID, which is a reference to an item in Devonthink."
  (shell-command (concat "open \"x-devonthink-item:" uid "\"")))

(provide 'devonthink-dir)
;;; dir-devonthink.el ends here
