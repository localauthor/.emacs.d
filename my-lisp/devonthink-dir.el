;;; devonthink-dir.el --- Support for accessing files in Devonthink  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; (require 'citar)
;; (require 'consult)
;; (require 'org)

(defvar devonthink-dir)

(setq devonthink-dir "~/Databases/Academic.dtBase2/Files.noindex/")

(defun devonthink-set-dir ()
  "Set DEVONthink DB."
  (interactive)
  (let ((db (completing-read "Choose: " '(Academic Personal) nil t)))
    (setq devonthink-dir (format "~/Databases/%s.dtBase2/Files.noindex/" db))))

;; (defun devonthink-dir-find-file (key-entry &optional initial)
;;   "Search devonthink archive for file, using `consult-find'."
;;   (interactive (list (citar-select-ref)))
;;   (let ((key (car key-entry)))
;;     (if key
;;         (consult-find devonthink-dir (format "%s" key))
;;       (consult-find devonthink-dir initial))))

(defun devonthink-dir-find-file (&optional initial)
  "Search devonthink archive for file, using `consult-find'."
  (interactive)
  (when (equal current-prefix-arg '(4))
    (devonthink-set-dir))
  (consult-find devonthink-dir initial))

(defun org-devonthink-setup ()
  "Register org-devonthink link type"
  (interactive)
  (org-link-set-parameters "x-devonthink-item"
                           :follow 'org-devonthink-item-open))

(defun org-devonthink-item-open (uid)
  "Open the given UID, which is a reference to an item in Devonthink."
  (shell-command (concat "open \"x-devonthink-item:" uid "\"")))

;; (defun gr/devonthink-find-file (keys-entries)
;;   "Search devonthink archive for file, using `consult-find'."
;;   (interactive (list (citar-select-refs :rebuild-cache current-prefix-arg)))
;;   (kill-new (or (car (citar--extract-keys keys-entries)) ""))
;;   (minibuffer-with-setup-hook
;;       'yank
;;     (define-key embark-file-map (kbd "RET") 'find-file)
;;     (consult-find gr/devonthink-dir)))


(defun consult-ripgrep-devonthink ()
  (interactive)
  (let ((consult-ripgrep-args "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number .")
        (vertico-count 15)
        (consult-preview-key nil))
    (consult-ripgrep gr/pdf-directory)))


(provide 'devonthink-dir)
;;; dir-devonthink.el ends here
