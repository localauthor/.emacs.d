;;; devonthink-dir.el --- Support for accessing files in Devonthink  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; (require 'citar)
;; (require 'consult)
;; (require 'org)

(defvar devonthink-dir)

(setq devonthink-dir "~/Databases/Academic Work.dtBase2/Files.noindex/")

(defun devonthink-dir-find-file (key-entry &optional initial)
  "Search devonthink archive for file, using `consult-find'."
  (interactive (list (citar-select-ref)))
  (let ((key (car key-entry)))
    (if key
        (consult-find devonthink-dir (format "%s" key))
      (consult-find devonthink-dir initial))))

;; DevonThink Links
;; obsolute, use org-link-set-parameters instead
(org-add-link-type "x-devonthink-item" 'org-devonthink-item-open)

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

(provide 'devonthink-dir)
;;; dir-devonthink.el ends here
