;;; gr-database.el --- Support for accessing files select directories  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


;;; gr-database

(require 'dickinson)

(defcustom gr/database-dirs nil
  "List of directories to search for files.")

(defvar devonthink-academic-dir "~/Databases/Academic.dtBase2/Files.noindex/")

(defvar devonthink-personal-dir "~/Databases/Personal.dtBase2/Files.noindex/")

(defvar gr/current-term-dir "~/Dropbox/Spring 2026/")

(defvar voice-dream-dir "~/Library/Mobile Documents/iCloud~com~voicedream~reader/Documents/Library")

(defvar calibre-dir "~/Calibre Library/")

(setopt gr/database-dirs (list
                          zk-directory
                          devonthink-academic-dir
                          devonthink-personal-dir
                          ;;gr/current-term-dir
                          "~/Documents/"
                          ;; voice-dream-dir
                          calibre-dir
                          gr/dickinson-dir))

(defun gr/database-find-file (&optional initial)
  "Find file in `gr/database-dirs’ using `consult--find'."
  (interactive)
  (let* ((default-directory "~/")
         (paths (mapcar (lambda (p)
                          (file-relative-name (expand-file-name p)))
                        gr/database-dirs))
         (builder (consult--find-make-builder paths)))
    (find-file (gr/database-consult--find
                "Find: " builder initial))))

(defun gr/database-consult--find (prompt builder initial)
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
   :group #'gr/database--group-function
   :require-match t
   :initial initial
   :add-history (thing-at-point 'filename)
   :category 'file
   :history '(:input consult--find-history)))

(defun gr/database--group-function (file transform)
  "TRANSFORM completion candidate FILE."
  (if transform
      (if (or (file-directory-p file)
              (string-match "Databases/" file)
              (string-match "/" file)
              (file-in-directory-p file zk-directory))
          (file-name-nondirectory file)
        file)
    (cond ((file-in-directory-p file zk-directory)
           "zk")
          ((file-directory-p file)
           "dir")
          (t
           (file-name-extension file)))))

(defun gr/database-ripgrep-all ()
  "Search text in `gr/database-dirs’ using `ripgrep-all’."
  (interactive)
  (let ((consult-ripgrep-args "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number .")
        (vertico-count 15)
        (consult-preview-key nil))
    (consult-ripgrep gr/database-dirs)))


;;; devonthink


(defvar devonthink-dir devonthink-academic-dir)

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


(defun devonthink-add-file (file)
  "Import FILE to devonthink."
  (let* ((filename (read-string
                    "Name: " (file-name-nondirectory file))))
    (if (y-or-n-p "Keep original?")
        (copy-file file (concat "~/DT3 Academic/"
                                filename))
      (rename-file file (concat "~/DT3 Academic/"
                                filename)))
    (message "File added: %s" filename)))

(bind-key "V" #'devonthink-add-file embark-file-map)

(defun consult-ripgrep-devonthink ()
  (interactive)
  (let ((consult-ripgrep-args "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number .")
        (vertico-count 15)
        (consult-preview-key nil))
    (consult-ripgrep "~/Databases/Academic.dtBase2/Files.noindex/pdf")))

(provide 'gr-database)
;;; gr-database.el ends here
