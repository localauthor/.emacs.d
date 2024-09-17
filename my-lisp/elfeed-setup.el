;;; elfeed-setup.el --- Setup elfeed     -*- lexical-binding: t; -*-

(use-package elfeed
  :bind
  (:map elfeed-search-mode-map
        ("q" . gr/elfeed-save-db-and-bury)
        ("r" . gr/elfeed-toggle-unread)
        ("S" . gr/elfeed-toggle-star)
        ("U" . elfeed-update)
        ("g" . elfeed-update)
        ("/" . elfeed-search-set-filter)
        ("E" . (lambda () (interactive)
                 (find-file (car rmh-elfeed-org-files))))
        ("A" . (lambda () (interactive)
                 (gr/elfeed-search-set-filter "")))
        ("a" . (lambda () (interactive)
                 (gr/elfeed-search-set-filter "+unread")))
        ("e" . (lambda () (interactive)
                 (gr/elfeed-search-set-filter "+emacs +unread")))
        ("l" . (lambda () (interactive)
                 (gr/elfeed-search-set-filter "+lit +unread")))
        ("j" . (lambda () (interactive)
                 (gr/elfeed-search-set-filter "+journal")))
        ("k" . (lambda () (interactive)
                 (gr/elfeed-search-set-filter "+knowledge +unread")))
        ("s" . (lambda () (interactive)
                 (gr/elfeed-search-set-filter "+star")))
        ("t" . (lambda () (interactive)
                 (gr/elfeed-search-set-filter "@1-day-ago")))
        ("w" . (lambda () (interactive)
                 (gr/elfeed-search-set-filter "@1-week-ago")))
        ("?" . hydra-elfeed/body))

  (:map elfeed-show-mode-map
        ("o" . link-hint-open-link)
        ("r" . gr/elfeed-toggle-unread)
        ("s" . gr/elfeed-toggle-star)
        ("g" . elfeed-search-update--force)
        ("q" . delete-window)
        ;; ("d" . elfeed-doi-scihub-or-ebib)
        ("e" . gr/elfeed-show-eww)
        ("M-v" . golden-ratio-scroll-screen-down)
        ("x" . gr/elfeed-show-xwidget)
        ("?" . hydra-elfeed/body))

  :custom
  (elfeed-use-curl nil)
  (elfeed-show-entry-switch 'pop-to-buffer)
  (elfeed-show-entry-delete 'delete-window)
  (elfeed-search-filter "+unread")
  (elfeed-search-remain-on-entry t)

  :config

  (defmacro gr/elfeed-toggle-tag (tag)
    `(defun ,(intern (concat "gr/elfeed-toggle-" (symbol-name tag))) ()
       ,(concat "Toggle elfeed tag `" (symbol-name tag) "’")
       (interactive)
       (let* ((entry (or elfeed-show-entry
                         (elfeed-search-selected t)))
              (tags (if entry (elfeed-entry-tags entry)
                      (error "No entry"))))
         (if (eq major-mode 'elfeed-show-mode)
             (if (elfeed-tagged-p ',tag entry)
                 (progn
                   (elfeed-show-untag ',tag)
                   (message (concat "Untagged: " (symbol-name ',tag))))
               (elfeed-show-tag ',tag)
               (message (concat "Tagged: " (symbol-name 'tag))))
           (if (elfeed-tagged-p ',tag entry)
               (progn
                 (elfeed-search-untag-all ',tag)
                 (message (concat "Untagged: " (symbol-name ',tag))))
             (elfeed-search-tag-all ',tag)
             (message (concat "Tagged: " (symbol-name ',tag))))))))

  (gr/elfeed-toggle-tag unread)
  (gr/elfeed-toggle-tag star)

  (defun elfeed-mark-all-as-read ()
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))

  (defun gr/elfeed-search-set-filter (new-filter)
    (interactive)
    (elfeed-search-set-filter new-filter)
    (message "Filter: %s" new-filter))

  (defhydra hydra-elfeed (:hint nil
                                :color red)
    "
_e_: emacs         _t_: today       _S_: scihub
_l_: lit           _w_: this week   _E_: add to ebib
_k_: knowledge                    _D_: copy DOI
_s_: starred       _A_: all
_a_: all unread    _U_: update      _L_: org list"

    ("U" (elfeed-update))
    ("L" (find-file (car rmh-elfeed-org-files)) :color blue)
    ("S" (elfeed-doi-scihub) :color blue)
    ("E" (elfeed-doi-add-to-ebib) :color blue)
    ("D" (elfeed-doi-copy) :color blue)
    ("A" (gr/elfeed-search-set-filter ""))
    ("a" (gr/elfeed-search-set-filter "+unread"))
    ("e" (gr/elfeed-search-set-filter "+emacs +unread"))
    ("l" (gr/elfeed-search-set-filter "+lit +unread"))
    ("k" (gr/elfeed-search-set-filter "+knowledge +unread"))
    ("s" (gr/elfeed-search-set-filter "+star"))
    ("t" (gr/elfeed-search-set-filter "@1-day-ago"))
    ("w" (gr/elfeed-search-set-filter "@1-week-ago"))
    ("Q" gr/elfeed-save-db-and-bury :color blue)
    ("q" nil :color blue))

  )

;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching

;;;###autoload
(defun gr/elfeed-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (require 'elfeed-db)
  (elfeed-db-load)
  (tab-bar-new-tab)
  (elfeed)
  (elfeed-search-update--force)
  (elfeed-update))

;;write to disk when quiting

;;;###autoload
(defun gr/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window)
  (tab-bar-close-tab))

;;;###autoload
(defun gr/elfeed-open-new-window ()
  (interactive)
  (make-frame-on-current-monitor)
  (gr/elfeed-open))

(defun gr/elfeed-show-eww ()
  (interactive)
  (let ((browse-url-browser-function 'eww-browse-url))
    (elfeed-show-visit)))

(defun gr/elfeed-show-xwidget ()
  (interactive)
  (let ((browse-url-browser-function 'xwidget-webkit-browse-url))
    (elfeed-show-visit)
    (delete-windows-on "*elfeed-entry*")))

;;; elfeed-db

(use-package elfeed-db
  :ensure nil
  :custom
  (elfeed-db-directory (concat user-emacs-directory "var/"
                               "elfeed/db/.elfeed")))

;;; elfeed-org

(use-package elfeed-org
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Dropbox/org/elfeed.org")))


;;; elfeed-tube

(use-package elfeed-tube
  :disabled
  :after elfeed
  :bind
  (:map elfeed-show-mode-map
        ("F" . elfeed-tube-fetch))
  (:map elfeed-search-mode-map
        ("F" . elfeed-tube-fetch))
  :config
  (elfeed-tube-setup))


;;; elfeed-doi

(defcustom elfeed-doi-regexps
  '("scheme=\"doi\" content=\"\\([^\"]*\\)\""
    "citation_doi\" content=\"\\([^\"]*\\)\""
    "data-doi=\"\\([^\"]*\\)\""
    "content=\"\\([^\"]*\\)\" name=\"citation_doi"
    "objectDOI\" : \"\\([^\"]*\\)\""
    "doi = '\\([^']*\\)'"
    "\"http://dx.doi.org/\\([^\"]*\\)\""
    "/doi/\\([^\"]*\\)\">"
    "doi/full/\\(.*\\)&"
    "doi=\\([^&]*\\)&amp")
  "List of regexps to match a DOI.
The doi should be in group 1 so that (match-string 1) contains
the DOI.")

(defun elfeed-doi-retrieve ()
  "Get DOI for elfeed entry."
  (interactive)
  (let* ((url (if elfeed-show-entry
                  (elfeed-entry-link elfeed-show-entry)
                (error "Not an elfeed entry")))
	 (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
         (doi)
         (dois))
    (if (string-match "DOI: \\(.*\\)$" content)
	(setq doi (match-string 1))
      (setq dois (with-current-buffer (url-retrieve-synchronously url)
                   (cl-loop for doi-pattern in elfeed-doi-regexps
	                    do
	                    (goto-char (point-min))
	                    (while (re-search-forward doi-pattern nil t)
		              (cl-pushnew (match-string 1) dois :test #'equal)))
                   (reverse dois)))
      (cond
       ((= 1 (length dois))
        (setq doi (car dois)))
       ((> (length dois) 1)
        (setq doi (completing-read
                   "Select a DOI: "
                   (let ((dois '()))
		     (with-current-buffer (url-retrieve-synchronously url)
		       (cl-loop for doi-pattern in elfeed-doi-regexps
			        do
			        (goto-char (point-min))
			        (while (re-search-forward doi-pattern nil t)
			          (push
				   ;; Cut off the doi, sometimes
				   ;; false matches are long.
				   (cons (format "%40s | %s"
					         (substring
					          (match-string 1)
					          0 (min
						     (length (match-string 1))
						     40))
					         doi-pattern)
				         (match-string 1))
				   dois)))
		       (reverse dois))))))
       (t (error "No DOI found"))))
    doi))

(defmacro elfeed-doi-do (name func docstring)
  `(defun ,(intern (concat "elfeed-doi-" name)) ()
     ,docstring
     (interactive)
     (let ((doi (elfeed-doi-retrieve)))
       (when doi
         (funcall ,func doi)))))

(elfeed-doi-do "copy" 'kill-new "Copy DOI of elfeed-entry.")
(elfeed-doi-do "scihub" 'scihub "Retrieve elfeed article from scihub.")
(elfeed-doi-do "add-to-ebib" 'ebib-zotero-import-identifier "Add elfeed-entry to ebib.")


;; (defun elfeed-doi-scihub-or-ebib ()
;;   "Get article for elfeed entry or add to ebib."
;;   (interactive)
;;   (let* ((url (elfeed-entry-link elfeed-show-entry))
;; 	 (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
;;          (doi)
;;          (dois))
;;     (if (string-match "DOI: \\(.*\\)$" content)
;; 	(setq doi (match-string 1))
;;       (setq dois (with-current-buffer (url-retrieve-synchronously url)
;;                    (cl-loop for doi-pattern in elfeed-doi-regexps
;; 	                    do
;; 	                    (goto-char (point-min))
;; 	                    (while (re-search-forward doi-pattern nil t)
;; 		              (cl-pushnew (match-string 1) dois :test #'equal)))
;;                    (reverse dois)))
;;       (cond
;;        ((= 1 (length dois))
;;         (setq doi (car dois)))
;;        ((> (length dois) 1)
;;         (setq doi (completing-read
;;                    "Select a DOI: "
;;                    (let ((dois '()))
;; 		     (with-current-buffer (url-retrieve-synchronously url)
;; 		       (loop for doi-pattern in elfeed-doi-regexps
;; 			     do
;; 			     (goto-char (point-min))
;; 			     (while (re-search-forward doi-pattern nil t)
;; 			       (push
;; 				;; Cut off the doi, sometimes
;; 				;; false matches are long.
;; 				(cons (format "%40s | %s"
;; 					      (substring
;; 					       (match-string 1)
;; 					       0 (min
;; 						  (length (match-string 1))
;; 						  40))
;; 					      doi-pattern)
;; 				      (match-string 1))
;; 				dois)))
;; 		       (reverse dois))))))
;;        (t (error "No DOI found"))))
;;     (let* ((select (completing-read (format "DOI: %s | Select: " doi)
;;                                     '("Scihub Lookup"
;;                                       "Add to Ebib"
;;                                       "Copy DOI"))))
;;       (cond
;;        ((equal select "Scihub Lookup")
;;         (scihub doi))
;;        ((equal select "Copy DOI")
;;         (kill-new doi))
;;        ((equal select "Add to Ebib")
;;         (ebib-zotero-import-identifier doi))))))

(provide 'elfeed-setup)
