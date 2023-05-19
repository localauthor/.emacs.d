;;; elfeed-setup.el --- Setup elfeed     -*- lexical-binding: t; -*-

(use-package elfeed
  :bind
  (:map elfeed-show-mode-map
        ("r" . gr/elfeed-mark-as-read)
        ("g" . elfeed-search-update--force)
        ("m" . elfeed-toggle-star)
        ("d" . elfeed-doi-scihub-or-ebib)
        ("e" . gr/elfeed-show-eww)
        ("M-v" . golden-ratio-scroll-screen-down)
        ("b" . gr/elfeed-show-xwidget))
  (:map elfeed-search-mode-map
        ("RET" . gr/elfeed-search-show-entry)
        ("q" . bjm/elfeed-save-db-and-bury)
        ("m" . elfeed-toggle-star)
        ;;("n" . (lambda () (interactive) (next-line) (call-interactively 'gr/elfeed-search-show-entry)))
        ;;("p" . (lambda () (interactive) (previous-line) (call-interactively 'gr/elfeed-search-show-entry)))
        ("b" . (lambda () (interactive) (call-interactively 'gr/elfeed-search-show-entry) (gr/elfeed-show-xwidget)))
        ("j" . hydra-elfeed/body))

  :config
  (setq elfeed-db-directory "~/.emacs.d/.elfeed")
  (setq elfeed-search-filter "+unread +AmLit")
  (setq elfeed-search-remain-on-entry t)
  (setq elfeed-show-entry-switch 'display-buffer)

  (defalias 'gr/elfeed-mark-as-read (elfeed-expose #'elfeed-show-untag 'unread) "Mark the current entry read.")
  (defalias 'elfeed-toggle-star (elfeed-expose #'elfeed-search-toggle-all 'star))

  (defun elfeed-mark-all-as-read ()
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))

  ;;functions to support syncing .elfeed between machines
  ;;makes sure elfeed reads index from disk before launching
  (defun bjm/elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

  ;;write to disk when quiting
  (defun bjm/elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window))

  (defhydra hydra-elfeed (:hint nil
                                :color blue)
    "
_a_: AmLit  _j_: Journals  _l_: lit  _g_: geog  _k_: knowledge  _s_: starred"
    ("a" (elfeed-search-set-filter "+AmLit"))
    ("j" (elfeed-search-set-filter "+journal"))
    ("l" (elfeed-search-set-filter "+lit"))
    ("g" (elfeed-search-set-filter "+geography"))
    ("k" (elfeed-search-set-filter "+knowledge"))
    ("s" (elfeed-search-set-filter "+star"))
    ("A" (elfeed-search-set-filter ""))
    ("T" (elfeed-search-set-filter "@1-day-ago"))
    ("Q" bjm/elfeed-save-db-and-bury :color blue)
    ("q" nil :color blue))
  )

(defun gr/elfeed-show-eww ()
  (interactive)
  (let ((browse-url-browser-function 'eww-browse-url))
    (elfeed-show-visit)))

(defun gr/elfeed-show-xwidget ()
  (interactive)
  (let ((browse-url-browser-function 'xwidget-webkit-browse-url))
    (elfeed-show-visit)
    (delete-windows-on "*elfeed-entry*")))

(use-package elfeed-db
  :ensure nil)

(use-package elfeed-goodies
  :after elfeed
  :bind
  (:map elfeed-show-mode-map
        ("o" . elfeed-goodies/show-link-hint))
  :init
  (elfeed-goodies/setup)
  :custom
  (elfeed-goodies/entry-pane-size '0.60)
  (elfeed-goodies/entry-pane-position 'bottom)
  (elfeed-goodies/powerline-default-separator 'bar)
  )

(use-package elfeed-org
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Dropbox/org/elfeed.org"))

  ;;prevent mark-as-read on open (does not work in elfeed-goodies)
  ;;so instead I override the function with one that comments out a line
  ;; original is line 54 in
  ;;~/.emacs.d/straight/repos/elfeed-goodies/elfeed-goodies-split-pane.el ;;(elfeed-untag entry 'unread)

  (defun gr/elfeed-goodies/split-search-show-entry (entry)
    "Display the currently selected item in a buffer."
    (interactive (list (elfeed-search-selected :ignore-region)))
    (when (elfeed-entry-p entry)
      ;;(elfeed-untag entry 'unread)
      (elfeed-search-update-entry entry)
      (elfeed-show-entry entry)))

  (advice-add #'elfeed-goodies/split-search-show-entry :override #'gr/elfeed-goodies/split-search-show-entry))

;;;###autoload
(defun gr/elfeed-search-show-entry (entry)
  "Display the currently selected item in a buffer."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (let ((switch-to-buffer-obey-display-actions nil))
    ;;  (require 'elfeed-show)
    (when (elfeed-entry-p entry)
      ;;(elfeed-untag entry 'unread)
      (elfeed-search-update-entry entry)
      (unless elfeed-search-remain-on-entry (forward-line))
      (elfeed-show-entry entry))))

;;;###autoload
(defun gr/elfeed-open-new-window ()
  (interactive)
  (make-frame-on-current-monitor)
  (gr/elfeed-open))

;;;###autoload
(defun gr/elfeed-open ()
  (interactive)
  (let ((inhibit-message t))
    (elfeed)
    (set-frame-size (selected-frame) 150 46)
    (delete-other-windows)
    (if (bound-and-true-p truncate-lines)
        (elfeed-update)
      (toggle-truncate-lines))
    (elfeed-update)))


(use-package elfeed-summary
  :after (elfeed)
  :config
  (setq elfeed-summary-settings
        '((group (:title . "journals")
                 (:elements
                  (group . ((:title . "AmLit Journals")
                            (:elements
                             (query . (and journal AmLit top)))))
                  (group . ((:title . "Top Journals")
                            (:elements
                             (query . (and journal top (not AmLit))))))
                  (group . ((:title . "Other Journals")
                            (:elements
                             (query . (and journal (not top AmLit))))))))
          (group (:title . "emacs")
                 (:elements
                  (query . (emacs)))))))

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

(defun elfeed-doi-scihub-or-ebib ()
  "Get article for elfeed entry or add to ebib."
  (interactive)
  (let* ((url (elfeed-entry-link elfeed-show-entry))
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
		       (loop for doi-pattern in elfeed-doi-regexps
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
    (let* ((select (completing-read "Select: "
                                    '("Scihub Lookup"
                                      "Add to Ebib"))))
      (cond
       ((equal select "Scihub Lookup")
        (scihub doi))
       ((equal select "Add to Ebib")
        (ebib-zotero-import-identifier doi))))))

(provide 'elfeed-setup)


;; elfeed tags hydra
;; (defun z/hasCap (s) ""
;;        (let ((case-fold-search nil))
;;          (string-match-p "[[:upper:]]" s)
;;          ))

;; (defun z/get-hydra-option-key (s)
;;   "Return single upper case letter (converted to lower) or first."
;;   (interactive)
;;   (let ((loc (z/hasCap s)))
;;     (if loc
;;         (downcase (substring s loc (+ loc 1)))
;;       (substring s 0 1))))

;; (defun mz/make-elfeed-cats (tags)
;;   "Returns a list of lists. Each one is line for the hydra configuration in the form (c function hint)."
;;   (interactive)
;;   (mapcar (lambda (tag)
;;             (let* ((tagstring (symbol-name tag))
;;                    (c (z/get-hydra-option-key tagstring)))
;;               (list c (append
;;                        '(elfeed-search-set-filter)
;;                        (list (format "@6-months-ago +%s" tagstring)))
;;                     tagstring)))
;;           tags))

;; (defmacro mz/make-elfeed-hydra ()
;;   `(defhydra mz/hydra-elfeed (:color blue)
;;      ""
;;      ,@(mz/make-elfeed-cats (elfeed-db-get-all-tags))
;;      ("*" (elfeed-search-set-filter "@6-months-ago +star") "Starred")
;;      ;;       ("M" elfeed-toggle-star "Mark")
;;      ("A" (elfeed-search-set-filter "@6-months-ago") "All")
;;      ("T" (elfeed-search-set-filter "@1-day-ago") "Today")
;;      ("Q" bjm/elfeed-save-db-and-bury "Quit Elfeed" :color blue)
;;      ("q" nil "quit" :color blue)
;;      ))

;; (defun mz/make-and-run-elfeed-hydra ()
;;   ""
;;   (interactive)
;;   (mz/make-elfeed-hydra)
;;   (mz/hydra-elfeed/body))
