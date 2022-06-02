;;; elfeed-setup.el --- Setup elfeed     -*- lexical-binding: t; -*-

(use-package elfeed
  :bind
  (:map elfeed-show-mode-map
        ("r" . gr/elfeed-mark-as-read)
        ("g" . elfeed-search-update--force)
        ("m" . elfeed-toggle-star)
        ("e" . gr/elfeed-show-eww)
        ("M-v" . golden-ratio-scroll-screen-down)
        ("b" . gr/elfeed-show-xwidget))
  (:map elfeed-search-mode-map
        ("RET" . gr/elfeed-search-show-entry)
        ("q" . bjm/elfeed-save-db-and-bury)
        ("Q" . bjm/elfeed-save-db-and-bury)
        ("m" . elfeed-toggle-star)
        ("M" . elfeed-toggle-star)
        ("j" . mz/make-and-run-elfeed-hydra)
        ("J" . mz/make-and-run-elfeed-hydra)
        ("n" . (lambda () (interactive) (next-line) (call-interactively 'gr/elfeed-search-show-entry)))
        ("p" . (lambda () (interactive) (previous-line) (call-interactively 'gr/elfeed-search-show-entry)))
        ("b" . (lambda () (interactive) (call-interactively 'gr/elfeed-search-show-entry) (gr/elfeed-show-xwwp))))
  :config
  (setq elfeed-db-directory "~/Dropbox/.elfeeddb")
  (setq elfeed-search-filter "@6-months-ago +unread +AmLit")
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

  ;; elfeed tags hydra

  (defun z/hasCap (s) ""
         (let ((case-fold-search nil))
           (string-match-p "[[:upper:]]" s)
           ))

  (defun z/get-hydra-option-key (s)
    "Return single upper case letter (converted to lower) or first."
    (interactive)
    (let ( (loc (z/hasCap s)))
      (if loc
          (downcase (substring s loc (+ loc 1)))
        (substring s 0 1)
        )))

  (defun mz/make-elfeed-cats (tags)
    "Returns a list of lists. Each one is line for the hydra configuration in the form (c function hint)."
    (interactive)
    (mapcar (lambda (tag)
              (let* (
                     (tagstring (symbol-name tag))
                     (c (z/get-hydra-option-key tagstring))
                     )
                (list c (append '(elfeed-search-set-filter) (list (format "@6-months-ago +%s" tagstring) ))tagstring  )))
            tags))

  (defmacro mz/make-elfeed-hydra ()
    `(defhydra mz/hydra-elfeed (:color blue)
       ""
       ,@(mz/make-elfeed-cats (elfeed-db-get-all-tags))
       ("*" (elfeed-search-set-filter "@6-months-ago +star") "Starred")
       ;;       ("M" elfeed-toggle-star "Mark")
       ("A" (elfeed-search-set-filter "@6-months-ago") "All")
       ("T" (elfeed-search-set-filter "@1-day-ago") "Today")
       ("Q" bjm/elfeed-save-db-and-bury "Quit Elfeed" :color blue)
       ("q" nil "quit" :color blue)
       ))

  (defun mz/make-and-run-elfeed-hydra ()
    ""
    (interactive)
    (mz/make-elfeed-hydra)
    (mz/hydra-elfeed/body))
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
  :straight nil)

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
  ;;  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    ;;(elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (unless elfeed-search-remain-on-entry (forward-line))
    (elfeed-show-entry entry)))

;;;###autoload
(defun gr/elfeed-open-new-window ()
  (interactive)
  (select-frame (make-frame-command))
  (gr/elfeed-open))

;;;###autoload
(defun gr/elfeed-open ()
(interactive)
(let ((inhibit-message t))
  (elfeed)
  (set-frame-size (selected-frame) 150 46)
  (set-frame-position (selected-frame) 150 80)
  (delete-other-windows)
  (if (bound-and-true-p truncate-lines)
      (elfeed-update)
    (toggle-truncate-lines))
  (elfeed-update)))

(provide 'elfeed-setup)
