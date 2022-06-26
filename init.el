;;; init.el                    -*- lexical-binding: t; -*-

(setq user-emacs-directory "~/.dotfiles/.emacs.d/")

(setq use-package-always-defer nil)

(load-library (concat user-emacs-directory "lisp/gcmh.el"))
(gcmh-mode 1)

;;; Straight setup

(setq straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-host-usernames '((github . "localauthor")))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(setq use-package-hook-name-suffix nil)

(defun straight-fetch-report (&rest _)
  "Show fetched commit summary."
  (interactive)
  (with-current-buffer (get-buffer-create "*straight-fetch-report*")
    (read-only-mode -1)
    (erase-buffer)
    (let ((updates nil)
          (error-repos nil))
      (straight--map-repos
       (lambda (recipe)
         (straight--with-plist recipe (package local-repo)
           (let* ((default-directory (straight--repos-dir local-repo))
                  (commits))
             (condition-case nil
                 (setq commits (straight--process-output "git" "log" "..@{u}" "--oneline"))
               (error (push local-repo error-repos)))
             (unless (or (string-empty-p commits)
                         (not commits))
               (push (cons package (split-string commits "\n")) updates))))))
      (if updates
          (progn
            (insert (propertize "Recently Fetched Commits" 'face 'outline-1)
                    "\n\n")
            (mapc (lambda (update)
                    (let* ((commits (cdr update))
                           (package (car update))
                           (dir (straight--repos-dir package)))
                      (insert
                       (propertize
                        (format "%s [%s commit%s]\n"
                                package
                                (number-to-string (length commits))
                                (if (cdr commits) "s" ""))
                        'face 'font-lock-constant-face))
                      (mapc (lambda (commit)
                              (let ((rev (string-limit commit 7)))
                                (insert-text-button
                                 (concat "  " 
                                         commit)
                                 'follow-link t
                                 'face 'default
                                 'action (lambda (_)
                                           (let ((default-directory dir))
                                             (magit-show-commit (magit-commit-p rev)))))
                                (newline)))
                            (cdr update)))
                    (newline))
                  (cl-sort updates #'string< :key #'car)))
        (insert (propertize "No Recently Fetched Commits" 'face 'outline-1)))
      (when error-repos
        (insert (propertize "Repos With No Upstream Branch" 'face 'outline-1) "\n")
        (mapc (lambda (repo)
                (insert "Click to set upstream: ")
                (insert-text-button
                 (file-name-base repo)
                 'follow-link t
                 'face 'default
                 'action (lambda (_)
                           (let ((default-directory repo)
                                 (branch (magit-get-current-branch)))
                             (magit-status repo)
                             (magit-branch-configure branch)))))
              error-repos))
      (special-mode)
      (pop-to-buffer (current-buffer))
      (goto-char (point-min)))))

(advice-add #'straight-fetch-all :after #'straight-fetch-report)


;;; Misc Startups

;; Debug if there's an error during setup. Set to nil at end of init.el
(setq debug-on-error t)

(setq load-prefer-newer t)

;; set mode for *scratch* buffer
(setq initial-major-mode 'emacs-lisp-mode)
(setq initial-scratch-message nil)

(setq warning-suppress-types '((comp)))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; put here to allow initial-buffer-choice to work in daemon mode
(setq auto-save-default nil) ; stop creating #autosave# files

(setq mu4e-mu-binary "/usr/local/bin/mu")

(setq safe-local-variable-values
      '((eval . (ignore-errors (when (derived-mode-p 'dired-mode)
                                          (setq-local truncate-lines t)
                                          (variable-pitch-mode 0))))
        (eval gr/daily-notes-new-headline)
        (eval setq-local zk-directory default-directory)
        (eval face-remap-add-relative 'org-level-8 :family "Monospace")
        (eval face-remap-add-relative 'org-level-7 :family "Monospace")
        (eval face-remap-add-relative 'org-level-6 :family "Monospace")
        (eval face-remap-add-relative 'org-level-5 :family "Monospace")
        (eval face-remap-add-relative 'org-level-4 :family "Monospace")
        (eval face-remap-add-relative 'org-level-3 :family "Monospace")
        (eval face-remap-add-relative 'org-level-2 :height 140)
        (eval face-remap-add-relative 'org-level-1 :height 150)
        (checkdoc-package-keywords-flag)
        (dired-omit-size-limit)
        (org-confirm-babel-evaluate)
        (eval progn
              (pp-buffer)
              (indent-buffer))
        (eval ignore-errors
              (when
                  (zk-file-p)
                (face-remap-add-relative 'default :family "Monospace" :height 130)))
        (eval face-remap-add-relative 'default :family "Monospace" :height 130)
        (eval remove-from-invisibility-spec '(org-link))))

;; the following sets the recipe used for org
;; anything in the use-package in myinit.org is subsequent to this
;; I specify ":files" to ensure oc-csl works properly

;; the recipe in straight gives an error, SSL certificate expired
;; couldn't figure out what that meant
;; setting the repo as below works

(straight-use-package '(org :repo "git://git.sv.gnu.org/emacs/org-mode.git"
                            :files (:defaults "lisp/*.el"
                                              ("etc/styles" "etc/styles/*")
                                              ("etc/csl/" "etc/csl/*"))))

;; added because f-shortdoc.el wasn't being found
(use-package f
  :straight (f :files (:defaults "f-shortdoc.el"))
  :defer 1)


;;; Basics
;;;; Emacs

(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "lisp")))
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "my-lisp")))
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "priv-lisp")))

(setq elisp-flymake-byte-compile-load-path load-path)

(require 'priv-variables)

(use-package exec-path-from-shell
  :defer 1
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  ;; (setq exec-path-from-shell-warn-duration-millis 999)
  :config
  (exec-path-from-shell-initialize))

(use-package emacs
  :straight nil
  :defer 1
  :bind
  ("C-x [" . beginning-of-buffer)
  ("C-x ]" . end-of-buffer)
  ("C-c e" . eval-buffer)
  ("C-x e" . eval-last-sexp)
  ("C-x E" .  kmacro-end-and-call-macro)
  (:map Info-mode-map
        ("o" . link-hint-open-link))
  (:map help-mode-map
        ("o" . link-hint-open-link))
  ;; config in early-init.el
  )

(use-package tab-bar
  :defer 2
  ;;:config
  ;;(tab-bar-mode 1)
  ;;(tab-bar-history-mode)
  :bind
  ("s-{" . tab-bar-switch-to-prev-tab)
  ("s-}" . tab-bar-switch-to-next-tab)
  ("C-{" . tab-bar-switch-to-prev-tab)
  ("C-}" . tab-bar-switch-to-next-tab)
  ("M-s-n" . gr/tab-to-frame)
  :config
  (defun gr/tab-to-frame ()
    "Open current tab in new frame."
    (interactive)
    (let* ((buffer (current-buffer)))
      (when (< 1 (length (tab-bar-tabs (window-frame))))
        (tab-close))
      (gr/make-frame)
      (switch-to-buffer buffer)))

  (defun gr/tab-bar-face-setup ()
    (set-face-attribute
     'tab-bar nil
     :font "Menlo" :height .8)
    (set-face-attribute
     'tab-bar-tab nil
     :background "grey75"
     :box '(:line-width 1 :style released-button))
    (set-face-attribute
     'tab-bar-tab-inactive nil
     :background "lightgrey"
     :box '(:line-width 1 :style pressed-button)))
  (setq tab-bar-show t
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-new-tab-to 'rightmost
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
  (gr/tab-bar-face-setup)
  :hook
  (server-after-make-frame-hook . gr/tab-bar-face-setup))

(use-package hydra
  :defer 1)

;;;; Faces / Themes Setup

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

(load-theme 'gr-light t)
;;(load-theme 'gr-dark t)

;; (progn (load-theme 'modus-operandi t) (set-face-attribute
;;'show-paren-match nil :underline nil :foreground "#ffffff" :background
;;"systemGreenColor"))

;; (use-package modus-themes
;;   :straight nil
;;   :custom
;;   (modus-themes-headings
;;    (quote ((1 . (underline (height 1)))
;;            (2 . ((foreground "navy blue"))))))
;;   )


;;;; MacOS Keybindings

(defun gr/delete-frame-or-tab ()
  "Delete frame or tab."
  (interactive)
  (if  (< 1 (length (tab-bar-tabs (window-frame))))
      (tab-close)
    (delete-frame)))

;; MacOS Keyboard Shortcuts
(bind-keys*
 ("s-v" . yank)
 ("s-x" . kill-region)
 ("s-c" . kill-ring-save)
 ("s-s" . save-buffer)
 ("s-z" . undo)
 ;;("s-Z" . redo)
 ("s-q" . save-buffers-kill-emacs)
 ("s-f" . consult-line)
 ("s-w" . gr/delete-frame-or-tab)
 ("s-t" . tab-new)
 ("s-n" . gr/make-frame))

(setq ns-alternate-modifier 'meta)
(setq ns-command-modifier 'super)

(when (eq system-type 'darwin)
  (setq mac-right-command-modifier 'control))


;; Global Cursor Bindings -- Mimic MacOS Behavior (eg, as in TextEdit)
;; Binds Command-<left/right arrow> TO MOVE CURSOR TO BEG/END OF LINE
(bind-keys
 ("s-<left>" . move-beginning-of-line)
 ("s-<right>" . move-end-of-line)
 ("s-<up>" . beginning-of-buffer)
 ("s-<down>" . end-of-buffer)
 ("C-<up>" . move-line-up)
 ("C-<down>" . move-line-down))


;; Bold, italics, underline functions

(defun bold-region-or-point ()
  (interactive)
  (if (region-active-p)
      (progn
        (let ((beg (region-end))
              (end (region-beginning)))
          (goto-char beg)
          (insert "*")
          (goto-char end)
          (insert "*")))
    (backward-word)
    (insert "*")
    (forward-word)
    (insert "*")))

(defun italicize-region-or-point ()
  (interactive)
  (if (region-active-p)
      (progn
        (let ((beg (region-end))
              (end (region-beginning)))
          (goto-char beg)
          (insert "/")
          (goto-char end)
          (insert "/")))
    (backward-word)
    (insert "/")
    (forward-word)
    (insert "/")))

(defun underline-region-or-point ()
  (interactive)
  (if (region-active-p)
      (progn
        (let ((beg (region-end))
              (end (region-beginning)))
          (goto-char beg)
          (insert "_")
          (goto-char end)
          (insert "_")))
    (backward-word)
    (insert "_")
    (forward-word)
    (insert "_")))

(bind-keys*
 ("s-i" . italicize-region-or-point)
 ("s-b" . bold-region-or-point)
 ("s-u" . underline-region-or-point))


;;;; Window and Frame Setup

(setq initial-buffer-choice "~/Dropbox/org/dailynotes.org")

(setq ns-use-proxy-icon nil)
(setq frame-title-format '("%b"))

;; if the whole window is 160 or more (char, not px), then a buffer will
;; split to the right, instead of below;
(setq split-height-threshold nil)
(setq split-width-threshold 160)


;;;; display-buffer-alist

(setq display-buffer-alist
      `(
        ("magit:"
         (display-buffer-at-bottom)
         (window-height . 0.5)
         (side . bottom))

        ("\\*elfeed-entry"
         (display-buffer-at-bottom)
         (window-height . 0.6)
         (side . bottom))

        ("\\*ZK-Luhmann"
         (display-buffer-at-bottom)
         (window-height . 0.4)
         (side . bottom))

        ("\\*ZK-Index"
         (display-buffer-at-bottom)
         (window-height . 0.4)
         (side . bottom))

        (,(concat
           "dailynotes.org\\|"
           "\\*\\("
           (string-join
            '("Completions" "Async" "Backups:" "helpful"
              "CAPTURE" "Pp Eval Output" "eshell" "Backtrace"
              "Messages" "Metahelp" "Python" "Org Agenda"
              "Warnings" "Go Translate" "Google Translate"
              "Org Select" "Compile-Log" "[Hh]elp" "annotations"
              "calfw-details" "Embark Collect")
            "\\|") "\\)")
         (display-buffer-at-bottom)
         (window-height . 0.38)
         (side . bottom))
        )
      )

(defun my-switch-to-buffer-list (buffer alist)
  (select-window  (display-buffer-use-some-window buffer alist)))

(defun +select-buffer-in-side-window (buffer alist)
  "Display buffer in a side window and select it"
  (let ((window (display-buffer-in-side-window buffer alist)))
    (select-window window)))

(defun +select-buffer-at-bottom (buffer alist)
  "Display buffer in a side window and select it"
  (let ((window (display-buffer-at-bottom buffer alist)))
    (select-window window)))

(defun +select-buffer-in-direction (buffer alist)
  "Display buffer in direction specified by ALIST and select it."
  (let ((window (display-buffer-in-direction buffer alist)))
    (select-window window)))

(defvar +occur-grep-modes-list '(occur-mode
                                 grep-mode
                                 xref--xref-buffer-mode
                                 ivy-occur-grep-mode
                                 ivy-occur-mode
                                 locate-mode
                                 rg-mode)
  "List of major-modes used in occur-type buffers")

;; the following function is being redefined to allow my custom
;; display-buffer-alist to work as expected on org buffers,
;; which override the display-buffer-alist, per:
;;   - https://www.mail-archive.com/emacs-orgmode@gnu.org/msg133885.html
;;   - https://github.com/wasamasa/shackle/issues/65
;; but if there's some funky window business, delete this

(defun org-switch-to-buffer-other-window (args)
  ;;  (org-no-popups
  ;;     (apply 'switch-to-buffer-other-window args)))
  (switch-to-buffer-other-window args))


;;;; gr-functions and gr-map

(use-package gr-functions
  :straight nil)

(define-prefix-command 'gr-map)

(bind-keys :map global-map
           :prefix-map gr-map
           :prefix "C-."
           ;;("C-." . execute-extended-command)
           ("C-." . avy-goto-char-timer)

           ("/" . switch-to-minibuffer-window)
           ("C-/" . exit-minibuffer)

           ("n" . gr/daily-notes)

           ("o" . link-hint-aw-select)
           ("O" . link-hint-other-tab)

           ("i" . gr/open-init-file)

           ("C-t" . gr/open-tasks-file)
           ("t" . gr/open-tasks-upcoming-agenda-other-frame)

           ("C-f" . gr/open-fragments-file)
           ("f" . gr/open-fragments-file-other-frame)

           ("m" . gr/open-mu4e)
           ("M" . mu4e)

           ("a" . gr/org-agenda)
           ("c" . gr/calfw-open-org-calendar)

           ("b" . consult-bookmark)

           ("g" . eww-duckduckgo)

           ("j" . gr/org-journal-new-entry)

           ("e" . gr/elfeed-open-new-window)
           ("C-e" . gr/elfeed-open)

           ("W" . gr/word-count-subtree)

           ("l" . gr/symbol-menu/body)

           ("D" . gr/lookup-word-at-point)
           ("d" . sdcv-search)

           ("L" . toggle-truncate-lines)

           ("T" . google-translate-smooth-translate)

           ;;("T" . gr/google-translate-lt-en)
           ;; ("T" . go-translate-popup-current)

           ;;("p" . hydra-persp/body)
           ;;("n" . hydra-annotate/body)
           ("s" . hydra-mac-speak/body)

           ("P" . password-store-copy-field)
           ;; ("C-c" . 'flyspell-popup-correct)
           ;;("C-c" . 'flyspell-auto-correct-previous-word)
           )

;;;; init-lock

(use-package init-lock
  :straight nil
  ;; :config
  ;; (init-lock-enable)
  :custom
  (init-lock-files '("~/.dotfiles/.emacs.d/init.el")))

;;;; link-hint

(use-package link-hint
  :defer 1
  :custom
  (link-hint-message nil))

(use-package link-hint-aw-select
  :straight (link-hint-aw-select :local-repo "~/.dotfiles/.emacs.d/my-lisp/link-hint-aw-select")
  :defer 1
  :bind
  (:map gr-map
        ("o" . link-hint-aw-select))
  :config
  ;; open org-links in same window
  ;; allows link-hint--aw-select-org-link to work properly
  (with-eval-after-load "org"
    (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)))

(use-package link-hint-preview
  :straight (link-hint-preview :local-repo "~/.dotfiles/.emacs.d/my-lisp/link-hint-preview")
  :defer 1
  :bind
  (:map link-hint-preview-mode-map
        ("n" . zk-index-preview-next)
        ("p" . zk-index-preview-previous))
  (:map zk-index-mode-map
        ("v" . link-hint--preview-button)
        ("P" . link-hint--preview-button))
  (:map gr-map
        ("p" . link-hint-preview))
  :hook
  (link-hint-preview-mode-hook . link-hint-preview-toggle-frame-mode-line)
  (link-hint-preview-mode-hook . toggle-frame-tab-bar))

;;;; recentf

(use-package recentf
  :defer 1
  :config
  (recentf-mode))

;;;; diminish

(use-package diminish
  :defer 1
  :init
  (eval-after-load 'org-indent '(diminish 'org-indent-mode))
  (diminish 'eldoc-mode)
  (diminish 'outline-mode)
  (diminish 'gcmh-mode)
  (diminish 'visual-line-mode)
  (diminish 'buffer-face-mode)
  (diminish 'outline-minor-mode)
  (diminish 'abbrev-mode))

(add-hook 'buffer-face-mode-hook #'(lambda () (diminish 'buffer-face-mode)))

;;; Org
;;;; org straight.el setup

;; https://github.com/raxod502/straight.el/blob/develop/README.md#installing-org-with-straightel
(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of 'org-mode'.
   Inserted by installing 'org-mode' or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of 'org-mode'.
   Inserted by installing 'org-mode' or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

;;;; org-mode

;; t causes errors
(setq org-element-use-cache nil)
;;(setq org-element-use-cache t)

(use-package org
  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  ("C-c C" . org-clock-goto)
  (:map org-mode-map
        ("RET" . scimax/org-return)
        ;("C-c *" . org-toggle-item)
        ;("C-x *" . org-toggle-item)
        ("C-c ;" . nil)
        ("<tab>" . org-cycle)
        ("C-c C-<tab>" . org-force-cycle-archived)
        ("<M-S-left>" . nil)
        ("<M-S-right>" . nil)
        ("<M-left>" . nil)
        ("<M-right>" . nil)
        ("C-<left>" . org-metaleft)
        ("C-<right>" . org-metaright)
        ("M-<up>" . backward-paragraph)
        ("M-<down>" . forward-paragraph)
        ("C-S-<up>" . org-metaup)
        ("C-S-<down>" . org-metadown)
        ("C-S-<left>" . org-shiftmetaleft)
        ("C-S-<right>" . org-shiftmetaright)
        ("C-<up>" . move-line-up)
        ("C-<down>" . move-line-down)
        ("C-<return>" . org-meta-return)
        ("M-<return>" . org-insert-heading-respect-content)
        ("C-c $" . gr/org-mark-done-and-archive-datetree)
        ("" . org-cycle-agenda-files))
  :mode (("\\.org$" . org-mode)
         ("\\.md$" . org-mode)
         )
  :init
  (with-eval-after-load "org"
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("n" . "notes")))
  :hook
  (org-mode-hook . variable-pitch-mode)
  :config
  (unbind-key "C-," org-mode-map)
  (unbind-key "C-'" org-mode-map)

  :custom-face
  (org-drawer ((t (:foreground "gray60" :height .8))))
  (org-special-keyword ((t (:foreground "gray50" :height .8))))
  (org-level-1 ((t (:inherit outline-1))))
  (org-level-2 ((t (:inherit outline-2))))
  :custom
  (org-ellipsis " ▼") ;◣ ▼ ▽ ► ➽
  (default-major-mode 'org-mode)
  (org-directory "~/Dropbox/org")
  (org-use-speed-commands t)
  (org-speed-commands-user '(("k" . ignore) ("=" . ignore) ("o" . ignore)))
  (org-startup-indented t)
  (org-table-use-standard-references 'from)
  (org-catch-invisible-edits 'smart)
  (org-tags-column -77)
  (org-tag-alist '(("noexport") ("noheadline")))
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)
  (org-hide-leading-stars nil)
  (org-hide-emphasis-markers nil)
  (org-link-keep-stored-after-insertion t)
  (org-link-search-must-match-exact-headline t)
  (org-support-shift-select 'always)
  (org-return-follows-link t)
  (org-export-backends '(ascii html latex md odt org))
  ;; Adds "CLOSED: [timestamp]" when item turned from TODO to DONE
  (org-log-done 'time)
  ;; Sets spacing between headings in org-mode
  (org-cycle-separator-lines -1)
  (org-blank-before-new-entry
   '((heading . nil)
     (plain-list-item . nil)))
  (org-emphasis-alist
   '(("*" bold)
     ("/" italic)
     ("_" underline)
     ("=" org-verbatim verbatim)
     ("+"
      (:strike-through t))
     ("~"
      (:overline t)
      verbatim)))
  (org-startup-with-latex-preview nil)
  (org-use-fast-todo-selection 'expert)
  (org-edit-src-content-indentation 0)
  (org-src-preserve-indentation nil)
  (org-log-states-order-reversed nil)
  )

(use-package org-agenda-setup
  :straight nil
  :defer 1
  :after org)

(use-package org-capture-setup
  :straight nil
  :after org
  :defer 1)

(use-package org-gcal-setup
  :straight nil
  :defer 1)

(use-package gr-org-extras
  :straight nil
  :defer 1)

;;;; org-superstar

(use-package org-superstar
  :defer t
  :after org
  :init
  :hook (org-mode-hook . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list  '("◉" "○" "▪" "◦" "•" "▫" "•" "▫"))
  (setq org-superstar-item-bullet-alist
        '((?+ . ?◦)
          (?* . ?➤)
          (?- . ?–))))

;;;;  org-journal

(use-package org-journal
  :defer t
  :init
  (setq org-journal-prefix-key "C-c j")
  :custom-face
  (org-journal-calendar-entry-face ((t (:underline t :weight bold))))
  :custom
  (org-journal-enable-agenda-integration t)
  (org-journal-dir "~/Dropbox/Writings/journal/")
  (org-journal-file-header "#+TITLE: %B %Y Journal\n#+STARTUP: folded")
  (org-journal-file-type 'monthly)
  (org-journal-date-format "%A, %B %d, %Y")
  (org-journal-file-format "journal-%Y-%m.org")
  (org-journal-find-file 'find-file))


;;;;  org-contrib

(use-package org-contrib
  :straight (org-contrib :files ("lisp/org-contrib.el" "lisp/ox-extra.el")))

(use-package ox-extra
  :straight nil
  :defer 1
  :config
  ;;(require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines))

  ;; change "ignore" tag to "noheadline"
  (defun org-export-ignore-headlines (data backend info)
    "Remove headlines tagged \"noheadline\" retaining contents and promoting children.
Each headline tagged \"ignore\" will be removed retaining its
contents and promoting any children headlines to the level of the
parent."
    (org-element-map data 'headline
      (lambda (object)
        (when (member "noheadline" (org-element-property :tags object))
          (let ((level-top (org-element-property :level object))
                level-diff)
            (mapc (lambda (el)
                    ;; recursively promote all nested headlines
                    (org-element-map el 'headline
                      (lambda (el)
                        (when (equal 'headline (org-element-type el))
                          (unless level-diff
                            (setq level-diff (- (org-element-property :level el)
                                                level-top)))
                          (org-element-put-property el
                                                    :level (- (org-element-property :level el)
                                                              level-diff)))))
                    ;; insert back into parse tree
                    (org-element-insert-before el object))
                  (org-element-contents object)))
          (org-element-extract-element object)))
      info nil)
    (org-extra--merge-sections data backend info)
    data)

  )


;;; Completion

;;;; vertico

(use-package vertico
  :init (vertico-mode)
  :bind* (:map vertico-map
               ("C-j" . vertico-exit-input)
               ("C-g" . keyboard-escape-quit))
  :custom
  (vertico-cycle t)
  (vertico-count 7))

(setq crm-separator ",")

;; Add prompt indicator to `completing-read-multiple'.
(defun crm-indicator (args)
  (cons (concat "[CRM] " (car args)) (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)


;;;; embark

(use-package embark
  :straight (:files (:defaults "embark-org.el"))
  :defer 1
  :bind
  ("C-," . embark-act)
  ("C-<" . embark-act-noquit)
  ("C->" . embark-act-all)
  ("M-," . embark-dwim)
  ("C-h b" . embark-bindings)
  (:map embark-identifier-map
        ("$" . ispell-region)
        ("z" . zk-search))
  (:map embark-symbol-map
        ("h" . helpful-symbol)
        ("c" . capitalize-region)
        ("$" . ispell-region)
        ("G d" . gr/lookup-word-at-point)
        ("G w" . eww-wiki)
        ("G g" . eww-duckduckgo))
  (:map embark-become-help-map
        ("v" . helpful-variable)
        ("f" . helpful-callable)
        ("h" . helpful-symbol))
  (:map embark-file-map
        ("A" . embark-attach-file)
        ;; ("F" . gr/find-file-recursively) doesn't work
        ("p" . gr/embark-save-absolute-path)
        ("P" . gr/embark-insert-absolute-path))
  (:map embark-region-map
        ("t" . title-case-region)
        ("G w" . eww-wiki)
        ("G g" . eww-duckduckgo)
        ("z" . zk-search))
  (:map embark-url-map
        ("s" . browse-url-generic)
        ("x" . xwidget-webkit-browse-url))
  :custom
  (embark-help-key "?")
  (embark-keymap-prompter-key ",")
  (embark-quit-after-action t)
  (embark-indicators '(embark-minimal-indicator embark-highlight-indicator))
  (embark-collect-view 'list)
  (prefix-help-command #'embark-prefix-help-command)
  :config

  (defun embark-act-noquit ()
    "Run action but don't quit the minibuffer afterwards."
    (interactive)
    (let ((embark-quit-after-action nil))
      (embark-act)))

  (setq prefix-help-command #'embark-prefix-help-command)

  ;; no completing read; (type "C-h" for completing read prompter)
  (setq embark-prompter 'embark-keymap-prompter)

  ;; enable completing read prompter
  ;;(setq embark-prompter 'embark-completing-read-prompter)


  ;; from https://karthinks.com/software/fifteen-ways-to-use-embark/
  (eval-when-compile
    (defmacro embark-aw-select (fn)
      `(defun ,(intern (concat "embark-aw-select-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn))))))

  (with-eval-after-load "embark"
    (define-key embark-file-map (kbd "o") (embark-aw-select find-file))
    (define-key embark-buffer-map (kbd "o") (embark-aw-select switch-to-buffer))
    (define-key embark-bookmark-map (kbd "o") (embark-aw-select bookmark-jump)))

  ;; embark action to attach file to email message
  (autoload 'gnus-dired-attach "gnus-dired")

  (defun embark-attach-file (file)
    "Attach FILE to an  email message.
The message to which FILE is attached is chosen as for
`gnus-dired-attach`, that is: if no message buffers are found a
new email is started; if some message buffer exist you are asked
whether you want to start a new email anyway, if you say no and
there is only one message buffer the attachements are place
there, otherwise you are prompted for a message buffer."
    (interactive "fAttach: ")
    (gnus-dired-attach (list (abbreviate-file-name (expand-file-name file)))))

  ;; absolute-path functions

  (defun gr/embark-insert-absolute-path (file)
    "Insert absolute path to FILE."
    (interactive "FFile: ")
    (insert (abbreviate-file-name (expand-file-name file))))

  (defun gr/embark-save-absolute-path (file)
    "Save the absolute path to FILE in the kill ring."
    (interactive "FFile: ")
    (kill-new (abbreviate-file-name (expand-file-name file))))


  ;; embark buffer actions

  (defun embark-target-this-buffer ()
    (cons 'this-buffer (buffer-name)))

  (add-to-list 'embark-target-finders #'embark-target-this-buffer t)

  (add-to-list 'embark-keymap-alist '(this-buffer . this-buffer-map))

  (push 'embark--allow-edit
        (alist-get 'write-file embark-target-injection-hooks))

  (embark-define-keymap this-buffer-map
    "Commands to act on current file or buffer."
    ("RET" eval-buffer)
    ("e" eval-buffer)
    ("R" rename-file)
    ("D" delete-file)
    ("W" write-file)
    ("$" ispell)
    ("!" shell-command)
    ("&" async-shell-command)
    ("x" consult-file-externally)         ; useful for PDFs
    ("c" copy-file)
    ("k" kill-buffer)
    ("z" bury-buffer)
    ("s" embark-eshell)
    ("|" embark-shell-command-on-buffer)
    ("g" revert-buffer)
    ("p" pwd)
    ("SPC" mark-whole-buffer)
    ("<" previous-buffer)
    (">" next-buffer))

  (add-to-list 'embark-repeat-actions #'previous-buffer)
  (add-to-list 'embark-repeat-actions #'next-buffer)

  )

(use-package embark-org
  :straight nil
  :defer 1
  :bind
  (:map embark-org-link-map
        ("x" . consult-file-externally)))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

;;;; consult

(use-package consult
  :defer 1
  :after (embark)
  :bind
  ("C-s" . consult-line)
  ("C-x b" . consult-buffer)
  ("M-y" . consult-yank-from-kill-ring)
  ("M-s f" . consult-find)
  ("M-s F" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s m" . consult-multi-occur)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  (:map consult-narrow-map
        ("?" . consult-narrow-help))
  :bind*
  ("C-c [" . consult-global-mark)
  ("C-;" . consult-imenu-all)
  ("C-:" . consult-outline)
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode)
  :custom
  (consult-fontify-preserve nil)
  (consult-async-split-style 'semicolon)

  :config

  ;; Set consult-preview-key for certain functions
  (consult-customize
   consult-git-grep consult-grep consult-global-mark consult-ripgrep
   consult-bookmark consult--source-buffer consult-recent-file consult-xref
   consult--source-bookmark consult-buffer
   :preview-key (list (kbd "C-{")
                      :debounce 1.5 'any))

  (setq consult-preview-key 'any)

  ;;Combine `consult-imenu' and `consult-imenu-multi'

  (defun consult-imenu-all (&optional arg)
    "Call `consult-imenu'. With prefix-command ARG, call
    `consult-imenu-multi'."
    (interactive "P")
    (if arg (consult-imenu-multi) (consult-imenu)))
  ;;makes C-s and C-r search forward and backward in consult-line
  ;;changed to make C-s call previous search term

  (defvar my-consult-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-s" #'previous-history-element)
      ;;(define-key map "\C-s" #'next-line)
      ;;(define-key map "\C-r" #'previous-line)
      map))
  (consult-customize consult-line :keymap my-consult-line-map)

  ;; manual preview for non-Consult commands using Embark

  (define-key minibuffer-local-map (kbd "M-[") #'my-embark-preview)

  (defun my-embark-preview ()
    (interactive)
    ;; Disable preview for Consult commands
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action))
          (embark-dwim)))))

  (defun my/consult-outline-narrow-heading (heading)
    "Narrow to and expand HEADING."
    (embark-consult-goto-location heading)
    (outshine-narrow-to-subtree)
    (outline-show-subtree))

  (embark-define-keymap embark-consult-outline-map
    "Keymap for embark actions in `consult-outline'."
    ("r" my/consult-outline-narrow-heading))

  (defun with-embark-consult-outline-map (fn &rest args)
    "Let-bind `embark-keymap-alist' to include `consult-location'."
    (let ((embark-keymap-alist
           (cons '(consult-location . embark-consult-outline-map)
                 embark-keymap-alist)))
      (apply fn args)))

  (advice-add 'consult-outline :around #'with-embark-consult-outline-map)

  (defun gr/consult-ripgrep-select-dir ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'consult-ripgrep)))

  (defun gr/consult-find-select-dir (p)
    (interactive "P")
    (let ((current-prefix-arg '(4)))
      (call-interactively #'consult-find)))

  )

;;;; consult-dir

(use-package consult-dir
  :straight (consult-dir :host github :repo "karthink/consult-dir")
  :bind ("C-x C-d" . consult-dir)
  (:map vertico-map
        ("C-x C-j" . consult-dir-jump-file))
  :custom
  (consult-dir-sources '(consult-dir--source-bookmark
                         consult-dir--source-recentf
                         consult-dir--source-straight-repos))
  :config

  (defvar consult-dir--source-straight-repos
    `(:name "Straight repos"
            :narrow ?s
            :hidden t
            :category file
            :face consult-file
            :history file-name-history
            :items ,#'consult-dir-straight-repos)
    "Straight repos directory source for `consult-dir--pick'.")

  (defun consult-dir-straight-repos ()
    "Return a list of the straight repos directories."
    (mapcar
     (lambda (x)
       (concat (abbreviate-file-name x) "/"))
     (directory-files
      (concat
       straight-base-dir
       "straight/repos/")
      t
      "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))
  )

(defun recentd-track-opened-file ()
  "Insert the name of the directory just opened into the recent list."
  (and (derived-mode-p 'dired-mode) default-directory
       (recentf-add-file default-directory))
  ;; Must return nil because it is run from `write-file-functions'.
  nil)

(defun recentd-track-closed-file ()
  "Update the recent list when a dired buffer is killed.
That is, remove a non kept dired from the recent list."
  (and (derived-mode-p 'dired-mode) default-directory
       (recentf-remove-if-non-kept default-directory)))

(add-hook 'dired-after-readin-hook 'recentd-track-opened-file)
(add-hook 'kill-buffer-hook 'recentd-track-closed-file)


;;;; bookmark-view

(use-package bookmark-view
  :straight (bookmark-view :host github :repo "minad/bookmark-view")
  :defer 1)

;;;; marginalia

(use-package marginalia
  :bind
  ("M-A" . marginalia-cycle)
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  )

;;;; orderless

(use-package orderless
  :init
  (setq orderless-matching-styles '(orderless-prefixes
                                    orderless-regexp)
        completion-styles '(orderless partial-completion initials)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles . (partial-completion initials)))))
  (setq orderless-style-dispatchers
          '(orderless-literal-dispatcher
            orderless-initialism-dispatcher))
  (setq orderless-component-separator "[ +]")
  :config
  (defun orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

(defun orderless-initialism-dispatcher (pattern _index _total)
  "Leading initialism dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "~" pattern)
    `(orderless-initialism . ,(substring pattern 0 -1))))
)

;;;; savehist

(use-package savehist
  :defer 1
  :config
  (savehist-mode 1)
  (setq savehist-additional-variables
        '(citar-history search-ring regexp-search-ring))
  )

;;;; company

(use-package company
  :defer 1
  :diminish
  :bind
  (:map company-active-map
        ("<return>" . nil)
        ("RET" . nil)
        ("<tab>" . 'company-complete-selection)
        ("TAB" . 'company-complete-selection)
        ("S-<tab>" . 'company-complete-common)
        ("S-TAB" . 'company-complete-common)
        ("C-n" . 'company-select-next-or-abort)
        ("C-p" . 'company-select-previous-or-abort)
        ("C-<return>" . 'company-complete-common)
        ("C-RET" . 'company-complete-common)
        ("C-e" . 'company-other-backend)
        ("C-s" . 'company-filter-candidates))
  :custom
  (company-require-match nil)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-tooltip-limit 12)
  (company-idle-delay 0.3)
  (company-echo-delay 0)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case 'keep-prefix)
  (company-sort-prefer-same-case-prefix t)
  (company-format-margin-function nil)
  (company-dabbrev-other-buffers t)
  ;; interfers with Luhmann, zk sort order
  (company-transformers '(company-sort-by-occurrence))
  :config
  (global-company-mode 1)
  ;; use TAB for completion-at-point
  (setq tab-always-indent nil)
  (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)
  (setq company-backends '(company-capf
                           company-bbdb
                           company-files
                           (company-elisp
                            company-dabbrev-code
                            company-gtags
                            company-etags
                            company-keywords)
                           company-dabbrev
                           company-yasnippet))
  (setq company-files-exclusions '(".git/" ".DS_Store")))

(use-package company-posframe
  :defer 1
  :diminish
  :bind
  (:map company-posframe-active-map
        ([mouse-1] . company-abort)
        ([?\e] . company-abort))
  :config
  (company-posframe-mode 1)
  :custom
  (company-posframe-show-indicator nil)
  (company-posframe-show-metadata nil)
  (company-posframe-quickhelp-delay nil)
  )

;; kills company buffer after completion, to prevent it from showing up when
;; new window is created, like elfeed or mu4e
(add-hook 'company-after-completion-hook
          (lambda (arg) (when (get-buffer " *company-posframe-buffer*")
(kill-buffer " *company-posframe-buffer*"))))

;;;; prescient / company-prescient

(use-package prescient
  :config
  (prescient-persist-mode 1)
  :custom
  (prescient-aggressive-file-save t))

(use-package company-prescient
  :disabled
  ;; interfers with Luhmann, zk sort order
  :config
  (company-prescient-mode -1))


;;; Citation / Bibliography
;;;; citar

(defvar gr/bibliography '("~/Dropbox/gr-bibliography.bib"))

(use-package citar
 ;; :straight (:host github :repo "bdarcus/citar" :fork t)
  :after (oc misc-file-handling devonthink-dir)
  :commands (citar-select-ref
             citar-select-refs
             gr/citar-mmd-insert-citation
             citar-format-reference
             citar--ensure-entries)
  :bind
  (:map org-mode-map
        ("C-c \\" . citar-insert-citation))
  (:map citar-org-citation-map
        ("<mouse-1>")
        ("<mouse-3>")
        ("C-d" . delete-char)
        ("C-k" . kill-visual-line)
        ("C-h" . embark-keymap-help))
  (:map citar-map
        ("F" . devonthink-dir-find-file)
        ("c" . citar-insert-citation)
        ("z" . zk-search)
        ("s" . ex/citar-search-pdf-contents))
  (:map citar-citation-map
        ("F" . devonthink-dir-find-file)
        ("z" . zk-search)
        ("s" . ex/citar-search-pdf-contents))
  :custom
  (citar-bibliography gr/bibliography)
  (citar-library-paths (list-dirs-recursively devonthink-dir))
  (citar-notes-paths '("~/Dropbox/ZK/Zettels"))
  (citar-additional-fields '("doi" "url" "crossref"))
  (citar-library-file-extensions '("pdf" "epub"))
  (citar-file-note-extensions '("org" "md"))
  (citar-file-open-function 'citar-file-open-external)
  (citar-file-additional-files-separator " ")
  (citar-file-open-prompt t)
  (citar-citeproc-csl-styles-dir "~/.csl")
  (citar-citeproc-csl-locales-dir "~/.csl/locales")
  (citar-format-reference-function 'citar-citeproc-format-reference)
  (citar-citeproc-csl-style
   "chicago-fullnote-bibliography-short-title-subsequent.csl")
  (citar-display-transform-functions '((t . citar-clean-string)))
  (citar-open-note-functions '(gr/citar-zk-open-note)) ;; added
  (citar-open-note-function 'gr/citar-zk-open-note) ;; obsoleted
  (citar-select-multiple t)
  ;; (citar-templates
  ;;  '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
  ;;    (suffix . "          ${=key= id:15}    ${=type=:12}    ${crossref tags keywords keywords:*}")
  ;;    (preview . "${author editor} (${year issued date}) ${title}, ${journal publisher container-title collection-title}.\n")
  ;;    (note . "${=key=} - ${title} (${year})")))

  :config
  ;; are these requires necessary?
  (require 'citar-org)
  (require 'citar-file)
  (require 'citar-filenotify)
  (require 'citar-citeproc)

  (citar-filenotify-global-watches)
  (setq citar-filenotify-callback 'refresh-cache)

  (add-to-list 'citar-library-paths "~/Dropbox/Dickinson Primary/")

  (defun gr/citar-zk-open-note (key entry)
    "Custom function for citar-open-note-function."
    (let* ((filematch (format "\\`[0-9]\\{12\\}.%s.*\\.\\(?:md\\)\\'"
                              (regexp-quote key)))
           (results-key (seq-mapcat
                         (lambda (dir)
                           (if-let ((file (directory-files dir t filematch)))
                               file))
                         citar-notes-paths)))
      (if results-key
          (funcall 'find-file (car results-key))
        (when (y-or-n-p "No note associated - create one?")
          (let* ((template "${=key=} - ${author}, ${title} (${year})")
                 (title
                  (when template
                    (subst-char-in-string ?: ?-
                                          (citar--format-entry-no-widths
                                           entry
                                           template)))))
            (zk-new-note title))))))

  (defun ex/citar-search-pdf-contents ()
    ;; from localauthor
    "Search pdfs."
    (interactive)
    (let* ((refs (citar-select-refs))
           (files (citar-file--files-for-multiple-entries
                   refs citar-library-paths '("pdf")))
           (string (read-string "Search string: ")))
      (pdf-occur-search files string t)))

  ;; crossref finding only works one way
  ;; ie, if Wolosky2004 is in collection (lists crossref) Bercovitch2004
  ;; then citar-open Wolosky2004 will show results Bercovitch2004
  ;; but citar-open Bercovitch2004 will not show Wolosky2004 or Packer2004

  (defun gr/citar-file--make-filename-regexp (keys extensions &optional additional-sep)
  "Regexp matching file names starting with KEYS and ending with EXTENSIONS.
When ADDITIONAL-SEP is non-nil, it should be a regular expression
that separates the key from optional additional text that follows
it in matched file names.  The returned regexp captures the key
as group 1, the extension as group 2, and any additional text
following the key as group 3."
  (let* ((entry (when keys
                  (citar--get-entry (car keys))))
         (xref (citar--get-value "crossref" entry)))
    (unless (or (null xref) (string-empty-p xref))
      (push xref keys))
    (when (and (null keys) (string-empty-p additional-sep))
      (setq additional-sep nil))
    (concat
     "\\`"
     (if keys (regexp-opt keys "[0-9]\\{12\\}?.*\\(?1:") ".*?\\(?1:[a-z]+[0-9]\\{4\\}[a-z]?\\)")
     ".?"
     (when additional-sep (concat "\\(?3:" additional-sep "[^z-a]*\\)?"))
     "\\."
     (if extensions (regexp-opt extensions "\\(?2:") "\\(?2:[^.]*\\)")
     "\\'")))

  (advice-add 'citar-file--make-filename-regexp :override 'gr/citar-file--make-filename-regexp)

  ;; maybe not necessary anymore?
  (defun citar-xref-notes ()
    (let* ((hasnote (citar-file--has-file citar-notes-paths
                                          citar-file-note-extensions))
           (candidates (citar-file--has-file-notes-hash))
           (note-keys))
      (maphash
       (lambda (citekey entry)
         (when (funcall hasnote citekey entry)
           (push citekey note-keys)))
       candidates)
      note-keys))

  ;; new
  (setq citar-has-note-functions '(citar-file-has-notes))

  ;; old new
  ;; (setq citar-keys-with-notes-functions '(citar-file--keys-with-file-notes citar-xref-notes))
  )

;;;; org-cite

(use-package oc
  :straight nil
  :defer 1
  :init
  (setq org-cite-csl-styles-dir "~/.csl"
        org-cite-csl-locales-dir "~/.csl/locales"
        org-odt-preferred-output-format nil ;; "docx"
        org-odt-styles-file "~/Dropbox/Academic Writings/template.ott"
        org-cite-global-bibliography gr/bibliography)
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        org-cite-export-processors '((t csl "~/.csl/chicago-fullnote-bibliography-short-title-subsequent.csl"))))

(use-package oc-csl
  :straight nil
  :defer t)

;;;; citeproc / parsebib

(use-package citeproc
  :defer t)

(use-package parsebib
  :straight (parsebib :host github :repo "joostkremers/parsebib")
  :defer t)

;;;; ebib

(use-package ebib
  :bind
  (:map ebib-index-mode-map
        ("?" . (lambda () (interactive) (embark-bindings-in-keymap ebib-index-mode-map)))
        ("h" . hydra-ebib/body)
        ("k" . nil)
        ("D" . ebib-delete-entry)
        ("d" . nil)
        ("c" . ebib-filters-cancel-filter)
        ("z" . nil)
        ("s" . ebib-filter-any)
        ("O" . ebib-filters-apply-filter)
        ("s-s" . ebib-save-curent-database)
        ("q" . ebib-smart-quit)
        )
  (:map ebib-entry-mode-map
        ("?" . (lambda ()
                 (interactive)
                 (embark-bindings-in-keymap ebib-entry-mode-map)))
        ("h" . hydra-ebib/body)
        ("d" . nil)
        ("j" . ebib-jump-to-entry)
        ("e" . ebib-edit-current-field)
        ("O" . ebib-filters-apply-filter)
        ("s-s" . ebib-save-curent-database)
        ("q" . ebib-quit-entry-buffer)
        ("k" . nil))
  :custom
  (ebib-preload-bib-files gr/bibliography)
  (ebib-autogenerate-keys t)
  (ebib-create-backups t)
  (ebib-uniquify-keys nil)
  ;; (ebib-index-default-sort '("timestamp" . descend))
  (ebib-use-timestamp t)
  (ebib-index-columns '(("Author/Editor" 40 t)
                        ("Entry Key" 15 t)
                        ;;("Year" 6 t)
                        ("Title" 50 t)))
  :config

  (advice-add #'ebib :after #'ebib-truncate-lines)

  (defun ebib-truncate-lines (&rest _)
    (ebib--pop-to-buffer (ebib--buffer 'index))
    (let ((inhibit-message t))
      (unless (bound-and-true-p truncate-lines)
        (toggle-truncate-lines))))

  (defhydra hydra-ebib (:hint nil :color blue)
    "
  _j_: Jump to Entry   _k_: Add Keyword    _!_: Auto-Citekey     _s_: DOI Lookup
  _O_: Apply Filter    _e_: ebib-open      _E_: Edit Citekey     _S_: ISBN Lookup
  _C_: Cancel Filter   _D_: Delete Field   _X_: Delete Entry     _I_: Auto Import
  "
    ("k" ebib-add-keywords-to-entry)
    ("e" ebib-open)
    ("!" ebib-generate-autokey)
    ("X" ebib-delete-entry)
    ("E" ebib-edit-keyname)
    ("D" ebib-delete-current-field-contents)
    ("j" ebib-jump-to-entry)
    ("O" ebib-filters-apply-filter)
    ("o" ebib-citar-open-resource)
    ("C" ebib-filters-cancel-filter)
    ;; ("s" ebib-save-current-database)
    ("I" ebib-zotero-import-identifier)
    ("S" ebib-isbn-web-search)
    ("s" crossref-lookup)
    ("q" nil))
  )


(use-package ebib-extras
  :straight nil
  :defer 1
  :bind
  (:map ebib-index-mode-map
        ("o" . ebib-citar-open-resource))
  (:map ebib-entry-mode-map
        ("o" . ebib-citar-open-resource))
  (:map citar-map
        ("e" . citar-ebib-jump-to-entry)))

(use-package ebib-zotero
   :straight nil
   :defer 1
   :bind
   (:map ebib-index-mode-map
         ("I" . ebib-zotero-import-identifier)))


(use-package pdf-drop-mode
  :straight (:host github :repo "rougier/pdf-drop-mode")
  :defer 1
  :custom
  (pdf-drop-search-methods '(metadata title user-title content))
  :config
  (pdf-drop-mode)
  (setq pdf-drop-search-hook #'my/pdf-process)
  (defun my/pdf-process (file doi)
    (ebib-zotero-import-identifier doi file))
  )


;;;; biblio / sci-hub

(use-package scihub
  :straight (:host github :repo "emacs-pe/scihub.el")
  :defer 1
  :custom
  (scihub-download-directory (expand-file-name "~/Downloads")))

(use-package biblio
  :defer 1
  :after (ebib)
  :custom
  (biblio-crossref-user-email-address vu-email)
  :config
  ;; override default to ido
  (defun biblio--completing-read-function ()
    completing-read-function)

  ;; convenience functions for getting doi and downloading from Sci-Hub

  (defun gr/biblio--copy-doi (bibtex entry)
    "Copy DOI from bibtex entry."
    (kill-new (biblio-alist-get 'doi entry)))

  (defun gr/biblio-copy-doi ()
    "Copy DOI of a selected entry."
    (interactive)
    (biblio--selection-forward-bibtex #'gr/biblio--copy-doi))

  (defun gr/biblio--copy-doi-ext (entry)
    "Copy DOI from bibtex entry."
    (let ((doi (biblio-alist-get 'doi entry)))
      (kill-new doi)
      (message (format "Copied doi: \"%s\"" doi))))

  (defun gr/biblio--get-from-sci-hub (bibtex entry)
    "Download selected reference from Sci-Hub."
    (scihub (biblio-alist-get 'doi entry)))

  (defun gr/biblio-get-from-sci-hub (bibtex entry)
    "Download selected reference from Sci-Hub."
    (interactive)
    (biblio--selection-forward-bibtex #'gr/biblio--get-from-sci-hub))

  (defun gr/biblio--get-from-sci-hub-ext (entry)
    "Download selected reference from Sci-Hub."
    (scihub (biblio-alist-get 'doi entry)))

  (defun gr/biblio--import-to-ebib (entry)
    (let ((doi (biblio-alist-get 'doi entry)))
      (ebib-auto-import doi)))

  (setq biblio-selection-mode-actions-alist
        '(("Import to ebib" . gr/biblio--import-to-ebib)
          ("Copy DOI" . gr/biblio--copy-doi-ext)
          ("Grab from Sci-Hub" . gr/biblio--get-from-sci-hub-ext)
          ("Find open access copy on Dissemin" . biblio-dissemin--lookup-record)))

  )

(use-package ebib-biblio
  :straight nil
  :after (ebib biblio)
  :bind (:map biblio-selection-mode-map
              ("e" . ebib-biblio-selection-import)))

;;;; oc-csl-activate

(use-package oc-csl-activate
  :disabled
  :straight (oc-csl-activate :host github :repo "andras-simonyi/org-cite-csl-activate")
  :after citeproc
  :config
  ;; not quite ready for primetime
  ;; :hook (org-mode-hook . (lambda ()
  ;; (cursor-sensor-mode 1)
  ;; (org-cite-csl-activate-render-all)))
  )

;;;; mmd-citation-support

(use-package mmd-citation-support
  :straight nil
  :defer 1
  :hook
  (completion-at-point-functions . gr/mmd-citation-completion-at-point))

;;; Writing

;;;; zk

(use-package zk
  :straight (zk :type nil
                :local-repo "~/.dotfiles/.emacs.d/my-lisp/zk/"
                :files (:defaults "zk-consult.el"))
  ;; :init
  ;; (require 'zk-consult)
  ;; (require 'zk-extras)
  :defer 1
  :bind
  (:map zk-id-map
        ("p" . zk-preview)
        ("s" . zk-search)
        ("r" . zk-consult-grep)
        ("o" . link-hint--aw-select-zk-link))
  :hook
  (completion-at-point-functions . zk-completion-at-point)
  (completion-at-point-functions . gr/mmd-citation-completion-at-point)
  :custom
  (zk-directory "~/Dropbox/ZK/Zettels")
  (zk-file-extension "md")
  (zk-default-backlink "201801190001")
  (zk-link-and-title 'ask)
  (zk-new-note-link-insert 'ask)
  (zk-link-and-title-format "%t [[%i]]")
  (zk-completion-at-point-format "%t [[%i]]")
  (zk-tag-grep-function #'zk-grep) ;; #'zk-consult-grep-tag-search)
  (zk-grep-function #'zk-grep) ;; #'zk-consult-grep
  (zk-current-notes-function nil)
  (zk-select-file-function 'zk-consult-select-file)
  (zk-consult-preview-functions
   '(zk-current-notes
     zk-unlinked-notes))
  :config
  (zk-setup-auto-link-buttons)
  (with-eval-after-load 'embark-org
    (zk-setup-embark))
  ;;(add-to-list 'auto-mode-alist '("\\.md\\'" . outline-mode))
  (with-eval-after-load 'embark
    (add-to-list 'embark-become-keymaps 'embark-become-zk-file-map))
  (with-eval-after-load 'consult
    (add-to-list 'consult-buffer-sources 'zk-consult-source 'append))
  (consult-customize
   zk-find-file zk-find-file-by-full-text-search zk-network zk-backlinks zk-links-in-note
      :preview-key (list (kbd "C-{"))))

(use-package zk-index
  :after zk
  :straight (zk-index :local-repo "~/.dotfiles/.emacs.d/my-lisp/zk/"
                      :type nil
                      :files ("zk-index.el"))
  :bind (:map zk-index-mode-map
              ("o" . zk-index-aw-select)
              ("j" . consult-line) ;; "jump"
              ("?" . hydra-zk-index/body))
  :config
  (zk-index-setup-embark)
  :custom
  (zk-index-prefix nil)
  (zk-index-desktp-prefix "- ")
  (zk-index-desktop-major-mode 'outline-mode)
  (zk-index-desktop-directory zk-directory))

(setq zk-index-desktop-directory (bound-and-true-p zk-directory))

(use-package zk-luhmann
  :after zk-index
  :straight (zk-luhmann :local-repo "~/.dotfiles/.emacs.d/my-lisp/zk-luhmann")
  :bind (:map zk-index-mode-map
              ("L" . zk-luhmann-index-sort)
              ("l" . zk-luhmann-index)
              ("C-f" . zk-luhmann-index-forward)
              ("C-b" . zk-luhmann-index-back)
              ("C-t" . zk-luhmann-index-unfold)
              ("t" . zk-luhmann-index-top))
  :hook (completion-at-point-functions . zk-luhmann-completion-at-point)
  :custom
  (zk-luhmann-id-prefix "{")
  (zk-luhmann-id-postfix "}")
  (zk-luhmann-indent-index t))

(use-package zk-consult
  :straight nil
  :defer 1)

(use-package zk-extras
  :straight nil
  :defer 1
  :config
  (setq zk-core-notes-count (length (zk-non-luhmann-list)))
  (setq zk-luhmann-notes-count (length (zk-luhmann-files))))

  (use-package zk-link-hint
  :straight (zk-link-hint :local-repo "~/.dotfiles/.emacs.d/my-lisp/zk")
  :defer 1)

(with-eval-after-load "embark"
  (embark-define-keymap embark-become-zk-file-map
    "Keymap for Embark zk-file actions."
    :parent embark-meta-map
    ("f" zk-find-file)
    ("g" consult-grep)
    ("s" zk-find-file-by-full-text-search)))

(eval-and-compile
  (defhydra hydra-zk (:hint nil
                            :color blue)
    "
  _h h_: Inbox      _i_: Insert Link   _N_: New Note       _d_: to desktop
  _h s_: Strct Nts  _c_: Insert Cite   _r_: Rename Note    _z_: zk grep
  _h i_: Index      _f_: Find File     _o_: Open Link      _e_: ebib-open
                  _b_: Backlinks     _C_: Current Notes  _B_: Biblio.biz
   [Luhmann: %`zk-luhmann-notes-count | Notes: %`zk-core-notes-count]"
    ("h h" (lambda () (interactive) (zk-find-file-by-id "201801190001")))
    ("h i" (lambda () (interactive) (zk-find-file-by-id "201801180001")))
    ("h s" (lambda () (interactive) (zk-find-file-by-id "201801180002")))
    ("N" zk-new-note)
    ("R" zk-rename-note)
    ("r" zk-rename-note)
    ("i" zk-luhmann-insert-link)
    ("e" ebib-open)
    ("B" hydra-bib/body)
    ;;("B b" gr/append-bibliography)
    ;;("B r" citar-insert-reference)
    ;;("B p" pullbib-pull)
    ("I" zk-index)
    ("l" zk-luhmann-index)
    ("G" zk-luhmann-index-go-to-current)
    ("L" zk-lit-notes)
    ("c" gr/citar-mmd-insert-citation)
    ("C" zk-current-notes)
    ("m" zk-make-link-buttons)
    ("o" link-hint-aw-select)
    ("b" zk-network)
    ("S" (lambda () (interactive) (zk-stats 1)) :color red)
    ("f" zk-find-file)
    ("F" zk-find-file-by-full-text-search)
    ("z" zk-consult-grep)
    ("g" zk-consult-grep)
    ("s" zk-search)
    ("d" zk-index-send-to-desktop)
    ("D" zk-index-desktop)
    ;; ("d" gr/consult-ripgrep-select-dir)
    ("p" devonthink-dir-find-file)
    ("q" nil)))

(bind-key* (kbd "C-z") 'hydra-zk/body)
(bind-key* (kbd "M-z") 'hydra-zk/body)

(eval-and-compile
  (defhydra hydra-bib (:hint nil
                            :color blue)
    "
       _r_: Insert Ref          _e_: ebib-hydra        _d_: DOI Lookup
       _b_: Insert Bib          _I_: Auto Import       _i_: ISBN Look up"

    ("b" gr/append-bibliography)
    ("r" citar-insert-reference)
    ("e" hydra-ebib/body)
    ("I" ebib-auto-import)
    ("i" ebib-isbn-web-search)
    ("d" crossref-lookup)
    ("c" gr/citar-mmd-insert-citation)
    ("q" nil)))

(eval-and-compile
  (defhydra hydra-zk-index (:hint nil)
    "
_S_: Size          List:
_M_: Modified      _l_: Luhmann  _L_: lit
_C_: Created       _a_: all      _c_: core "
    ("a" zk-index-refresh)
    ("l" zk-luhmann-index)
    ("c" zk-core-index)
    ("n" zk-non-luhmann-index)
    ("L" zk-lit-notes-index)
    ("e" zk-ed-index)
    ("M" zk-index-sort-modified)
    ("C" zk-index-sort-created)
    ("S" zk-index-sort-size)
    ("q" nil)))

(defun zk-org-try-to-follow-link (fn &optional arg)
  "When 'org-open-at-point' FN fails, try 'zk-follow-link-at-point'.
Optional ARG."
  (let ((org-link-search-must-match-exact-headline t))
    (condition-case nil
	(apply fn arg)
      (error (unless (ignore-errors (zk-follow-link-at-point))
               (message "Invalid org-link type"))))))

(advice-add 'org-open-at-point :around #'zk-org-try-to-follow-link)

;;;; sdcv-mode

(use-package sdcv-mode
  :straight (sdcv-mode :host github :repo "gucong/emacs-sdcv")
  :defer 1)

;;;; ispell / flyspell

(use-package ispell
  :defer 3
  :config
  (setq ispell-program-name "/usr/local/bin/hunspell")
  (setq ispell-local-dictionary "en_US")
  )

(use-package flyspell
  :defer 3
  :bind
  (:map gr-map
        ("C-c" . flyspell-auto-correct-previous-word))
  ;;:custom-face
  ;;(flyspell-incorrect ((t nil)))
  )

;; (use-package flyspell-popup)

;; (dolist (hook '(text-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode 1))))

(with-eval-after-load "flyspell"
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  (define-key flyspell-mouse-map [mouse-3] #'undefined))

(use-package flyspell-correct
  :after flyspell
  :bind
  (:map flyspell-mode-map ("M-[" . flyspell-correct-wrapper))
  :config
  (setq flyspell-correct-interface #'flyspell-correct-completing-read)
  )

(use-package flyspell-correct-popup
  :after flyspell-correct
  )


;;;; autocorrect with abbrev

;; abbreviations and corrections stored in ~/.emacs.d/abbrev_defs

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(define-key ctl-x-map "\C-i"
  #'endless/ispell-word-then-abbrev)

(defun endless/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will be global.
If there's nothing wrong with the word at point, keep looking for
a typo until the beginning of buffer. You can skip typos you
don't want to fix with `SPC', and you can abort completely with
`C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (endless/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)


;;;; yasnippet

(use-package yasnippet
  :defer 2
  :diminish (yas-minor-mode)
  :config
  (yas-global-mode 1)
  (yas-reload-all))

;; (use-package yasnippet-multiple-key
;;   :defer 1
;;   :straight (yasnippet-multiple-key :host github :repo "ShuguangSun/yasnippet-multiple-key"))

;;;; org-reveal

(use-package ox-reveal
  :defer 2
  :config
  (setq org-reveal-root (format "file://%s/.reveal.js" home-dir))
  :custom
  (org-reveal-hlevel 2)
  )

(defun gr/inliner-create-single-html-file ()
  "Turn webpage into a single html file.
Uses 'inliner' npm utility to inline CSS, images, and javascript."
  (interactive)
  (let* ((orig (expand-file-name (read-file-name "File: ")))
         (origext (file-name-extension orig))
         (newfile (concat (file-name-sans-extension orig) " 2." origext)))
    (with-current-buffer (find-file-noselect orig)
      (goto-char (point-min))
      (replace-regexp "https://cdn.jsdelivr.net/npm/reveal.js/dist/theme/moon.css"
                      (file-relative-name "~/.reveal.js/dist/theme/moon.css")))
    (if (equal "html" origext)
        (async-shell-command (format "inliner '%s' > '%s'" orig newfile))
      (error "Must select .html file"))
    ))

;;;; pdf-tools

(use-package pdf-tools
  :straight (pdf-tools :host github :repo "vedang/pdf-tools" :fork "localauthor/pdf-tools")
  ;; :straight (pdf-tools :host github :repo "dalanicolai/pdf-tools"
  ;;                      :files (:defaults "lisp/*.el"
  ;;                                        "README"
  ;;                                        "vimura-server/*.py"
  ;;                                        ("build" "Makefile")
  ;;                                        ("build" "server")
  ;;       		                 (:exclude "lisp/tablist.el" "lisp/tablist-filter.el")))
  :defer 3
  :bind
  (:map pdf-view-mode-map
        ("h" . pdf-annot-add-highlight-markup-annotation)
        ("l" . pdf-annot-list-annotations)
        ("t" . pdf-annot-add-text-annotation)
        ("D" . pdf-annot-delete)
        ("C-s" . pdf-occur))
  :hook
  (pdf-annot-list-mode-hook . (lambda () (pdf-annot-list-follow-minor-mode)))
  (pdf-occur-buffer-mode-hook . next-error-follow-minor-mode)
  (pdf-view-mode-hook . (lambda () (setq-local make-backup-files nil)))
  ;;(pdf-view-mode-hook . pdf-keynav-minor-mode)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights")

  :config
  (pdf-tools-install)
  (require 'tablist)
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-view-use-scaling t)
  ;; turn off cua so copy works
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  (add-hook 'pdf-view-mode-hook (lambda () (linum-mode -1)))
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)

  ;; allow region highlighting with pdf-keynav
  (setq pdf-keynav-transient-mark-mode t)

  ;; set pdf-annot-list columns
  (setq pdf-annot-list-format '((page . 3)
                                (type . 10)
                                (label . 10)
                                (date . 10)))
  ;; display pdf-annot-list on left side
  (setq pdf-annot-list-display-buffer-action
        '((display-buffer-in-direction
           +select-buffer-in-direction)
          (direction . left)
          (window-width . 0.2)
          (inhibit-same-window . t))
        tablist-context-window-display-action
        '((display-buffer-in-side-window)
          (side . bottom)
          (slot . 0)
          (window-height . 0.1)
          (inhibit-same-window . t))
        pdf-annot-edit-contents-display-buffer-action
        '((display-buffer-reuse-window display-buffer-split-below-and-attach)
          (inhibit-same-window . t)
          (window-height . 0.2)))

  ;; funtion to save after adding comment
  (defun bjm/save-buffer-no-args ()
    "Save buffer ignoring arguments"
    (save-buffer))

  ;; set RET to save annotations
  (with-eval-after-load "pdf-annot"
    (define-key pdf-annot-edit-contents-minor-mode-map (kbd "RET") 'pdf-annot-edit-contents-commit)
    (define-key pdf-annot-edit-contents-minor-mode-map (kbd "S-RET") 'newline)

    (advice-add 'pdf-annot-edit-contents-commit :after 'bjm/save-buffer-no-args))
  )

(use-package org-pdftools
  :hook (org-mode-hook . org-pdftools-setup-link))


;;;; Pandoc

(use-package pandoc-mode
  :defer t)

;;;; LaTeX / AUCTeX

(use-package tex
  :straight auctex
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-engine 'luatex)

;; for syncing auctex with pdf-tools
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-mode t))

;; set wider margins in latex
(setq org-latex-packages-alist '(("margin=1in" "geometry" nil)))

;; pdflatex stopped working, for some reason (error "latexmk bad option")
(setq org-latex-compiler "xelatex")

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t)

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

;;(setq org-startup-with-latex-preview t)
(setq org-preview-latex-default-process 'dvisvgm)



;;;; ox-hugo

(use-package ox-hugo
  :defer 3
  :after org)

(defun gr/blog-deploy-localauthor ()
  (interactive)
  (shell-command "cd ~/Dropbox/Writings/localauthor && ./deploy.sh"))

(defun gr/blog-test-localauthor ()
  (interactive)
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (if
        (equal 1 (shell-command "pgrep 'hugo'"))
        (start-process-shell-command "hugo server" "*hugo server*" "cd ~/Dropbox/Writings/localauthor && hugo server")
      nil)
    (browse-url "http://localhost:1313/")))

(defun gr/web-deploy ()
  (interactive)
  (shell-command "cd ~/Dropbox/gr-web && ./deploy.sh"))

(defun gr/web-test ()
  (interactive)
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (if
        (equal 1 (shell-command "pgrep 'hugo'"))
        (start-process-shell-command "hugo server" "*hugo server*" "cd ~/Dropbox/gr-web && hugo server")
      nil)
    (browse-url "http://localhost:1313/")))

;;;; annotate.el

(use-package annotate
  ;; :hook (after-save-hook . annotate-save-annotations)
  :disabled
  :custom
  (annotate-summary-ask-query nil)
  (annotate-annotation-position-policy :by-length)
  :config
  (eval-and-compile
    (defhydra hydra-annotate (:hint nil
                                    :color blue)
      "
     ,*Annotate Mode*   _A_: Mode On/Off
  : next    _n_: new        _l_: load
  : prev    _s_: show all   _S_: save all
           "
      ("A" annotate-mode)
      ("a" annotate-annotate)
      ("n" annotate-annotate)
      ("s" annotate-show-annotation-summary)
      ("S" annotate-save-annotations)
      ("l" annotate-load-annotations)
      ("" annotate-goto-next-annotation :color red)
      ("" annotate-goto-previous-annotation :color red)
      ("q" nil)
      )))

;;;; org-wc

(use-package org-wc
  :after org
  :defer 1)

;; org-wc-display is useful


;;;; markdown mode

(use-package markdown-mode
  :disabled)


;;;; org-mind-map

(use-package org-mind-map
  :straight (:host github :repo "the-ted/org-mind-map")
  :defer t
  :config
  (setq org-mind-map-engine "dot")
  ;; if t, links don't work
  ;; error in org-mind-map-write-tags
  (setq org-mind-map-include-text nil)
  (setq org-mind-map-default-graph-attribs '(("autosize" . "false")
  ("size" . "9,12")
  ("resolution" . "100")
  ("nodesep" . "0.75")
  ("overlap" . "false")
  ("spline" . "true")
  ("rankdir" . "TB")))
  (setq org-mind-map-dot-output '("pdf" "png" "svg")))


;;;; emacs-benchmark

(use-package elisp-benchmarks
  :defer t)


;;; my-lisp


(use-package elfeed-setup
  :straight nil
  :defer t
  :commands (gr/elfeed-open-new-window))

(use-package misc-file-handling
  :straight nil
  :defer t)

(use-package text-to-speech
  :straight nil
  :defer t
  :commands (hydra-mac-speak/body))

(use-package devonthink-dir
  :straight nil
  :defer 1)

;; priv-lisp

(use-package dickinson
  :straight nil
  :defer 1)

(use-package mu4e-setup
  :straight nil
  :defer 1)

;;; Packages
;;;; magit

(use-package magit
  :bind
  ("C-c m" . magit-status)
  ("C-x m" . magit-status)
  :custom-face
  (diff-refine-added ((t (:background "yellow" :foreground "red"))))
  :custom
  (magit-diff-refine-hunk t))

;; (setq-default mode-line-format
;;               (delete '(vc-mode vc-mode) mode-line-format))



;;;; git-gutter

(use-package git-gutter
  :init
  :diminish (git-gutter-mode)
  :hook
  (emacs-lisp-mode-hook . git-gutter-mode)
  :config
  (set-face-foreground 'git-gutter:modified "orange")
  (set-face-foreground 'git-gutter:added    "forestgreen")
  (set-face-foreground 'git-gutter:deleted  "red")
  (set-fringe-mode '(8 . 0))
  )

;;;; esup

(use-package esup
  :defer t
  :custom
  (esup-user-init-file (concat user-emacs-directory "init.el"))
  :config
  (setq esup-depth 0))

;;;; re-builder

(use-package re-builder
  :defer 1
  :init
  (setq reb-re-syntax 'string))

;;;; ibuffer

(use-package ibuffer
  :straight (:type built-in)
  :defer 1
  :bind
  (:map ctl-x-map
        ("C-b" . ibuffer))
  (:map ibuffer-mode-map
        ("<backtab>". ibuffer-toggle-filter-group)
        ("TAB". ibuffer-toggle-filter-group))
  :hook
  (ibuffer-hook . gr/truncate-lines)
  (ibuffer-hook . gr/ibuffer-set-filter-group)
  :custom
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-auto-mode t)

  :config

  (defun gr/ibuffer-set-filter-group ()
    (ibuffer-switch-to-saved-filter-groups "default")
    (setq ibuffer-hidden-filter-groups (list "***" "ORG" "ZK" "el"))
    (ibuffer-update nil t))

  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 18 -1 :left))))

  (setq ibuffer-saved-filter-groups
        (list
         (cons "default"
               (append
                '(
                  ("Article" (or (and (filename . "/Academic Writing/*")
                                      (not (name . "magit")))))
                  ("Autumn 2021" (or (and (filename . "/Spring 2022/*")
                                          (not (name . "magit")))))
                  ("Writing" (or (and (filename . "/Writings/*")
                                      (not (name . "magit")))))
                  ("ZK" (or (name . "*ZK")
                            (and (filename . "/Zettels/")
                                 (not (name . "magit")))))
                  ("ORG" (or (and (filename . "/org/")
                                  (name . "\\.org$"))
                             (name . "^\\*calfw-calendar")))
                  ("PDF" (or (mode . pdf-view-mode)
                             (mode . pdf-occur-buffer-mode)
                             (mode . pdf-annot-list-mode)
                             (name . "^\\*Contents")
                             (name . "^\\*Edit Annotation ")))
                  ("magit" (name . "magit"))
                  ;;("dired" (or (name . ":~/")
                  ;;             (mode . dired-mode)))
                  ;;("helpful" (name . "^\\*helpful"))
                  ("el" (and (mode . emacs-lisp-mode)
                             (not (name . "^\\*scratch"))
                             (not (name . "init.el"))))
                  ("dired" (mode . dired-mode))
                  ("helpful" (mode . helpful-mode))
                  ("***" (or (name . "^\\*scratch")
                             (name . "init.el")
                             (name . "^\\*Messages")
                             (name . "^\\*mu4e-")
                             (name . "org_archive")
                             (name . ".persp")
                             ))
                  )))))
  )

(defun gr/truncate-lines (&rest _)
  (interactive)
  (let ((inhibit-message t))
    (unless (bound-and-true-p truncate-lines)
      (toggle-truncate-lines))))

(defun force-truncate-lines (&rest _)
  "Force line truncation. For use in hooks."
  (setq truncate-lines t))

;;;; dired

(use-package dired
  :defer t
  :straight (:type built-in)
  :bind
  ("C-x C-j" . dired-jump)
  ("C-x d" . dired-jump)
  ;;("C-x d" . dired)
  (:map dired-mode-map
        ("o" . link-hint-aw-select)
        ("C-x C-q" . dired-toggle-read-only))
  :hook
  (dired-mode-hook . dired-omit-mode)
  (dired-mode-hook . dired-hide-details-mode)
  (dired-omit-mode-hook . (lambda ()
                            (delete "~" dired-omit-extensions))) ;; show backup files
  :custom
  (dired-listing-switches "-algho --group-directories-first")
  (delete-by-moving-to-trash t)
  (dired-hide-details-mode t)
  :config
  (setq dired-omit-files "\\.DS_Store\\|\\.dropbox\\|Icon\\\015")
  (setq dired-kill-when-opening-new-dired-buffer t)

  )

;; to allow --group-directories-first to work on osx
(setq insert-directory-program "/usr/local/bin/gls" dired-use-ls-dired t)


(use-package all-the-icons-dired
  :defer t
  :hook (dired-mode-hook . all-the-icons-dired-mode)
  :diminish)


;;;; avy

(use-package avy
  :defer t
  :bind
  ("M-g w" . avy-goto-word-1)
  :bind*
  ;;("C-l" . gr/avy-goto)
  ("C-. C-," . gr/avy-goto-string)
  ("C-'" . gr/avy-goto-string)
  ;;("C-'" . avy-goto-char-timer)
  :custom
  (avy-timeout-seconds 0.4)
  :config
  (setq avy-keys '(?g ?d ?l ?r ?u ?e ?i ?c ?p ?f ?s))

  (setq avy-dispatch-alist '((?, . avy-action-embark)
                             (?j . avy-action-aw-select)
                             (?2 . avy-action-split-below)
                             (?n . avy-action-open-in-new-frame)
                             (?m . avy-action-mark)
                             (?w . avy-action-copy)
                             (?k . avy-action-kill-move)
                             (?K . avy-action-kill-stay)
                             (?  . avy-action-mark-to-char)
                             (?y . avy-action-yank)
                             (?$ . avy-action-ispell)
                             (?z . avy-action-zap-to-char)
                             (?h . avy-action-helpful)
                             ;;(?= . avy-action-define)
                             (?t . avy-action-teleport)))

  (defun gr/avy-goto-string (str &optional arg)
    "Jump to the currently visible STR.
The window scope is determined by `avy-all-windows' (ARG negates it)."
    (interactive (list (read-string "Input: ")
                       current-prefix-arg))
    (avy-with avy-goto-char
      (avy-jump
       (regexp-quote str)
       :window-flip arg)))

  (defun gr/avy-goto ()
    (interactive)
    (avy-goto-line)
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (avy-goto-word-0 nil beg end)))

  (defun avy-action-aw-select (pt)
    (if (> (length (aw-window-list)) 1)
        (let ((window (aw-select nil))
              (buffer (current-buffer))
              (new-buffer))
          (goto-char pt)
          (link-hint-open-link-at-point)
          (setq new-buffer (current-buffer))
          (switch-to-buffer buffer)
          (aw-switch-to-window window)
          (switch-to-buffer new-buffer))
      (link-hint-open-link-at-point)))

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (defun avy-action-split-below (pt)
    (goto-char pt)
    (delete-other-windows nil)
    (split-window-below nil))

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (defun avy-action-embark (pt)
    (goto-char pt)
    (embark-act))
  ;; (select-window
  ;;  (cdr (ring-ref avy-ring 0)))
  ;; t)

  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-open-in-new-frame (pt)
    (let ((buffer (current-buffer))
          (new-buffer))
      (goto-char pt)
      (link-hint-open-link-at-point)
      (setq new-buffer (current-buffer))
      (switch-to-buffer buffer)
      (gr/make-frame)
      (switch-to-buffer new-buffer))
    (link-hint-open-link-at-point))

  (define-key isearch-mode-map (kbd "C-'") 'avy-isearch)
  )

;; (defun gr/avy-goto-char-timer ()
;;   (interactive)
;;   (call-interactively #'avy-goto-char-timer)
;;   (forward-word)
;;   )


;;;; helpful

(use-package helpful
  :defer 1
  :bind
  (:map help-map
        ("f" . helpful-symbol)
        ("v" . helpful-symbol)
        ("h" . helpful-symbol)
        ("C-h" . helpful-symbol)
        ("k" . helpful-key)
        ;;("f" . helpful-callable)
        ;;("v" . helpful-variable)
        ("l" . find-library)
        ("?" . (lambda ()
                 (interactive)
                 (embark-bindings-in-keymap help-map))))
  (:map helpful-mode-map
        ("o" . link-hint-open-link))
  :custom
  (helpful-max-buffers 5)
  :config
  (with-eval-after-load 'semantic/symref/grep
    (add-to-list 'semantic-symref-filepattern-alist '(helpful-mode "*.el" "*.ede" ".emacs" "_emacs")))
  )

;;;; Undo-tree

(use-package undo-tree
  :diminish undo-tree-mode
  :defer 1
  ;; :init
  ;; (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil))

;;;; vundo

(use-package vundo
  :custom
  ;;(vundo-glyph-alist vundo-unicode-symbols)
  (vundo-roll-back-on-quit nil))

;;;; python

(use-package python-mode
  :disabled
  :defer 1
  :straight nil
  :config
  (setq python-shell-interpreter (format "%s/.pyenv/shims/python" home-dir))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (dot .t)))
  )

(with-eval-after-load "python"
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       "_"))))

;;;; wgrep

(use-package wgrep
  :defer t)

;;;; web browsing / eww / xwidget webkit /xwwp

(use-package eww
  :defer t
  :straight (:type built-in)
  :bind
  (:map eww-mode-map
        ("o" . link-hint-open-link))
  :config
  (setq shr-inhibit-images nil)
  (setq eww-search-prefix "https://html.duckduckgo.com/html/?q=")
  (setq eww-download-directory (expand-file-name "~/Downloads"))
  )

(use-package prot-eww
  ;;located in ~/.emacs.d/lisp/prot-eww.el
  :straight nil
  :defer 1
  :config
  (setq prot-eww-save-history-file
        (locate-user-emacs-file "prot-eww-visited-history"))
  (setq prot-eww-save-visited-history t)
  (setq prot-eww-bookmark-link nil)
  (define-prefix-command 'prot-eww-map)
  (define-key global-map (kbd "C-. w") 'prot-eww-map)
  (setq shr-folding-mode t
        shr-use-colors nil
        shr-bullet "• ")
  :hook
  (prot-eww-history-mode-hook . hl-line-mode)
  :bind
  (:map prot-eww-map
        ("b" . prot-eww-visit-bookmark)
        ("e" . prot-eww-browse-dwim)
        ("g" . eww-duckduckgo)
        ("w" . eww-wiki))
  (:map eww-mode-map
        ("B" . prot-eww-bookmark-page)
        ("D" . prot-eww-download-html)
        ("F" . prot-eww-find-feed)
        ("H" . prot-eww-list-history)
        ("b" . prot-eww-visit-bookmark)
        ("e" . prot-eww-browse-dwim)
        ("O" . prot-eww-open-in-other-window)
        ("E" . prot-eww-visit-url-on-page)
        ("J" . prot-eww-jump-to-url-on-page)
        ("R" . prot-eww-readable)
        ("Q" . prot-eww-quit)))

(defun eww-wiki ()
  "Function used to search Wikipedia for the given text."
  (interactive)
  (let* ((word (if (use-region-p)
                   (buffer-substring
                    (region-beginning)
                    (region-end))
                 (thing-at-point 'word)))
         (text (read-string "Wiki for: " word)))
    (eww (format "https://en.m.wikipedia.org/wiki/Special:Search?search=%s"
                 (url-encode-url text)))))

(defun eww-duckduckgo ()
  "Function used to search DuckDuckGo for the given text."
  (interactive)
  (let* ((word (if (use-region-p)
                   (buffer-substring
                    (region-beginning)
                    (region-end))
                 (thing-at-point 'word)))
         (text (read-string "DDG for: " word)))
    (eww (format "https://duckduckgo.com/?q=%s"
                 (url-encode-url text)))))

(defun gr/switch-browser (choice)
  (interactive (list (completing-read "Choose: " '(safari eww xwidget) nil t)))
   (let ((completion-ignore-case  t))
     (setq browse-url-browser-function
           (pcase choice
             ("safari" 'browse-url-default-browser)
             ("eww" 'eww)
             ("xwidget" 'xwwp-browse-url-other-window)))
     (message (format "browse-url set to `%s'" choice))))

(setq browse-url-generic-program "/usr/bin/open")
(setq browse-url-browser-function #'browse-url-default-browser)

(defvar gr/open-url-browsers-list '("eww" "xwidget" "safari")
  "Web browsers list.")

(defun gr/open-url-select-browser ()
  (interactive)
  (let ((browser (completing-read "Select web browser to open the url: " gr/open-url-browsers-list))
        (url (thing-at-point-url-at-point))
        (browse-url-generic-program "open")
        (browse-url-generic-args nil))
    (pcase browser
      ("eww" (eww-browse-url url t))
      ("safari" (browse-url-generic url))
      ("xwidget" (xwidget-webkit-browse-url url t)))))

(global-set-key (kbd "C-. q") ' gr/open-url-select-browser)

(use-package xwidget
  :defer t
  :config
  ;; causes problems if this variable is not defined
  (defvar xwidget-webkit-enable-plugins nil))

(use-package ctable
  :defer t)

(use-package xwwp-full
  :after xwidget
  :init (require 'xwwp-full)
  :straight (xwwp-full :host github
                       :repo "BlueFlo0d/xwwp"
                       :files (:defaults "*.js" "*.css"))
  :custom
  (xwwp-follow-link-completion-system 'default)
  :bind (:map xwidget-webkit-mode-map
              ("o" . xwwp-follow-link)
              ("l" . xwwp-ace-toggle)
              ("h" . xwwp-history-show)
              ("s" . xwwp-section)
              ("R" . xwwp-reader-toggle)))


;;;; pass

(use-package pass
  :defer t
  :after (embark consult)
  :custom
  (password-store-password-length 12)
  :init
  (setf epg-pinentry-mode 'loopback)

  ;; add embark actions for password-store
  (embark-define-keymap embark-password-store-actions
    "Keymap for actions for password-store."
    ("c" password-store-copy)
    ("f" password-store-copy-field)
    ("i" password-store-insert)
    ("I" password-store-generate)
    ("r" password-store-rename)
    ("e" password-store-edit)
    ("k" password-store-remove)
    ("U" password-store-url))

  (add-to-list 'embark-keymap-alist '(password-store . embark-password-store-actions))

  ;; Either add a prompt classifier or overwrite password-store--completing-read
  (add-to-list 'marginalia-prompt-categories '("Password entry" . password-store))

  ;; (defun password-store--completing-read ()
  ;;   "Read a password entry in the minibuffer, with completion."
  ;;   (completing-read
  ;;    "Password entry: "
  ;;    (let ((passwords (password-store-list)))
  ;;      (lambda (string pred action)
  ;;        (if (eq action 'metadata)
  ;;            '(metadata (category . password-store))
  ;;          (complete-with-action action passwords string pred))))))
  )

(use-package password-generator
  :custom
  (password-generator-custom-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890!@#$%^&*()+?")
  (password-generator-custom-length 12)
  )


;;;; ace-window

(use-package ace-window
  :straight (ace-window :host github :repo "fbuether/ace-window" :fork t
                        :files (:defaults "ace-window-posframe.el"))
  :defer 2
  :bind
  ("C-x o" . ace-window)
  :custom-face
  (aw-leading-char-face ((t (:family "Menlo" :foreground "red" :height 4.0))))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?l))
  (aw-scope 'visible)
  (aw-dispatch-always t)
  :config
  (ace-window-posframe-mode 1)
  (setq aw-dispatch-alist
        '(
          (?b aw-switch-buffer-in-window "Select Buffer in Target")
          (?w aw-swap-window "Swap Current and Target")
          (?m aw-copy-window "Move Current to Target")
          (?2 aw-split-window-vert "Split Vert Window")
          (?3 aw-split-window-horz "Split Horz Window")
          (?k aw-delete-window "Delete Window")
          (?0 aw-delete-window "Delete Window")

          ;;(?F aw-split-window-fair "Split Fair Window")
          ;;(?m aw-move-window "Move Curr. to Targ.")
          ;;(?n aw-flip-window "Flip Window")
          ;;(?J aw-switch-buffer-other-window "Select Buffer in Targ.")
          ;;(?o delete-other-windows "Delete Other Windows")
          ;;(?T aw-transpose-frame "Transpose Frame")
          ;; ?i ?r ?t are used by hyperbole.el
          ;;(?e aw-execute-command-other-window "Execute Command Other Window")

          (?? aw-show-dispatch-help)))

  (defun aw--consult-buffer ()
    (cond ((bound-and-true-p ivy-mode)
           (ivy-switch-buffer))
          ((bound-and-true-p ido-mode)
           (ido-switch-buffer))
          (t
           (call-interactively 'consult-buffer))))

  (advice-add #'aw--switch-buffer :override #'aw--consult-buffer)

  )


;;;; popper

(use-package popper
  :bind (("C-\\"   . popper-toggle-latest)
         ("M-\\"   . popper-cycle)
         ("C-M-\\" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("^dailynotes.org"
          "^magit\\:"
          "Output"
          "^\\*Messages\\*"
          "^\\*Warnings\\*"
          "^\\*sdcv\\*"
          "^\\*Backtrace\\*"
          "^\\*ZK-Index\\*"
          "^\\*ZK-Desktop"
          "^\\*ZK-Luhmann\\*"
          "^\\*Apropos\\*"
          "^\\*eshell\\*"
          "^\\*PDF-Occur\\*"
          "^\\*Org Agenda"
          "^\\*compilation"
          "^\\*elfeed-entry\\*"
          "^\\*calfw-details\\*"
          "^\\*Python\\*"
          "^\\*grep\\*"
          "^\\*undo-tree\\*"
          "^\\*Async Shell Command\\*"
          "^\\*Embark Collect\\*"
          "^\\*Google Translate\\*"
          "^\\*annotations\\*"
          "^\\*Ilist\\*"
          "^\\*Backups:"
          undo-tree-mode
          helpful-mode
          help-mode
          compilation-mode))
  (popper-mode 1)
  :config
  (setq popper-display-function #'popper-select-popup-at-bottom)
  (setq popper-display-control 'user))

;;;; expand-region

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;;; bookmark

(use-package bookmark
  :defer t
  :init
  (setq bookmark-bmenu-toggle-filenames nil
        bookmark-save-flag 1
        bookmark-set-fringe-mark nil)
  :custom-face
  (bookmark-face ((t nil))))


;;;; google-translate

(use-package google-translate
  :bind ("C-c t" . google-translate-smooth-translate)
  )

(use-package google-translate-smooth-ui
  :straight nil
  :defer t
  :config
  (setq google-translate-backend-method 'curl)

  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))

  (setq google-translate-translation-directions-alist
        '(("lt" . "en")
          ("en" . "lt")))

  (defun gr/google-translate-lt-en ()
    (interactive)
    (call-interactively #'google-translate-smooth-translate)
    (pop-to-buffer "*Google Translate*" 'display-buffer-below-selected))
  )

;;;; nov.el

(use-package nov
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))


;;;; paredit

(use-package paredit
  :defer t
  ;; :hook
  ;; (emacs-lisp-mode-hook . enable-paredit-mode)
  ;; (eval-expression-minibuffer-setup-hook . enable-paredit-mode)
  ;; (ielm-mode-hook . enable-paredit-mode)
  ;; (lisp-mode-hook . enable-paredit-mode)
  ;; (lisp-interaction-mode-hook . enable-paredit-mode)
  )

;;;; golden-ratio-scroll-screen

(use-package golden-ratio-scroll-screen
  :defer 1
  :config
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))

;;;; explain-pause-mode

(use-package explain-pause-mode
;;  :disabled
  :defer t
  :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
  :diminish
  :config
  (explain-pause-mode))

;;;; outshine-mode

(use-package outline-mode
  :straight nil
  :diminish
  :bind
  ("C-S-<right>" . outline-demote)
  ("C-S-<left>" . outline-promote)
  ("C-<right>" . outline-demote)
  ("C-<left>" . outline-promote)
  :custom-face
  (outline-1 ((t (:foreground "black" :weight bold :underline t))))
  (outline-2 ((t (:foreground "blue3" :underline nil))))
  :hook
  (outline-minor-mode-hook . (lambda () (diminish 'outline-minor-mode))))

(use-package outshine
  :defer 1
  :diminish
  :hook
  (nxml-mode-hook . outshine-mode)
  (emacs-lisp-mode-hook . outshine-mode)
  (outline-minor-mode-hook . outshine-mode))

;;;; whitespace-mode

(use-package whitespace
  :defer 1
  ;; :hook
  ;; (emacs-lisp-mode-hook . whitespace-mode)
  :custom
  (whitespace-style '(face trailing lines)))

;;;; flycheck and package-lint

(use-package flycheck-package
  :defer 1
  ;;:hook (emacs-lisp-mode-hook . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit))

(use-package package-lint
  :defer t)

;;;; visual-fill-column

(use-package visual-fill-column
  ;;only used in zk, as dir-local
  ;;because it doesn't work with git-gutter
  :hook
  (org-mode-hook . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 90))

;;;; hyperbole

(use-package hyperbole
  :disabled
  :diminish
  :config
  (hyperbole-mode 0))


;;;; nxml-mode

(use-package nxml-mode
  :straight nil
  :bind
  (:map nxml-mode-map
        ("C-<return>" . completion-at-point))
  :config
  (add-to-list 'rng-schema-locating-files "~/Dropbox/TEI/nxml-schemas/schemas.xml"))


;;; variable resets

(setq debug-on-error nil)
