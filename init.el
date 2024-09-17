;;; init.el                    -*- lexical-binding: t; -*-

;;; Startups

(setq debug-on-error t)

(use-package no-littering
  :demand t
  :init
  (setq custom-file (concat user-emacs-directory "etc/" "custom.el")
        backup-directory-alist
        `(("." . ,(concat user-emacs-directory "var/"
                          "backups/per-save")))))

(use-package recentf
  :defer 1
  :config
  (dolist (dir '("etc/" "var/"))
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name
                  (concat user-emacs-directory dir))))
  (recentf-mode 1))

(use-package diminish)

(use-package gcmh
  :load-path "lisp/"
  :diminish
  :config
  (gcmh-mode 1))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(dolist (dir '("lisp" "my-lisp" "my-lisp/priv-lisp"))
  (let ((exp-dir (expand-file-name (concat user-emacs-directory dir))))
    (add-to-list 'load-path exp-dir)))

(defun gr/recompile-lisp-dirs ()
  (interactive)
  (dolist (dir '("lisp" "my-lisp" "/my-lisp/priv-lisp"))
    (let ((exp-dir (expand-file-name (concat user-emacs-directory dir))))
      (byte-recompile-directory exp-dir 0 nil t))))

;; (add-to-list 'after-init-hook #'gr/recompile-lisp-dirs)

;;;; safe-local-variable-values

(setq safe-local-variable-values
      '((eval gr/daily-notes-new-dateline)
        (dired-omit-size-limit)
        (zk-link-and-title-format . "+%t [[%i]]+")
        (gr/mmd-citation-use . t)
        (eval . (gr/toggle-capslock))
        (eval . (text-scale-adjust 10))))

;;;; Basics

(use-package auth-source
  :config
  (setq epg-gpg-program "gpg2")
  ;; (add-to-list 'auth-sources 'macos-keychain-generic)
  ;; (add-to-list 'auth-sources 'macos-keychain-internet)
  ;; (auth-source-pass-enable)
  )

(use-package emacs
  :diminish
  eldoc-mode
  visual-line-mode
  abbrev-mode
  auto-fill-mode
  scroll-lock-mode
  :bind
  ("M-n" . scroll-up-command)
  ("M-p" . scroll-down-command)
  ("C-x [" . beginning-of-buffer)
  ("C-x ]" . end-of-buffer)
  ("C-c e" . eval-buffer)
  ("C-x e" . eval-last-sexp)
  ("C-x E" .  kmacro-end-and-call-macro)
  ("M-o" . other-window)
  ("<f2>" . nil)
  (:map help-mode-map
        ("o" . link-hint-open-link))
  ;; rest of config in early-init.el
  :hook
  (prog-mode-hook . (lambda () (setq show-trailing-whitespace t)))
  (prog-mode-hook . visual-line-mode)
  (text-mode-hook . visual-line-mode)
  (text-mode-hook . (lambda () (modify-syntax-entry ?’ "w")))
  (text-mode-hook . (lambda () (show-paren-local-mode -1)))
  :custom-face
  ;; what I want for every theme
  ;; (default ((t (:height 130 :font "JetBrains Mono"))))
  ;; (default ((t (:height 130 :font "Roboto Mono"))))
  ;; (default ((t (:height 140 :font "Consolas"))))
  ;; (default ((t (:height 130 :font "IBM Plex Mono"))))
  ;; (default ((t (:height 130 :font "Menlo"))))
  ;; (default ((t (:height 130 :font "Fira Mono"))))
  ;; (default ((t (:height 130 :font "Inconsolata"))))
  (default ((t (:height 130 :font "DejaVu Sans Mono"))))
  (fringe ((t (:background "gray90" :box (:line-width 1 :style released-button)))))
  :config
  (setq ad-redefinition-action 'accept)
  (setq user-emacs-directory "~/.emacs.d/"
        initial-buffer-choice "~/Dropbox/org/dailynotes.org"
        ns-use-proxy-icon nil
        create-lockfiles nil
        initial-major-mode 'lisp-interaction-mode
        initial-scratch-message nil
        set-mark-command-repeat-pop t
        use-dialog-box nil
        confirm-kill-emacs 'y-or-n-p
        minibuffer-follows-selected-frame nil
        sentence-end-double-space nil
        find-library-include-other-files nil
        vc-follow-symlinks t)

  (setq-default indent-tabs-mode nil ;; use spaces for tabs
                fill-column 77)

  ;; compilation

  (setq warning-suppress-types (quote (bytecomp comp))
        native-comp-async-report-warnings-errors 'silent
        warning-minimum-level ':error
        ring-bell-function 'ignore
        byte-compile-warnings '((not cl-functions)))

  ;;(setq recenter-positions '(middle bottom top))

  ;; yes-or-no function

  (setq y-or-n-p-use-read-key t ;; needed for embark
        use-short-answers t)

  (define-advice y-or-n-p
      (:around (orig-func &rest args) y-or-n-p-with-return)
    "Allow RET as affirmative to y-or-n-p."
    (let ((query-replace-map (copy-keymap query-replace-map)))
      (define-key query-replace-map (kbd "RET") 'act)
      (define-key query-replace-map (kbd "<return>") 'act)
      (apply orig-func args)))

  ;; modes
  (repeat-mode 1)
  (auto-save-visited-mode 1)
  (desktop-save-mode -1)
  (global-auto-revert-mode t)
  (delete-selection-mode 1)
  (global-hl-line-mode 0)
  (winner-mode 1)
  (transient-mark-mode 1)
  ;;(global-visual-line-mode 1))
  (blink-cursor-mode -1)
  (pixel-scroll-precision-mode)

  ;; for left and right fringe/margin
  (define-advice mwheel-scroll
      (:override (event &optional arg) pixel-scroll-precision))
  )

(with-current-buffer "*Messages*"
  (visual-line-mode))

(with-current-buffer "*scratch*"
  (visual-line-mode))

;;;; backups

(setq make-backup-files t
      vc-make-backup-files t
      version-control t ;; Use version numbers for backups.
      kept-new-versions 6 ;; Number of newest versions to keep.
      kept-old-versions 3 ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t ;; Copy all files, don't rename them.
      )

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist `(("." . ,(concat user-emacs-directory "var/" "backups/per-session"))))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(with-eval-after-load 'zk
  (add-to-list 'backup-directory-alist
               `(,zk-id-regexp . ,(concat user-emacs-directory "var/" "backups/per-save/ZK-backups")))

  (defun force-backup-of-buffer ()
    ;; Make a special "per session" backup at the first save of each
    ;; emacs session.
    (when (not buffer-backed-up)
      ;; Override the default parameters for per-session backups.
      (let ((backup-directory-alist `((,zk-id-regexp . ,(concat user-emacs-directory "var/" "backups/per-session/ZK-backups"))
                                      ("." . ,(concat user-emacs-directory "var/" "backups/per-session"))))
            (kept-new-versions 3))
        (backup-buffer)))
    ;; Make a "per save" backup on each save.  The first save results in
    ;; both a per-session and a per-save backup, to keep the numbering
    ;; of per-save backups consistent.
    (let ((buffer-backed-up nil))
      (backup-buffer))))

(add-hook 'before-save-hook 'force-backup-of-buffer)

;;;; trash

(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash")

(defun system-move-file-to-trash (path)
  "Moves file at PATH to macOS Trash following `move-file-to-trash' convention.

      Relies on the command-line utility 'trash' to be installed.
      Get it from:  <http://hasseg.org/trash/>"
  (shell-command (concat "trash -vF \"" path "\""
                         "| sed -e 's/^/Trashed: /'")
                 nil ;; Name of output buffer
                 "*Trash Error Buffer*"))

;;;; mode-line

(custom-set-faces
 `(mode-line ((t ( :family "Menlo"
                   :height 110
                   :box (:line-width -1 :style released-button)
                   ;; :background "gray75"
                   ;; :foreground "black"
                   ))))
 `(mode-line-buffer-id ((t (:weight bold))))
 `(mode-line-emphasis ((t (:weight bold))))
 `(header-line ((t (:inherit (mode-line)))))
 )

;; truncate buffer name in mode-line to 60 characters
(setq-default mode-line-buffer-identification
              (append '(-60)
                      (propertized-buffer-identification "%b")))

(add-hook 'text-mode-hook '(lambda () (line-number-mode -1)))

(setq-default mode-line-format
              '("  "
                display-time-string ;; left align
                mode-line-frame-identification
                mode-line-buffer-identification
                "  "
                mode-line-position
                "  "
                mode-line-modes
                "  "
                vc-mode
                "  "
                mode-line-format-right-align
                mode-line-misc-info
                "  "
                ))

;; (setq-default mode-line-format
;;               '("  "
;;                 display-time-string ;; left align
;;                 mode-line-frame-identification
;;                 mode-line-buffer-identification
;;                 "  "
;;                 mode-line-position
;;                 "              "
;;                 mode-line-modes
;;                 ;;
;;                 "              "
;;                 mode-line-misc-info
;;                 "              "
;;                 vc-mode
;;                 "  "))

(use-package time
  :init
  (display-time-mode 1)
  :custom
  (display-time-24hr-format t)
  (display-time-day-and-date nil)
  (display-time-default-load-average nil)
  (display-time-format "[%H:%M]") ;; put time in brackets
  :config
  (setq global-mode-string '(""))) ;; remove display-time-string from right


;;;; tab-bar

(use-package tab-bar
  :defer 1
  :bind
  ("s-{" . tab-bar-switch-to-prev-tab)
  ("s-}" . tab-bar-switch-to-next-tab)
  ("C-{" . tab-bar-switch-to-prev-tab)
  ("C-}" . tab-bar-switch-to-next-tab)
  ("M-s-n" . gr/tab-to-frame)
  :custom-face

  (tab-bar ((t (:background "grey90" :foreground "black" :font "Menlo" :height 110))))
  (tab-bar-tab ((t (:background "gray40" :foreground "gray90" :box (:line-width 1 :style released-button)))))
  (tab-bar-tab-inactive ((t (:background "gray60" :foreground "gray80":box (:line-width 1 :style pressed-button)))))

  :custom
  (tab-bar-show 1)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-auto-width nil)
  (tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
  :config

  (defun gr/tab-to-frame ()
    "Open current tab in new frame."
    (interactive)
    (let* ((buffer (current-buffer)))
      (when (< 1 (length (tab-bar-tabs (window-frame))))
        (tab-close))
      (gr/make-frame)
      (switch-to-buffer buffer))))

(defun gr/reinstall-package (pkg)
  (interactive (list (intern
                      (completing-read
                       "Reinstall package: "
                       (mapcar #'car package-alist)))))
  (unload-feature pkg)
  (package-reinstall pkg)
  (require pkg))

(use-package markdown-mode :defer 1)

(use-package hydra :defer 1)

(use-package keycast :defer 1)

;;;; themes

(setq custom-theme-directory "~/.emacs.d/var/themes")

(load-theme 'gr-light t)
(load-theme 'gr-dark t t)

(defun gr/toggle-theme ()
  (interactive)
  (if (equal custom-enabled-themes '(gr-light))
      (progn
        (mapc #'disable-theme custom-enabled-themes)
        (enable-theme 'gr-dark)
        (setq-default org-hide-leading-stars nil)
        (setq-default org-superstar-leading-bullet ?\s)
        (setq-default org-indent-mode-turns-on-hiding-stars nil))
    (progn
      (mapc #'disable-theme custom-enabled-themes)
      (enable-theme 'gr-light)
      (setq-default org-hide-leading-stars t)
      (setq-default org-superstar-leading-bullet ?\s)
      (setq-default org-indent-mode-turns-on-hiding-stars t))))

;;;; MacOS Keybindings

;; MacOS Keyboard Shortcuts
(bind-keys*
 ("s-v" . yank)
 ("s-x" . kill-region)
 ("s-c" . kill-ring-save)
 ("s-s" . save-buffer)
 ("s-z" . undo)
 ("s-q" . save-buffers-kill-emacs)
 ("s-f" . consult-line)
 ("s-t" . tab-new))

(setq ns-alternate-modifier 'meta)
(setq ns-command-modifier 'super)

(when (eq system-type 'darwin)
  (setq mac-right-command-modifier 'control))

;;;; Bold, italics, underline functions

(defmacro surround (name key-bind symbol &optional symbol-two)
  (let ((func-name (intern (concat "surround-" (symbol-name name)))))
    `(progn
       (defun ,func-name ()
         (interactive)
         (cond ((region-active-p)
                (progn
                  (let ((beg (region-beginning))
                        (end (1+ (region-end))))
                    (goto-char beg)
                    (insert ,symbol)
                    (goto-char end)
                    (insert ,(if symbol-two
                                 symbol-two
                               symbol)))))
               ((word-at-point)
                (forward-char)
                (backward-word)
                (insert ,symbol)
                (forward-word)
                (insert ,(if symbol-two
                             symbol-two
                           symbol)))
               (t
                (insert ,symbol)
                (insert ,(if symbol-two
                             symbol-two
                           symbol))
                (forward-char -1))))
       (keymap-global-set ,key-bind ',func-name))))

(surround quotes "s-\"" "\"")
(surround bold "s-b" "\*")
(surround italics "s-i" "\/")
(surround underline "s-u" "\_")
(surround parens "s-\(" "\(" "\)")
(surround brackets "s-\[" "\[" "\]")

(bind-keys*
 ("s-\/" . surround-italics)
 ("s-\*" . surround-bold)
 ("s-\_" . surround-underline))

;;;; display-buffer-alist

(setq switch-to-buffer-obey-display-actions t)
;; means that bookmark-jump-to-frame will not work on files/dirs defined
;; below

(setq display-buffer-alist
      `(("*Org-Side-Tree*\\|^<tree>\\|\\*Embark Live"
         (display-buffer-in-side-window)
         (post-command-select-window t)
         (side . left))

        ((major-mode . dired-mode)
         (display-buffer-at-bottom)
         (post-command-select-window t)
         (window-height . 0.45))

        ((major-mode . magit-status-mode)
         (display-buffer-at-bottom)
         (post-command-select-window t)
         (window-height . 0.6))

        ("CAPTURE-"
         (display-buffer-at-bottom)
         (post-command-select-window t)
         (window-height . 0.4))

        ("\\*elfeed-entry\\|*info"
         (display-buffer-at-bottom)
         (window-height . 0.75))

        ("*mu4e-main*"
         (display-buffer-full-frame))

        ("*Async Shell Command*"
         (display-buffer-no-window))

        ("Org Links"
         (display-buffer-no-window)
         (allow-no-window . t))

        (,(concat
           "\\*\\("
           (string-join
            '("ZK-Index" "Occur" "Backups:"
              "helpful" "Pp Eval Output"
              "eshell" "Google Translate" "Org Select"
              "annotations" "Embark Collect")
            "\\|") "\\)")
         (display-buffer-at-bottom)
         (post-command-select-window t)
         (window-height . 0.4))

        (,(concat
           "\\*\\("
           (string-join
            '("Messages" "Org-Babel" "trace-output" "*Completions*"
              "Warnings" "Compile-Log" "[Hh]elp"
              "calfw-details")
            "\\|") "\\)")
         (display-buffer-at-bottom)
         (window-height . 0.3))
        ))

;;;; gr-functions and gr-map

(use-package gr-functions
  :ensure nil
  :init
  (define-prefix-command 'gr-map)
  (bind-keys :map global-map
             :prefix-map gr-map
             :prefix "C-."
             ("/" . switch-to-minibuffer-window)
             ("C-/" . exit-minibuffer)
             ("n" . gr/daily-notes)
             ("C-n" . (lambda ()
                        (interactive)
                        (gr/daily-notes '(16))))
             ("N" . gr/daily-notes-new-headline)
             ("i" . gr/open-init-file)
             ("T" . gr/toggle-theme)
             ("D" . gr/lookup-word-at-point)
             ("L" . toggle-truncate-lines)
             ("H" . gr/toggle-headphones))
  :bind*
  ("s-n" . gr/make-frame)
  ("s-w" . gr/delete-frame-or-tab)
  ("M-c" . ct/capitalize-word-at-point)
  ("M-u" . ct/upcase-word-at-point)
  ("M-l" . ct/downcase-word-at-point)
  ("M-U" . title-case-region)
  ("C-M-;" . gr/comment-and-copy)
  ("C-o" . gr/insert-line)
  ("<f12>" . gr/toggle-capslock))

(use-package text-to-speech
  :ensure nil
  :defer t
  :commands hydra-mac-speak/body)

(use-package devonthink-dir
  :ensure nil
  :defer 1)

(use-package dickinson
  :ensure nil
  :defer 1)

(use-package pdf-pagelabels
  :load-path "my-lisp/pdf-pagelabels"
  :ensure nil
  :defer 1)

;;;; erc

(use-package erc
  :custom
  (erc-server "irc.libera.chat")
  (erc-nick "localauthor")
  (erc-prompt-for-password nil)
  (erc-kill-buffer-on-part t)
  (erc-auto-query 'bury)
  (erc-autojoin-channels-alist '((Libera.Chat "#emacs" "#org-mode" "#systemcrafters"))))

;;;; elec-pair

(use-package elec-pair
  :init
  (electric-pair-mode)
  :config
  (setq electric-pair-inhibit-predicate
        (lambda (c)
          (or
           (char-equal c ?\>)
           (char-equal c ?\<)))))

(use-package electric
  :init
  (electric-quote-mode)
  :custom
  (electric-quote-context-sensitive t)
  (electric-quote-replace-double t))

;;;; info

(use-package info
  :bind
  (:map Info-mode-map
        ("o" . link-hint-open-link)))

;;;; expand-region

(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))

;;;; bookmark

(use-package bookmark
  :defer t
  :init
  (setq bookmark-bmenu-toggle-filenames nil
        bookmark-save-flag 1
        bookmark-fringe-mark nil))

(define-advice bookmark-default-handler
    (:around (orig-fun bmk-record)
             gr/bookmark-find-from-dir-or-default)
  "Around advice for bookmark-default-handler.
Calls through unless bookmark is a directory, in which case, calls counsel-find-file."
  (let ((file (bookmark-get-filename bmk-record)))
    (if (file-directory-p file)
        (let ((default-directory file))
          (call-interactively 'find-file))
      (funcall orig-fun bmk-record))))

;;;; register

(use-package consult-register
  :ensure nil
  :bind
  ("M-#" . consult-register-load)
  ("M-'" . consult-register-store)
  ("C-M-#" . consult-register)
  :config
  (setq register-preview-function #'consult-register-format)
  (define-advice register-preview
      (:override (buffer &optional show-empty) consult-register-window)))

;;;; isearch

(use-package isearch
  :ensure nil
  :bind
  (:map isearch-mode-map
        ("C-n" . isearch-repeat-forward)
        ("C-p" . isearch-repeat-backward))
  :custom-face
  (lazy-highlight ((t (:background "turquoise2"))))
  :custom
  (search-default-mode 'char-fold-to-regexp) ;; ignore diacritics
  (isearch-lazy-count t)
  (isearch-repeat-on-direction-change t))

;;;; re-builder

(use-package re-builder
  :defer 1
  :init
  (setq reb-re-syntax 'string))

;;;; init-lock

(use-package init-lock
  :load-path "my-lisp/init-lock/"
  :defer t
  :commands (init-lock)
  :custom
  (init-lock-files '("~/.emacs.d/init.el")))

;;;; link-hint

(use-package link-hint
  :defer 1
  :custom
  (link-hint-message nil))

(use-package link-hint-aw-select
  :load-path "my-lisp/link-hint-aw-select/"
  :bind
  (:map gr-map
        ("o" . link-hint-aw-select))
  :custom
  (link-hint-aw-select-dispatch-alist
   '((?2 . avy-action-split-below)
     (?n . avy-action-open-in-new-frame)
     (?t . avy-action-open-in-new-tab)
     (?w . avy-action-copy)
     (?, . avy-action-embark)))

  :config
  (add-to-list 'link-hint-aw-select-ignored-buffers 'org-side-tree-mode)
  (add-to-list 'link-hint-aw-select-ignored-buffers 'zk-index-mode)
  ;; open org-links in same window
  ;; allows link-hint--aw-select-org-link to work properly
  (with-eval-after-load 'org
    (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)))

(use-package link-hint-preview
  :load-path "my-lisp/link-hint-preview/"
  :bind
  (:map gr-map
        ("p" . link-hint-preview))
  :hook
  (link-hint-preview-mode-hook . link-hint-preview-toggle-frame-mode-line))

;;;; savehist

(use-package savehist
  :defer 1
  :config
  (savehist-mode 1)
  (setq savehist-additional-variables
        '(register-alist kill-ring citar-history search-ring regexp-search-ring)))


;;; Org

;;;; org-mode

(use-package org
  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  ("C-c C" . org-clock-goto)
  (:map org-mode-map
        ("C-c ;" . nil)
        ("<tab>" . org-cycle)
        ("C-c ," . org-insert-structure-template)
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
        ("C-<return>" . org-meta-return)
        ("M-<return>" . org-insert-heading-respect-content)
        ("" . org-cycle-agenda-files))
  :mode (("\\.org$" . org-mode))
  :init
  (setq org-export-backends '(reveal ascii html latex md odt org))
  (with-eval-after-load 'org
    (setq org-structure-template-alist
          '(("c" . "comment")
            ("q" . "quote")
            ("n" . "notes")
            ("s" . "src")
            ("v" . "verse")
            ("el" . "src emacs-lisp")
            ("C" . "center"))))
  (with-eval-after-load 'org-indent
    (diminish 'org-indent-mode))
  (with-eval-after-load 'org-num
    (diminish 'org-num-mode))

  :custom-face
  (org-drawer ((t (:height .8))))
  (org-special-keyword ((t (:height .8))))
  (org-ellipsis ((t (:inherit fixed-pitch :foreground "gray50" :underline nil))))
  (org-hide ((t (:foreground ,(face-attribute 'default :background)))))

  :custom

  (org-directory "~/Dropbox/org")
  (org-ellipsis " ▼") ;◣ ▼ ▽ ► ➽
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-startup-truncated nil)
  (org-tags-column -67)
  (org-use-fast-todo-selection 'expert)
  (org-log-done 'time)
  (org-log-states-order-reversed nil)
  (org-emphasis-alist
   '(("*" bold)
     ("/" italic)
     ("_" underline)
     ("=" org-verbatim verbatim)
     ("+" (:background "gray85" :height .9))
     ("~" verbatim)))
  (org-archive-location "%s_archive::datetree/")
  (org-footnote-section nil)
  (org-generic-id-locations-file (concat user-emacs-directory "var/"  "org/.org-generic-id-locations"))

  ;; org-export
  (org-export-with-smart-quotes t)
  (org-export-with-toc nil)
  (org-export-with-section-numbers nil)
  (org-export-with-tags nil)
  (org-export-with-date nil)
  (org-export-dispatch-use-expert-ui t)

  ;; org-cycle
  (org-cycle-separator-lines -1)  ;; spacing between headings

  ;; org-keys
  (org-return-follows-link t)
  (org-use-speed-commands t)
  (org-speed-commands
   '(("Outline Navigation")
     ("n" . gr/org-next-heading)
     ("p" . gr/org-previous-heading)
     ("Outline Visibility")
     ("i" . org-cycle)
     ("Clock Commands")
     ("I" . org-clock-in)
     ("O" . org-clock-out)
     ("Misc")
     ("?" . org-speed-command-help)))

  ;; ol
  (org-link-keep-stored-after-insertion t)
  (org-link-search-must-match-exact-headline t)

  ;; org-num
  (org-num-skip-tags '("nonum"))
  (org-num-skip-commented t)
  (org-num-skip-footnotes t)

  ;; org-refile
  ;; (org-refile-targets '((nil :maxlevel . 2)
  ;;                       (org-agenda-files :maxlevel . 2)))
  ;; (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)

  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)

  :config
  (unbind-key "C-," org-mode-map)
  (unbind-key "C-'" org-mode-map)
  (add-to-list 'org-file-apps '("\\.docx\\'" . default) 'append)

  (defmacro gr/org-heading-function (name)
    `(defun ,(intern (concat "gr/org-" name "-heading")) ()
       (interactive)
       (let (org-side-tree-narrow-on-jump)
         (if (org-buffer-narrowed-p)
             (progn
               (setq org-side-tree-narrow-on-jump t)
               (,(intern (concat "org-side-tree-" name "-heading"))))
           (org-speed-move-safe ',(intern (concat "org-" name "-visible-heading")))
           (org-side-tree-update)))))

  (gr/org-heading-function "next")
  (gr/org-heading-function "previous")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)))

  (defun org-babel-execute:yaml (body params) body))

(use-package gr-org-extras
  :ensure nil
  :after org
  :bind
  (:map org-mode-map
        ("C-c n" . narrow-or-widen-dwim)
        ("C-c $" . gr/org-mark-done-and-archive-datetree)
        ("RET" . gr/org-return)))

(use-package org-agenda-setup
  :ensure nil
  :defer 1
  :after org
  :bind
  (:map gr-map
        ("a" . gr/org-agenda)))

(use-package org-capture-setup
  :ensure nil
  :after org
  :defer 1
  :bind
  (:map gr-map
        ("c" . org-capture)))


;;;; org-contrib

(use-package org-contrib)

(remove-hook 'org-mode-hook #'org-eldoc-load)

(use-package ox-extra
  :ensure nil
  :defer 1
  :config
  (ox-extras-activate '(latex-header-blocks ignore-headlines))

  (define-advice org-export-ignore-headlines
      (:override (data backend info) gr/org-export-ignore-headlines)
    "Remove headlines tagged \"ignore\" or \"noheadline\" retaining
contents and promoting children. Each headline tagged \"ignore\"
or \"noheadline\" will be removed retaining its contents and
promoting any children headlines to the level of the parent."
    (org-element-map data 'headline
      (lambda (object)
        (when (or (member "noheadline" (org-element-property :tags object))
                  (member "ignore" (org-element-property :tags object)))
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

;;;; org-superstar

(use-package org-superstar
  :defer t
  :after org
  :hook (org-mode-hook)
  :custom
  ;; fixes org-hide on theme-change
  ;; (org-superstar-leading-bullet ?\s)
  ;; (org-hide-leading-stars nil)
  ;; (org-indent-mode-turns-on-hiding-stars nil)
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list  '("•" "➤"))
  ;;  "◉" "○" "▪" "➤"
  (org-superstar-item-bullet-alist
   '((?+ . ?◦)
     (?* . ?➤)
     (?- . ?–))))

;;; Completion

;;;; vertico

(use-package vertico
  :init (vertico-mode 1)
  :after orderless
  :bind* (:map vertico-map
               ("C-x C-j" . consult-dir-jump-file)
               ("C-j" . vertico-exit-input)
               ("C-g" . keyboard-escape-quit))
  :hook
  (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  :custom
  (vertico-cycle t)
  (vertico-count 7)

  :config
  (vertico-multiform-mode)

  (setq vertico-multiform-commands
        '((consult-imenu buffer)
          ;;(gr/citar-insert-citation buffer)
          ;;(execute-extended-command buffer)
          (consult-dir buffer)))

  (setq vertico-multiform-categories
        '((file buffer)
          ;;(vertico-sort-function . sort-directories-first-alpha))
          (zk-file buffer
                   (vertico-sort-function . gr/sort-modified))
          (embark-keybinding grid)
          (consult-mu-messages buffer)
          (bookmark buffer)
          (consult-grep buffer)))

  (defun sort-directories-first-alpha (files)
    (setq files (vertico-sort-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  (setq vertico-buffer-display-action '(display-buffer-in-side-window
                                        (window-height . 0.3)
                                        (side . bottom)))
  ;; (setq crm-separator ",")

  ;;; vertico sort modified

  (defun gr/sort-modified (list)
    "Sort LIST of files for latest modified."
    (let ((ht (make-hash-table :test #'equal :size 5000)))
      (dolist (x list)
        (puthash x (file-attribute-modification-time (file-attributes x)) ht))
      (sort list
            (lambda (a b)
              (let ((one
                     (gethash a ht))
                    (two
                     (gethash b ht)))
                (time-less-p two one))))))

  (defun vertico-sort-modified ()
    (interactive)
    (setq-local vertico-sort-override-function
                (and (not vertico-sort-override-function)
                     #'gr/sort-modified)
                vertico--input t))

  (keymap-set vertico-map "M-M" #'vertico-sort-modified)

  )

;; Add prompt indicator to `completing-read-multiple'.

(define-advice completing-read-multile
    (:filter-args (args) crm-indicator)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

(minibuffer-depth-indicate-mode 1)

;;;; embark

(use-package embark
  :bind
  ("C-," . embark-act)
  ("C->" . embark-act-noquit)
  ("C-<" . embark-act-all)
  ("M-," . embark-dwim)
  ("C-h b" . embark-bindings)
  (:map embark-general-map
        ("," . embark-select))
  (:map embark-identifier-map
        ("$" . ispell-region)
        ("d" . sdcv-search)
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
        ("t" . find-file-other-tab)
        ("k" . embark-copy-as-kill)
        ("K" . gr/copy-file-as-org-link)
        ("L" . gr/insert-file-as-org-link)
        ("M" . mail-add-attachment))
  (:map embark-region-map
        ("t" . title-case-region)
        ("G w" . eww-wiki)
        ("G g" . eww-duckduckgo)
        ("z" . zk-search))
  (:map embark-url-map
        ("s" . browse-url-generic)
        ("f" . browse-url-firefox))
  :custom
  (embark-help-key "?")
  (embark-keymap-prompter-key ",")
  (embark-quit-after-action t)
  (embark-confirm-act-all nil)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (prefix-help-command #'embark-prefix-help-command)
  :config


  (defun embark-act-noquit ()
    "Run action but don't quit the minibuffer afterwards."
    (interactive)
    (embark-act t))

  (defun gr/org-link-mail-attach-file (file)
    (interactive "fAttach file: ")
    (when (f-file-p file)
      (mail-add-attachment file)))

  (defun embark--simplify-path (_type target)
    "Simplify and '//' or '~/' in the TARGET file path."
    ;; FIX for: https://github.com/oantolin/embark/issues/704
    (cons 'file (abbreviate-file-name
                 (expand-file-name
                  (substitute-in-file-name target)))))

  (defun gr/copy-file-as-org-link (filename &optional wildcards)
    "Copy FILENAME as org-link with optional short filename description."
    (interactive
     (find-file-read-args "Find file: "
                          (confirm-nonexistent-file-or-buffer)))
    (let ((description
           (when (y-or-n-p "Short filename?")
             (concat "][" (file-name-nondirectory filename)))))
      (kill-new (concat "[[" filename
                        description
                        "]]"))))

  (defun gr/insert-file-as-org-link (filename &optional wildcards)
    "Insert FILENAME as org-link with optional short filename description."
    (interactive
     (find-file-read-args "Find file: "
                          (confirm-nonexistent-file-or-buffer)))
    (let ((description
           (when (y-or-n-p "Short filename?")
             (concat "][" (file-name-nondirectory filename)))))
      (insert (concat "[[" filename
                      description
                      "]]"))))

  (setq prefix-help-command #'embark-prefix-help-command)

  ;; no completing read; (type "?" for completing read prompter)
  (setq embark-prompter 'embark-keymap-prompter)

  ;; from https://karthinks.com/software/fifteen-ways-to-use-embark/
  (eval-when-compile
    (defmacro embark-aw-select (fn)
      `(defun ,(intern (concat "embark-aw-select-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn))))))

  (with-eval-after-load 'embark
    (define-key embark-file-map (kbd "o") (embark-aw-select find-file))
    (define-key embark-buffer-map (kbd "o") (embark-aw-select switch-to-buffer))
    (define-key embark-bookmark-map (kbd "o") (embark-aw-select bookmark-jump)))

  )

(use-package embark-org
  :ensure nil
  :after (embark org)
  :bind
  (:map embark-org-link-map
        ("l" . org-insert-link)
        ("M" . gr/org-link-mail-attach-file)))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

;;;; consult

(use-package consult
  :after (embark)
  :bind
  ;;("C-s" . consult-line)
  ("C-x b" . consult-buffer)
  ("M-y" . consult-yank-pop)
  ;; C-c bindings in `mode-specific-map'
  ("C-c M-x" . consult-mode-command)
  ("C-c h" . consult-history)
  ("C-c k" . consult-kmacro)
  ("C-c i" . consult-info)
  ([remap Info-search] . consult-info)
  ;; M-g bindings in `goto-map'
  ("M-g f" . consult-flymake)
  ("M-g g" . consult-goto-line)
  ("M-g M-g" . consult-goto-line)
  ("M-g o" . consult-outline)
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ;; M-s bindings in `search-map'
  ("M-s d" . consult-find)
  ("M-s D" . consult-locate)
  ("M-s f" . consult-find)
  ("M-s s" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-goto-line)
  ("M-s L" . consult-line-multi)
  ("M-s m" . multi-occur)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  (:map consult-narrow-map
        ("M-?" . consult-narrow-help))
  (:map gr-map
        ("b" . consult-bookmark))
  :bind*
  ("C-. [" . consult-global-mark)
  ("C-:" . consult-imenu)
  ("C-;" . consult-outline)
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode)
  :custom-face
  (consult-preview-line ((t (:inherit default))))
  :custom
  (completion-in-region-function 'consult-completion-in-region)
  (consult-fontify-preserve t)
  (consult-project-function nil)
  (consult-async-split-style 'semicolon)
  (consult-preview-key "C-{")
  (consult-locate-args "mdfind -name")

  :config

  ;; consult-preview settings

  (consult-customize
   consult-git-grep consult-grep consult-mark consult-line
   consult-xref consult-ripgrep consult-global-mark
   consult-goto-line
   :preview-key 'any)

  ;;make C-s and C-r search forward and backward in consult-line
  ;;changed to make C-s call previous search term
  ;; (defvar my-consult-line-map
  ;;   (let ((map (make-sparse-keymap)))
  ;;     (define-key map "\C-s" #'previous-history-element)
  ;;     ;;(define-key map "\C-s" #'next-line)
  ;;     ;;(define-key map "\C-r" #'previous-line)
  ;;     map))

  ;; (consult-customize consult-line :keymap my-consult-line-map)

  (defun gr/consult-ripgrep-select-dir ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'consult-ripgrep)))

  (defun gr/consult-find-select-dir ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'consult-find)))
  )

;;;; consult-dir

(use-package consult-dir
  :bind ("C-x C-d" . consult-dir)
  :custom
  (consult-dir-sources '(consult-dir--source-bookmark
                         consult-dir--source-recentf)))

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

(add-hook 'dired-after-readin-hook #'recentd-track-opened-file)
(add-hook 'kill-buffer-hook #'recentd-track-closed-file)

;;;; marginalia

(use-package marginalia
  :demand t
  :commands marginalia-mode
  :bind
  ("M-A" . marginalia-cycle)
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :hook
  (after-init-hook . marginalia-mode)

  :config

  (defun gr/marginalia--annotate-local-file (cand)
    "Annotate local file CAND.
Removes modes, which I’ve never needed or wanted."
    (marginalia--in-minibuffer
      (when-let (attrs (ignore-errors
                         ;; may throw permission denied errors
                         (file-attributes (substitute-in-file-name
                                           (marginalia--full-candidate cand))
                                          'integer)))
        (marginalia--fields
         ((marginalia--file-size attrs) :face 'marginalia-size :width -7)
         ((marginalia--time (file-attribute-modification-time attrs))
          :face 'marginalia-date :width -12)
         ;; File owner at the right
         ((marginalia--file-owner attrs) :face 'marginalia-file-owner)))))

  (defun gr/marginalia-annotate-file (cand)
    "Annotate file CAND with its size, modification time and other attributes.
These annotations are skipped for remote paths."
    (if-let (remote (or (marginalia--remote-file-p cand)
                        (when-let (win (active-minibuffer-window))
                          (with-current-buffer (window-buffer win)
                            (marginalia--remote-file-p (minibuffer-contents-no-properties))))))
        (marginalia--fields (remote :format "*%s*" :face 'marginalia-documentation))
      (gr/marginalia--annotate-local-file cand)))

  (add-to-list 'marginalia-annotator-registry
               '(file gr/marginalia-annotate-file marginalia-annotate-file builtin none))

  )



;;;; orderless

(use-package orderless
  :init
  (setq orderless-matching-styles '(orderless-prefixes
                                    orderless-regexp)
        completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles . (basic partial-completion orderless)))))
  (setq orderless-component-separator "[ +]")
  )

;; (use-package orderless-kwd
;;   :ensure nil
;;   :config
;;   (add-to-list 'orderless-style-dispatchers #'orderless-kwd-dispatch))

;;;; cape

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  :bind (("M-i" . completion-at-point)
         ("C-c p p" . completion-at-point) ;; capf
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p h" . cape-history)
         ("C-c p i" . cape-dict)
         ("C-c p :" . cape-emoji)))

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(setq tab-always-indent 'complete)


;;;; tempel

(use-package tempel
  ;;Require trigger prefix before template name when completing.
  :custom
  (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
         (:map tempel-map
               ("<tab>" . tempel-next)
               ("<backtab>" . tempel-previous)
               ("C-]" . tempel-next)))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))
  :hook
  (prog-mode-hook . tempel-setup-capf)
  (text-mode-hook . tempel-setup-capf)
  )

;;; Citation / Bibliography

;;;; citar

(defvar gr/bibliography '("~/Dropbox/gr-bibliography.bib"))

(use-package citar
  :after (citar-org oc devonthink-dir)
  :functions list-dirs-recursively
  :bind*  ("C-\"" . gr/citar-insert-citation)
  :bind
  (:map citar-map
        ("i" . citar-insert-citation)
        ("k" . citar-copy-reference)
        ("z" . zk-search)
        ("s" . ex/search-pdf-contents))
  (:map citar-citation-map
        ("z" . zk-search)
        ("k" . citar-copy-reference)
        ("s" . ex/search-pdf-contents))
  :init
  (setq citar-citeproc-csl-style
        "chicago-fullnote-bibliography-short-title-subsequent.csl")
  :custom
  (citar-bibliography gr/bibliography)
  (citar-notes-paths '("~/Dropbox/ZK/Zettels"))
  (citar-additional-fields '("doi" "url"))

  (citar-library-file-extensions '("pdf" "epub"))
  (citar-library-paths-recursive t)
  (citar-library-paths `(,devonthink-dir
                         "~/Dropbox/Dickinson Primary/"))

  (citar-file-note-extensions '("org" "md"))
  (citar-file-open-functions '(("html" . citar-file-open-external)
                               ("pdf" . citar-file-open-external)
                               ("epub" . citar-file-open-external)
                               (t . find-file)))
  (citar-file-additional-files-separator " ")

  (citar-open-entry-function 'ebib-open)
  (citar-open-prompt nil)
  (citar-format-reference-function 'citar-citeproc-format-reference)
  (citar-display-transform-functions nil)
  (citar-select-multiple t)
  (citar-open-resources '(:files :notes :create-notes))

  :config

  ;; overrides
  ;; allows for finding files with citekeys anywhere in the file name

  (define-advice citar-file--make-filename-regexp
      (:override (keys extensions &optional additional-sep) gr/citar-file--make-filename-regexp)
    "Regexp matching file names starting with KEYS and ending with EXTENSIONS.
When ADDITIONAL-SEP is non-nil, it should be a regular expression
that separates the key from optional additional text that follows
it in matched file names.  The returned regexp captures the key
as group 1, the extension as group 2, and any additional text
following the key as group 3."
    (when (and (null keys) (string-empty-p additional-sep))
      (setq additional-sep nil))
    (concat
     "\\`"
     (if keys (regexp-opt keys ".*\\(?1:") ".*?\\(?1:[a-z]+[0-9]\\{4\\}[a-z]?\\)")
     (when additional-sep (concat "\\(?3:" additional-sep "[^z-a]*\\)?"))
     "\\."
     (if extensions (regexp-opt extensions "\\(?2:") "\\(?2:[^.]*\\)")
     "\\'"))

  (use-package citar-citeproc
    :ensure nil
    :after (citar)
    :custom
    (citar-citeproc-csl-styles-dir "~/.csl")
    (citar-citeproc-csl-locales-dir "~/.csl/locales")
    (citar-citeproc-csl-style
     "chicago-fullnote-bibliography-short-title-subsequent.csl"))

  (use-package citar-embark
    :after (citar)
    :diminish
    :config
    (citar-embark-mode))

  (use-package citar-org
    :ensure nil
    :after (citar)
    :bind
    (:map citar-org-citation-map
          ("<mouse-1>" . nil)
          ("<mouse-3>" . nil)))
  )

;;;; org-cite

(use-package oc
  :ensure nil
  :defer 1
  :init
  (setq org-cite-csl-styles-dir "~/.csl"
        org-cite-csl-locales-dir "~/.csl/locales"
        org-odt-preferred-output-format "docx"
        org-odt-styles-file "~/Dropbox/Academic/template.ott"
        org-cite-global-bibliography gr/bibliography
        org-cite-csl-link-cites nil)
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor nil
        org-cite-activate-processor nil
        org-cite-export-processors '((t csl "~/.csl/chicago-fullnote-bibliography-short-title-subsequent.csl"))))

(use-package oc-csl
  :ensure nil
  :defer t)

;;;; citeproc / parsebib

(use-package citeproc :defer t)

(use-package parsebib :defer t)

;;;; ebib

(use-package ebib
  :bind
  (:map ebib-index-mode-map
        ("C-h" . (lambda ()
                   (interactive)
                   (embark-bindings-in-keymap
                    ebib-index-mode-map)))
        ("?" . hydra-ebib/body)
        ("h" . hydra-ebib/body)
        ("F" . ebib-import-file)
        ("k" . nil)
        ("D" . ebib-delete-entry)
        ("d" . nil)
        ("c" . ebib-filters-cancel-filter)
        ("z" . nil)
        ("s" . ebib-filter-any)
        ("O" . ebib-filters-apply-filter)
        ("s-s" . ebib-save-curent-database))
  (:map ebib-entry-mode-map
        ("C-h" . (lambda ()
                   (interactive)
                   (embark-bindings-in-keymap
                    ebib-entry-mode-map)))
        ("?" . hydra-ebib/body)
        ("d" . nil)
        ("F" . ebib-import-file)
        ("j" . ebib-jump-to-entry)
        ("e" . ebib-edit-current-field)
        ("O" . ebib-filters-apply-filter)
        ("s-s" . ebib-save-curent-database)
        ("q" . ebib-quit-entry-buffer)
        ("k" . ebib-copy-current-field-contents))
  :hook
  (ebib-entry-mode-hook . visual-line-mode)
  :custom
  (ebib-preload-bib-files gr/bibliography)
  (ebib-filters-default-file
   (concat user-emacs-directory "var/" "ebib-filters.el"))
  (ebib-autogenerate-keys t)
  (ebib-create-backups t)
  (ebib-extra-fields '((biblatex "crossref"  "xdata"
                                 "annotation" "abstract"
                                 "keywords" "file"
                                 "timestamp" "shorttitle")
                       (BibTeX "crossref" "annote"
                               "abstract" "keywords"
                               "file" "timestamp"
                               "url" "doi" "shorttitle")))
  (ebib-uniquify-keys nil)
  (ebib-index-default-sort '("timestamp" . descend))
  (ebib-use-timestamp t)
  (ebib-index-columns '(("Author/Editor" 40 t)
                        ("Entry Key" 15 t)
                        ;;("Year" 6 t)
                        ("Title" 50 t)))
  )


(use-package ebib-extras
  :ensure nil
  :commands (ebib-open ebib-isbn-web-search)
  :bind
  (:map ebib-index-mode-map
        ("o" . ebib-citar-open-resource)
        ("q" . ebib-smart-quit))
  (:map ebib-entry-mode-map
        ("o" . ebib-citar-open-resource))
  :config
  (with-eval-after-load 'hydra
    (defhydra hydra-ebib (:hint nil :color blue)
      "
  _j_: Jump to Entry   _k_: Add Keyword    _!_: Auto-Citekey     _s_: DOI Lookup
  _O_: Apply Filter    _F_: Import File    _E_: Edit Citekey     _S_: ISBN Lookup
  _C_: Cancel Filter   _D_: Delete Field   _X_: Delete Entry     _I_: Auto Import
  "
      ("k" ebib-add-keywords-to-entry)
      ("!" ebib-generate-autokey)
      ("X" ebib-delete-entry)
      ("E" ebib-edit-keyname)
      ("F" ebib-import-file)
      ("D" ebib-delete-current-field-contents)
      ("j" ebib-jump-to-entry)
      ("O" ebib-filters-apply-filter)
      ("o" ebib-citar-open-resource)
      ("C" ebib-filters-cancel-filter)
      ;; ("s" ebib-save-current-database)
      ("I" ebib-zotero-import-identifier)
      ("S" ebib-isbn-web-search)
      ("s" crossref-lookup)
      ("q" nil))))

(use-package ebib-zotero
  :ensure nil
  :commands (ebib-auto-import ebib-import-pdf)
  :bind
  (:map ebib-index-mode-map
        ("I" . ebib-zotero-import-identifier)))

(use-package pdf-drop-mode
  :ensure nil
  :vc (:url "https://github.com/rougier/pdf-drop-mode")
  :defer 1
  :custom
  (pdf-drop-search-methods '(doi/metadata
                             doi/title
                             doi/user))
  :config
  (pdf-drop-mode)
  (setq pdf-drop-search-hook #'my/pdf-process))

(defun my/pdf-process (file doi)
  (ebib-zotero-import-identifier (cdr doi) file))

;;;; biblio / sci-hub

(use-package scihub
  :ensure nil
  :vc (:url "https://github.com/emacs-pe/scihub.el")
  :defer 1
  :custom
  (scihub-homepage "https://sci-hub.ren/")
  (scihub-download-directory (expand-file-name "~/DT3 Academic/")))

(use-package biblio
  :defer 1
  ;;:after ebib
  ;; :custom
  ;; (biblio-crossref-user-email-address vu-email)
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
      (message "Copied doi: \"%s\"" doi)))

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
  :ensure nil
  :after (ebib biblio)
  :bind (:map biblio-selection-mode-map
              ("e" . ebib-biblio-selection-import)))

;;;; mmd-citation-support

(use-package mmd-citation-support
  :ensure nil
  :defer 1
  :bind
  (:map embark-mmd-citation-map
        ("c" . gr/mmd-citation-convert))
  (:map citar-citation-map
        ("c" . gr/mmd-citation-convert))
  :hook
  (completion-at-point-functions . gr/mmd-citation-completion-at-point))

;;; Writing

;;;; zk

(use-package zk-setup
  :ensure nil
  :after hydra
  :demand t
  :bind
  ("C-'" . hydra-zk/body)
  ("C-z" . hydra-zk/body))

;;;; org-side-tree

(use-package org-side-tree
  :load-path "my-lisp/org-side-tree/"
  :ensure nil
  :defer 1
  ;; :hook
  ;; (org-side-tree-mode-hook . org-indent-mode)
  :bind
  (:map gr-map
        ("s" . org-side-tree))
  (:map org-side-tree-mode-map
        ("S-<right>" . org-side-tree-next-todo)
        ("S-<left>" . org-side-tree-previous-todo)
        ("S-<up>" . org-side-tree-priority-up)
        ("S-<down>" . org-side-tree-priority-down)
        ("C-<left>" . org-side-tree-promote)
        ("C-<right>" . org-side-tree-demote)
        ("C-S-<down>" . org-side-tree-move-subtree-down)
        ("C-S-<up>" . org-side-tree-move-subtree-up)
        ("C-S-<left>" . org-side-tree-promote-subtree)
        ("C-S-<right>" . org-side-tree-demote-subtree))
  :custom-face
  (org-side-tree-heading-face ((t (:inherit font-lock-builtin-face))))
  :custom
  (org-side-tree-cursor 'box)
  (org-side-tree-persistent t)
  (org-side-tree-fontify t)
  (org-side-tree-narrow-on-jump nil)
  (org-side-tree-timer-delay .3))

;; (add-hook 'window-buffer-change-functions 'org-side-tree)

;; (defun open-org-side-tree (_window)
;;   (interactive)
;;   (org-side-tree))

;;;; outline-minor-mode

(use-package outline
  :ensure nil
  :diminish outline-minor-mode
  :bind
  (:map outline-minor-mode-map
        ("C-S-<right>" . gr/outline-demote-subtree)
        ("C-S-<left>" . gr/outline-promote-subtree)
        ("C-S-<up>" . outline-move-subtree-up)
        ("C-S-<down>" . outline-move-subtree-down)
        ("C-<right>" . outline-demote)
        ("C-<left>" . outline-promote))
  (:map outline-minor-mode-cycle-map
        ("<backtab>" . outline-cycle-buffer)
        ("<left-margin> <mouse-1>" . nil)
        ("<left-margin> S-<mouse-1>" . nil)
        ("<right-margin> <mouse-1>" . nil)
        ("<right-margin> S-<mouse-1>" . nil))
  :hook
  (prog-mode-hook . outline-minor-mode)
  ;; (emacs-lisp-mode-hook . outline-minor-mode)
  ;; (emacs-lisp-mode-hook . (lambda () (setq-local outline-regexp ";;;\\(;* [^   \t\n]\\)")))
  ;; see:  https://github.com/clojure-emacs/clojure-mode/issues/550
  :custom
  (outline-blank-line t)
  (outline-minor-mode-highlight 'override)
  (outline-minor-mode-cycle t)

  :custom-face
  ;; now setting these in gr-light theme
  (outline-1 ((t (:foreground "dark blue" :weight bold))))
  ;; (outline-2 ((t (:foreground "black" :underline t))))
  ;;;; (outline-2 ((t (:underline t))))
  ;; (outline-3 ((t (:underline t))))
  ;; (outline-4 ((t (:underline t))))
  ;; (outline-5 ((t (:underline t))))

  :config

  (defun gr/outline-demote-subtree ()
    (interactive)
    (outline-demote 'subtree))

  (defun gr/outline-promote-subtree ()
    (interactive)
    (outline-promote 'subtree))

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              ;; prevent `outline-level' from being overwritten by `lispy'
              ;; (setq-local outline-level #'outline-level)
              ;; setup heading regexp specific to `emacs-lisp-mode'
              (setq-local outline-regexp ";;;\\(;* \\)")
              ;; heading alist allows for subtree-like folding
              (setq-local outline-heading-alist
                          '((";;; " . 1)
                            (";;;; " . 2)
                            (";;;;; " . 3)
                            (";;;;;; " . 4)
                            (";;;;;;; " . 5))))))

;;;; olivetti mode

(use-package olivetti
  :diminish
  :hook
  (text-mode-hook)
  (prog-mode-hook)
  (nov-mode-hook)
  :config
  (setq-default olivetti-body-width 0.75)
  (setq olivetti-minimum-body-width 72)
  (setq olivetti-recall-visual-line-mode-entry-state t)
  (set-fringe-mode 8))

;;;; sdcv-mode - stardict dictionary

(use-package sdcv-mode
  :ensure nil
  :vc (:url "https://github.com/gucong/emacs-sdcv")
  :defer 1
  :bind
  (:map gr-map
        ("d" . sdcv-search))
  :custom
  (sdcv-buffer-name "*Dictionary*"))

;; note: dictionaries are in ~/.stardic/dic

;;;; ispell / abbrev custom

(use-package abbrev
  :ensure nil
  :init
  (setq-default abbrev-mode t)
  :custom
  (save-abbrevs 'silently))

(use-package hippie-expand
  :ensure nil
  :bind
  ([remap dabbrev-expand] . hippie-expand)
  :custom
  (hippie-expand-verbose t))

(use-package ispell
  :defer t
  :bind
  (:map ctl-x-map
        ("C-i" . endless/ispell-word-then-abbrev))
  :init
  (setenv "DICTIONARY" "en_US")
  :commands endless/ispell-word-then-abbrev
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary "en_US")
  :config

  (define-advice ispell-command-loop
      (:override (miss guess word start end) gr/ispell-command-loop)
    (let ((vertico-sort-function nil)
          (ol (make-overlay start end)))
      (unwind-protect
          (progn
            (overlay-put ol 'face 'highlight)
            (push-mark (1- start))
            (completing-read (format "Replace \"%s\" with: " word) miss nil nil nil nil word))
        (delete-overlay ol))))

  ;; abbreviations and corrections stored in ~/.emacs.d/etc/abbrev.el

  (defun endless/simple-get-word ()
    (car-safe (save-excursion (ispell-get-word nil "[-'.@]"))))

  (defun endless/ispell-word-then-abbrev (p)
    "Call `ispell-word', then create an abbrev for it.
Finds first incorrect word before point, up to the beginning of
buffer. Adds replacement, from list or input, to global abbrev.
With prefix P, create local abbrev. Press `RET' with no input to
add the word to `ispell-personal-dictionary'. Abort with `C-g'."
    ;; FIX unfold org headings
    (interactive "P")
    (push-mark)
    (unwind-protect
        (let (bef aft)
          (while (if (setq bef (endless/simple-get-word))
                     ;; Word was corrected or used quit.
                     (if (ispell-word nil 'quiet)
                         nil ; End the loop.
                       ;; Also end if we reach `bob'.
                       (not (bobp)))
                   ;; If there's no word at point, keep looking
                   ;; until `bob'.
                   (not (bobp)))
            (unless (backward-word)
              (user-error "No typo at or before point"))
            (backward-char))
          (setq aft (endless/simple-get-word))
          (cond ((and aft bef (equal aft bef))
                 (progn
                   (ispell-send-string (concat "*" aft "\n"))
                   (setq ispell-pdict-modified-p '(t))
                   (ispell-pdict-save)))
                ((and aft bef (not (equal aft bef)))
                 (let ((aft (downcase aft))
                       (bef (downcase bef)))
                   (define-abbrev
                     (if p local-abbrev-table global-abbrev-table)
                     bef aft)
                   (message "\"%s\" now expands to \"%s\" %sally"
                            bef aft (if p "loc" "glob"))))))
      (forward-word -1)
      (forward-char -1)))
  )

;;;; org-reveal

(use-package ox-reveal
  :after org
  :custom
  (org-reveal-title-slide nil)
  (org-reveal-root "~/.reveal.js/")
  (org-reveal-single-file t)
  (org-reveal-hlevel 3))

;;;; LaTeX / AUCTeX

(use-package auctex
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-engine 'luatex)

  ;; for syncing auctex with pdf-tools
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-mode t))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("memoir" "\\documentclass[11pt]{memoir}"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;; remove ugly red boxes from footnote numbers
;; (setopt org-latex-hyperref-template "
;; \\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},
;;  pdfsubject={%d},\n pdfcreator={%c},\n pdflang={%L},\n colorlinks=true,\n linkcolor=black}\n")

(setopt org-latex-hyperref-template
        "\\hypersetup{
 hidelinks,
 pdfauthor={%a},
 pdftitle={%t},
 pdfkeywords={%k},
 pdfsubject={%d},
 pdfcreator={%c},
 pdflang={%L}}
")

;; pdflatex stopped working, for some reason (error "latexmk bad option")
(setq org-latex-compiler "xelatex")

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t)

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(setq org-preview-latex-default-process 'dvisvgm)

;;;; websites

(use-package ox-hugo
  :defer 3
  :after org)

(defun gr/blog-deploy-localauthor ()
  (interactive)
  (shell-command "cd ~/Dropbox/Sites/localauthor && ./deploy.sh"))

(defun gr/blog-test-localauthor ()
  (interactive)
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (if
        (equal 1 (shell-command "pgrep 'hugo -t hugo-la-rocinante'"))
        (start-process-shell-command "hugo server" "*hugo server*" "cd ~/Dropbox/Sites/localauthor && hugo server")
      nil)
    (browse-url "http://localhost:1313/")))

(defun gr/web-deploy ()
  (interactive)
  (shell-command "cd ~/Dropbox/Sites/gr-web && ./deploy.sh"))

(defun gr/web-test ()
  (interactive)
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (if
        (equal 1 (shell-command "pgrep 'hugo'"))
        (start-process-shell-command "hugo server" "*hugo server*" "cd ~/Dropbox/Sites/gr-web && hugo server")
      nil)
    (browse-url "http://localhost:1313/")))

;;;; org-wc

(use-package org-wc
  :after org
  :defer 1
  :bind
  (:map gr-map
        ("W" . org-wc-display)))

;;;; docsim

(use-package docsim
  ;; for finding similar notes, using docsim cli
  :ensure nil
  :vc (:url "https://github.com/hrs/docsim.el")
  :defer t
  :after zk
  :commands (docsim-search
             docsim-search-buffer
             zk-docsim)
  :custom
  (docsim-search-paths (list zk-directory))
  (docsim-get-title-function 'gr/docsim--get-title-function-zk)
  :config
  (defun gr/docsim--get-title-function-zk (path)
    "Return a title determined by parsing the file at PATH."
    (if (zk-file-p path)
        (zk--parse-file 'title path)
      path))

  (defun gr/docsim-search (query)
    "Search for notes similar to QUERY.

This calls out to the external `docsim' tool to perform textual
analysis on all the notes in `docsim-search-paths', score them by
similarity to QUERY, and return the sorted results, best first.

Include the similarity scores (between 0.0 and 1.0) of each note
if `docsim-show-scores' is non-nil.

Show at most `docsim-limit' results (or all of them, if
                                        `docsim-limit' is nil)."
    (interactive (list (docsim--read-search-term)))
    (let* ((results (docsim--query query))
           (files (mapcar #'car results)))
      (find-file
       (funcall zk-select-file-function
                "Similar Notes:"
                files))))

  (defun zk-docsim ()
    "Find notes similar to current buffer using docsim."
    (interactive)
    (gr/docsim-search (current-buffer)))
  )

;;; Packages

;;;; Calendar / Calfw

;;(package-vc-install '(calfw . :url "https://github.com/localauthor/emacs-calfw"))

(use-package calfw
  :ensure nil
  :vc (:url "https://github.com/localauthor/emacs-calfw")
  ;;:load-path "elpa/calfw"
  :defer 1
  :bind (:map calfw-calendar-mode-map
              ("S" . org-gcal-sync)
              ("RET" . calfw-show-details-command)
              ("<" . gr/calfw-prev)
              (">" . gr/calfw-next)
              ("g" . calfw-refresh-calendar-buffer)
              ("v" . calfw-cycle-view)
              ("V" . calfw-cycle-view-reverse))
  :custom
  (calfw-display-calendar-holidays nil))

(use-package calfw-org
  :ensure nil
  :load-path "elpa/calfw"
  :defer 1
  :bind
  (:map calfw-calendar-mode-map
        ("c" . gr/calfw-org-capture))
  (:map gr-map
        ("C" . gr/calfw-open-org-calendar))
  :init

  (defun gr/calfw-open-org-calendar (p)
    (interactive "P")
    (when p
      (select-frame (make-frame-command)))
    (save-excursion
      (let* ((source1 (calfw-org-create-source))
             (curr-keymap (if calfw-org-overwrite-default-keybinding calfw-org-custom-map calfw-org-schedule-map))
             (cp (calfw-create-calendar-component-buffer
                  :view 'month
                  :contents-sources (list source1)
                  :custom-map curr-keymap
                  :sorter 'calfw-org-schedule-sorter)))
        (unless p
          (tab-bar-new-tab))
        (switch-to-buffer (calfw-cp-get-buffer cp))
        (calfw-refresh-calendar-buffer nil))))

  (defun gr/calfw-org-capture ()
    (interactive)
    (setq calfw-org-capture-template
          '("x" "[calfw-auto]" entry (file "gcal-ruta.org")
            "* %?\n:org-gcal:\n%(calfw-org-capture-day)\n:END:\n" :empty-lines 1))
    (setq org-capture-templates
          (append org-capture-templates (list calfw-org-capture-template)))
    (calfw-org-capture))
  )

;; FIX "<" and ">" keybindings
;; currently "<" and ">" move by month
;; these functions will move according to current view

(defun gr/calfw-next ()
  (interactive)
  (let* ((cp (calfw-cp-get-component))
         (view (calfw-cp-get-view cp)))
    (pcase view
      ('day (call-interactively #'calfw-navi-next-day-command))
      ('week (call-interactively #'calfw-navi-next-week-command))
      ('two-weeks (call-interactively #'calfw-navi-next-week-command))
      ('month (call-interactively #'calfw-navi-next-month-command)))))

(defun gr/calfw-prev ()
  (interactive)
  (let* ((cp (calfw-cp-get-component))
         (view (calfw-cp-get-view cp)))
    (pcase view
      ('day (call-interactively #'calfw-navi-previous-day-command))
      ('week (call-interactively #'calfw-navi-previous-week-command))
      ('two-weeks (call-interactively #'calfw-navi-previous-week-command))
      ('month (call-interactively #'calfw-navi-previous-month-command)))))

;; There is a problem when multi-day events also have times, ie:
;; <2022-04-23 Sat 10:00>--<2022-04-24 Sun 08:00>
;; I think the issue is in the function calfw-org-get-timerange ?

(use-package org-gcal-setup
  :ensure nil
  :defer 1)

;;;; mu4e

(use-package mu4e-setup
  :ensure nil
  :defer 1
  :bind
  (:map gr-map
        ("m" . gr/mu4e-open-tab)))

;;;; magit

(use-package magit
  :bind
  ("C-c m" . magit-status)
  ("C-x m" . magit-status)
  :custom-face
  (diff-refine-added ((t (:background "yellow" :foreground "red"))))
  :custom
  (magit-diff-refine-hunk t))

;;;; esup

(use-package esup
  :defer t
  :custom
  (esup-user-init-file (concat user-emacs-directory "init.el"))
  :config
  (setq esup-depth 0))

;;;; elfeed

(use-package elfeed-setup
  :ensure nil
  :defer t
  :bind (:map gr-map
              ("e" . gr/elfeed-open))
  :commands gr/elfeed-open)

;;;; ibuffer

(use-package ibuffer
  :bind
  (:map ctl-x-map
        ("C-b" . ibuffer))
  (:map ibuffer-mode-map
        ("<backtab>". ibuffer-toggle-filter-group)
        ("TAB". ibuffer-toggle-filter-group))
  :hook
  (ibuffer-hook . gr/ibuffer-set-filter-group)
  :custom
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-auto-mode t)

  :config

  (defun gr/ibuffer-set-filter-group ()
    (ibuffer-switch-to-saved-filter-groups "default")
    (setq ibuffer-hidden-filter-groups (list "***" "helpful" "trees" "ORG" "ZK" "el" "Default"))
    (ibuffer-update nil t))

  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 18 -1 :left))))

  (setq ibuffer-saved-filter-groups
        '(("default"
           ;; ("Article" (or (and (directory . "/Academic/*")
           ;;                     (not (name . "magit")))))
           ("Teaching" (or (and (directory . "/Spring 2024/*")
                                (not (mode . special-mode))
                                (not (name . "magit")))))
           ("Writing" (or (and (directory . "/Writings/*")
                               (not (mode . special-mode))
                               (not (name . "magit")))))
           ("PR Work" (or (and (directory . "/PR Work/*")
                               (not (mode . special-mode))
                               (not (name . "magit")))))
           ("ZK" (or (name . "*ZK")
                     (and (directory . "/Zettels/")
                          (filename . "\\.org$")
                          (not (name . "^\\*scratch"))
                          (not (name . "magit")))))
           ("ORG" (and (filename . "\\.org$")
                       (not (name . "gcal"))))
           ("PDF" (or (mode . pdf-view-mode)
                      (mode . pdf-occur-buffer-mode)
                      (mode . pdf-annot-list-mode)
                      (name . "^\\*Contents")
                      (name . "^\\*Edit Annotation ")))
           ("magit" (and (name . "magit")
                         (not (mode . helpful-mode))))
           ("el" (and (mode . emacs-lisp-mode)
                      (not (name . "^\\*scratch"))
                      (not (name . "init.el"))))
           ("dired" (mode . dired-mode))
           ("eww" (mode . eww-mode))
           ("helpful" (mode . helpful-mode))
           ("***" (or (name . "^\\*scratch")
                      (mode . org-side-tree-mode)
                      (name . "init.el")
                      (name . "^\\*Messages")
                      (name . "^\\*mu4e-")
                      (name . "^\\*calfw-calendar")
                      (name . "*Calculator*")
                      (name . "org_archive")
                      (name . "*davmail-server*")
                      (name . "gcal")))
           )))
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

;; to allow --group-directories-first to work on osx
(setq insert-directory-program "/usr/local/bin/gls")

(use-package dired
  :ensure nil
  :bind
  ("C-x C-j" . dired-jump)
  ("C-x d" . dired-jump)
  (:map dired-mode-map
        ("K" . dired-kill-subdir)
        ("RET" . gr/dired-find-file-other-window)
        ("C-x C-q" . dired-toggle-read-only))
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  (dired-mode-hook . force-truncate-lines)
  :custom
  (dired-listing-switches "-algho --group-directories-first")
  (dired-hide-details-mode t)
  (dired-free-space nil)
  (dired-mouse-drag-files t)
  (dired-auto-revert-buffer t)
  ;;(dired-hide-details-preserved-columns '(3 4 5 7))
  (dired-clean-up-buffers-too t)
  (dired-create-destination-dirs 'ask)
  (dired-create-destination-dirs-on-trailing-dirsep t)
  (dired-guess-shell-alist-user
   '(("\\.pdf\\'" "open")
     ("\\.docx\\'" "open")))
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (add-to-list 'completion-ignored-extensions ".DS_Store")

  (defun gr/dired-find-file-other-window ()
    "In dired, open directories in same window, files in other window."
    (interactive)
    (let ((switch-to-buffer-obey-display-actions nil)
          (file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (dired--find-possibly-alternative-file file)
        (dired--find-file #'find-file-other-window file))))
  )

(use-package dired-x
  :ensure nil
  :hook
  (dired-mode-hook . dired-omit-mode)
  :config
  (setq dired-omit-files "\\.DS_Store\\|\\.dropbox\\|Icon\\\015")
  ;; show backup files
  (mapc
   (lambda (x) (delete x dired-omit-extensions))
   '("~")))

;;;; avy

(use-package avy
  :bind
  ("M-g w" . avy-goto-word-1)
  (:map gr-map
        ;; ("C-." . avy-goto-word-0)
        ("C-." . avy-goto-char-timer))
  (:map isearch-mode-map
        ("C-." . avy-isearch))
  :bind*
  ("C-. C-," . gr/avy-goto-string)
  ;;("C-'" . avy-goto-char-timer)
  :custom
  (avy-timeout-seconds 0.2)
  (avy-all-windows 'all-frames)
  (avy-keys '(?a ?d ?f ?l ?r ?u ?g ?e ?i ?c ?p ?d ?s))

  (avy-dispatch-alist '((?, . avy-action-embark)
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

  :config
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

  )

;; (defun gr/avy-goto-char-timer ()
;;   (interactive)
;;   (call-interactively #'avy-goto-char-timer)
;;   (forward-word)
;;   )

;;;; helpful

(use-package helpful
  :bind
  (:map help-map
        ("f" . helpful-function)
        ("v" . helpful-variable)
        ("h" . helpful-symbol)
        ("C-h" . helpful-symbol)
        ("k" . helpful-key)
        ("l" . find-library)
        ("?" . (lambda ()
                 (interactive)
                 (embark-bindings-in-keymap help-map))))
  (:map helpful-mode-map
        ("o" . link-hint-open-link))
  :custom
  (helpful-max-buffers 5)
  :hook
  (helpful-mode-hook . visual-line-mode)
  :config
  (with-eval-after-load 'semantic/symref/grep
    (add-to-list 'semantic-symref-filepattern-alist '(helpful-mode "*.el" "*.ede" ".emacs" "_emacs")))
  )

;;;; grep / wgrep

(setq grep-use-headings t)
(setq xref-search-program 'ugrep)

(use-package wgrep
  :defer t
  :commands wgrep
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t)
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)))

;;;; eww / web browsing

(use-package eww
  :bind
  (:map gr-map
        ("G" . eww-duckduckgo))
  (:map eww-mode-map
        (";" . consult-outline)
        ("j" . eww-switch-to-buffer)
        ("l" . eww-list-buffers)
        ("B" . eww-list-bookmarks)
        ("f" . eww-forward-url)
        ("b" . eww-back-url)
        ("DEL" . eww-back-url)
        ("o" . link-hint-open-link)
        ("M-n" . scroll-up-command)
        ("M-p" . scroll-down-command)
        ("n" . scroll-up-command)
        ("p" . scroll-down-command)
        )
  :custom
  (shr-inhibit-images nil)
  (eww-search-prefix "https://html.duckduckgo.com/html/?q=")
  (eww-download-directory (expand-file-name "~/Downloads")))

(use-package prot-eww
  :ensure nil
  :defer 1
  :config
  (setq prot-eww-save-history-file
        (concat user-emacs-directory "var/" "eww/prot-eww-visited-history"))
  (setq prot-eww-save-visited-history t)
  (setq prot-eww-bookmark-link nil)
  (define-prefix-command 'prot-eww-map)
  (setq shr-folding-mode t
        shr-use-colors t
        shr-bullet "• ")
  :hook
  (prot-eww-history-mode-hook . hl-line-mode)
  :bind
  ;; (:map gr-map
  ;;       ("w" . prot-eww-map))
  (:map prot-eww-map
        ("b" . prot-eww-visit-bookmark)
        ("e" . prot-eww-browse-dwim)
        ("g" . eww-duckduckgo)
        ("d" . eww-duckduckgo)
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

(defmacro eww-make-search (name prompt url)
  `(defun ,(intern (concat "eww-" (symbol-name name))) ()
     (interactive)
     (let* ((word (when (use-region-p)
                    (buffer-substring
                     (region-beginning)
                     (region-end))))
            (text (if word word
                    (read-string ,prompt nil nil word))))
       (eww (format ,url
                    (url-encode-url text))))))

(eww-make-search britannica "Britannica: " "https://www.britannica.com/search?query=%s")

(eww-make-search wiki "Wiki: " "https://en.m.wikipedia.org/wiki/Special:Search?search=%s")

(eww-make-search duckduckgo "DDG: " "https://duckduckgo.com/?q=%s")

(defun gr/switch-browser (choice)
  (interactive (list (completing-read "Choose: " '(safari eww) nil t)))
  (let ((completion-ignore-case  t))
    (setq browse-url-browser-function
          (pcase choice
            ("safari" 'browse-url-default-browser)
            ("eww" 'eww)))
    (message "browse-url set to `%s'" choice)))

(setq browse-url-generic-program "/usr/bin/open")
(setq browse-url-browser-function #'browse-url-default-browser)


;;;; pass

(use-package pass
  :defer t
  :after (embark consult)
  :bind
  (:map gr-map
        ("P" . password-store-copy))
  :custom
  (password-store-password-length 12)
  :init
  (setf epg-pinentry-mode 'loopback)

  (defvar-keymap embark-password-store-actions
    :doc "Keymap for actions for password-store."
    :parent embark-general-map
    "c" #'password-store-copy
    "f" #'password-store-copy-field
    "i" #'password-store-insert
    "I" #'password-store-generate
    "r" #'password-store-rename
    "e" #'password-store-edit
    "k" #'password-store-remove
    "U" #'password-store-url)

  (add-to-list 'embark-keymap-alist '(password-store . embark-password-store-actions))

  (add-to-list 'marginalia-prompt-categories '("Password entry" . password-store))
  )

;;;; ace-window

(use-package ace-window
  :bind
  ("C-x o" . ace-window)
  :custom-face
  (aw-leading-char-face ((t (:font "Menlo" :foreground "red" :height   2.5))))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?l))
  (aw-scope 'visible)
  (aw-dispatch-always t)
  (aw-ignore-current nil)
  (aw-ignore-on t)
  ;; doesn't work with yabai
  ;; (ace-window-posframe-mode -1)
  (aw-dispatch-alist
   '((?b aw-switch-buffer-in-window "Select Buffer in Target")
     (?w aw-swap-window "Swap Current and Target")
     (?m aw-copy-window "Move Current to Target")
     (?2 aw-split-window-vert "Split Vert Window")
     (?3 aw-split-window-horz "Split Horz Window")
     (?k aw-delete-window "Delete Window")
     (?0 aw-delete-window "Delete Window")
     (?? aw-show-dispatch-help)))

  :config
  (define-advice aw--switch-buffer
      (:override nil aw--consult-buffer)
    (cond ((bound-and-true-p ivy-mode)
           (ivy-switch-buffer))
          ((bound-and-true-p ido-mode)
           (ido-switch-buffer))
          (t
           (call-interactively #'consult-buffer))))
  )

;;;; popper

(use-package popper
  :bind (("C-\\"   . popper-toggle)
         ("M-\\"   . popper-cycle)
         ("C-M-\\" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*xref\\*"
          "\\*chatgpt\\*"
          "\\*gptel-quick\\*"
          "elfeed-entry"
          dired-mode
          magit-status-mode
          zk-index-mode
          org-side-tree-mode
          occur-mode
          grep-mode
          eshell-mode
          sdcv-mode
          org-agenda-mode
          compilation-mode))
  (setq popper-display-control nil)
  (setq popper-group-function #'popper-group-by-directory)
  :config
  (popper-mode 1)
  )

;;;; google-translate

(use-package google-translate
  :defer t
  :bind
  (:map gr-map
        ("t" . gr/translate))
  :custom
  (google-translate-default-source-language "lt")
  (google-translate-default-target-language "en")
  (google-translate-backend-method 'curl)
  (google-translate-translation-to-kill-ring t)
  (google-translate-pop-up-buffer-set-focus t)

  :config

  (with-eval-after-load 'google-translate-core-ui
    ;; override to prevent insertion of original text
    (define-advice google-translate--translating-text
        (:override (gtos format))
      " "))

  (defun gr/translate (p)
    (interactive "P")
    (if p
        (if (use-region-p)
            (let ((google-translate-output-destination 'paragraph-insert))
              (google-translate-at-point-reverse))
          (google-translate-query-translate-reverse))
      (if (use-region-p)
          (google-translate-at-point)
        (if (derived-mode-p 'mu4e-view-mode)
            (google-translate-buffer)
          (google-translate-query-translate)))))
  )

(use-package google-translate-smooth-ui
  :ensure nil
  :defer t
  :custom
  (google-translate-translation-directions-alist
   '(("lt" . "en")
     ("en" . "lt"))))


;;;; whitespace-mode

(use-package whitespace
  :defer 1
  :custom
  (whitespace-style '(face trailing lines)))

;;;; cyclekey

(use-package cyclekey
  :ensure nil
  :vc (:url "https://github.com/shankar2k/cyclekey")
  :defer t
  :bind
  (:map gr-map
        ("l" . cyclekey-cycle))
  (:repeat-map cyclekey-repeat-map
               ("l" . cyclekey-cycle))
  :custom
  (cyclekey-languages '("Currency" "Lithuanian" "French"))
  (cyclekey-marks-alist
   '(("Lithuanian" "aą" "cč" "eėę" "iį" "sš" "uūų" "zž" "AĄ" "CČ" "EĖĘ" "IĮ" "SŠ" "UŪŲ" "ZŽ" "\"„”" "'‚‘")
     ("French" "aàáâæ" "cç" "eéèêë" "iîï" "oôœ" "uùûü" "yÿ" "AÀÁÆ" "CÇ" "EÉÈÊË" "IÎÏ" "OÔŒ" "UÙÛÜ" "YŸ" "\"«»")
     ("Currency" "E€" "L£")))
  :config
  (cyclekey-init))

;;;; hide-cursor-mode

(defvar-local hide-cursor--original nil)

(define-minor-mode hide-cursor-mode
  "Hide or show the cursor.

When the cursor is hidden `scroll-lock-mode' is enabled, so that
the buffer works like a pager."
  :global nil
  :lighter " HideCursor"
  (if hide-cursor-mode
      (progn
        (scroll-lock-mode 1)
        (setq-local hide-cursor--original
                    cursor-type)
        (setq-local cursor-type nil))
    (scroll-lock-mode -1)
    (setq-local cursor-type (or hide-cursor--original
                                t))))

(keymap-global-set "<f7>" 'hide-cursor-mode)

;; (diminish 'scroll-lock-mode)

;;;; move-text

(use-package move-text
  :after org
  :bind*
  ("C-<up>" . gr/move-text-up)
  ("C-<down>" . gr/move-text-down)
  :config
  (defun gr/move-text-up ()
    (interactive)
    (cond ((org-at-heading-p)
           (org-move-subtree-up))
          ;; ((org-at-item-p)
          ;;  (org-move-item-up))
          (t (call-interactively #'move-text-up))))

  (defun gr/move-text-down ()
    (interactive)
    (cond ((org-at-heading-p)
           (org-move-subtree-down))
          ;; ((org-at-item-p)
          ;;  (org-move-item-down))
          (t (call-interactively #'move-text-down))))

  )


;;;; golden-ratio-scroll-screen

(use-package golden-ratio-scroll-screen
  :defer 1
  :custom-face
  (golden-ratio-scroll-highlight-line-face ((t (:background "darkseagreen2" :foreground "black" :weight normal :inherit highlight))))
  :custom
  (golden-ratio-scroll-screen-ratio 3)
  (golden-ratio-scroll-highlight-flag 'both)
  :config
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)

  )

;;;; osx-reveal-in-finder

(use-package reveal-in-osx-finder
  :defer 1
  :after embark
  :commands gr/embark-reveal-in-osx-finder
  :bind
  (:map embark-file-map
        ("O" . gr/embark-reveal-in-osx-finder))
  :config
  (defun gr/embark-reveal-in-osx-finder (file)
    "Embark action to reveal file or buffer in finder."
    (interactive "FFile: ")
    (let ((dir (or (expand-file-name (file-name-directory file))
                   default-directory))
          (filename (file-name-nondirectory file)))
      (reveal-in-osx-finder-as dir filename))))


;;;; gptel

(use-package gptel
  :ensure nil
  :vc (:url "https://github.com/karthink/gptel")
  :bind
  ("C-c C-<return>" . gptel-menu)
  ("C-c <return>" . gptel-send)
  ;;("C-h C-q" . gptel-quick)
  (:map gr-map
        ("g" . gptel))
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model "gpt-4o") ;; better and cheaper than 4
  :config
  (require 'gr-gptel-directives))

(use-package gptel-org
  :ensure nil
  :custom
  ;; use whole doc by default
  (gptel-org-branching-context nil)
  :config

  (progn (declare-function org-element-lineage-map "org-element-ast")
         (defalias 'gptel-org--element-lineage-map 'org-element-lineage-map))

  (defun gptel-org-toggle-branching-context ()
    "Toggle gptel context between doc and subheading."
    (interactive)
    (if gptel-org-branching-context
        (progn
          (setq-local gptel-org-branching-context nil)
          (message "Context: whole doc"))
      (setq-local gptel-org-branching-context t)
      (message "Context: subheading")))
  )

(use-package gptel-quick
  :ensure nil
  :vc (:url "https://github.com/karthink/gptel-quick")
  :bind
  ("C-h C-q" . gptel-ask)
  (:map embark-general-map
        ("C-h C-q" . gptel-quick))
  :config

  (setf (alist-get "^\\*gptel-ask\\*" display-buffer-alist
                   nil nil #'equal)
        `((display-buffer-in-side-window)
          (side . bottom)
          (window-height . ,#'fit-window-to-buffer)))

  (defvar gptel-ask--history nil)

  (defun gptel-ask ()
    (interactive)
    (if (use-region-p)
        (call-interactively #'gptel-quick)
      (let ((prompt (read-string "Ask: " nil 'gptel-ask--history)))
        (when (string= prompt "") (user-error "A prompt is required."))
        (gptel-request
            prompt
          :callback
          (lambda (response info)
            (if (not response)
                (message "gptel-ask failed with message: %s" (plist-get info :status))
              (with-current-buffer (get-buffer-create "*gptel-ask*")
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert response))
                (special-mode)
                (visual-line-mode 1)
                (pop-to-buffer (current-buffer)))))
          :system
          (alist-get 'default gptel-directives)))))
  )

;;;; consult-web

(use-package consult-web
  :load-path "elpa/consult-web"
  :ensure nil
  ;;:vc (:url "https://github.com/armindarvish/consult-web")
  :after consult
  :bind
  (:map gr-map
        ("w" . consult-web))

  :init
  (setq consult-web-sources-modules-to-load
        '(consult-web-google))

  (defun gr/api-key-from-auth-source (host)
    (plist-get
     (car (auth-source-search
           :host host))
     :secret))

  :custom
  (consult-web-default-interactive-command #'consult-web-dynamic)

  (consult-web-alternate-browse-function #'xwidget-webkit-browse-url)

  (consult-web-preview-key "C-?")
  (consult-web-default-preview-function #'xwidget-webkit-browse-url)

  (consult-web-dynamic-input-debounce 0.8)
  (consult-web-dynamic-input-throttle 1.6)
  (consult-web-dynamic-refresh-delay 0.8)

  :config
  ;; (add-to-list 'consult-web-multi-sources "Brave")
  ;; (add-to-list 'consult-web-dynamic-sources "Brave")
  (add-to-list 'consult-web-multi-sources "Google")
  (add-to-list 'consult-web-dynamic-sources "Google")

  (define-key global-map (kbd "C-?") nil)

  (consult-customize
   consult-web
   consult-web-dynamic
   consult-web-multi
   :preview-key "C-?"))

(use-package consult-web-sources
  :ensure nil
  :after consult-web
  :load-path "elpa/consult-web/sources"
  :custom
  ;; (consult-web-brave-api-key
  ;;  (gr/api-key-from-auth-source "api.search.brave.com"))

  (consult-web-google-customsearch-key
   (gr/api-key-from-auth-source "api.google.com"))
  (consult-web-google-customsearch-cx
   (gr/api-key-from-auth-source "cx.google.com"))
  )

(use-package consult-web-embark
  :ensure nil
  :after consult-web)

;;;; casuals

(use-package casual-dired
  :after dired
  :bind
  (:map dired-mode-map
        ("?" . casual-dired-tmenu)))

(use-package casual-info
  :after info
  :bind
  (:map Info-mode-map
        ("?" . casual-info-tmenu)))

(use-package casual-ibuffer
  :after ibuffer
  :bind
  (:map ibuffer-mode-map
        ("?" . casual-ibuffer-tmenu)))

;;;; tab-sets

(use-package tab-sets
  :demand t
  :ensure nil
  :load-path "my-lisp/tab-sets/"
  :bind
  (:map gr-map
        ("r" . tab-sets-open)
        ("R" . tab-sets-save))
  :custom
  (tab-sets-data-file "~/.emacs.d/var/tab-sets.eld")
  (tab-sets-bookmark-store t)
  (tab-sets-bookmark-prefix "")
  :config
  (tab-sets-setup-embark)
  (tab-sets-reconcile-bookmarks)

  (with-eval-after-load 'consult
    (add-to-list
     'consult-bookmark-narrow
     '(?t "Tab-Set" tab-sets-bookmark-handler)))
  )

;;;; activities

(use-package activities)

;;; Dev

;;;; emacs-benchmark

(use-package elisp-benchmarks :defer t)

;;;; melpazoid

(use-package melpazoid
  :ensure nil
  :vc (:url "https://github.com/riscy/melpazoid"
            :lisp-dir "melpazoid")
  :bind
  (:map gr-map
        ("E" . gr/elisp-check-buffer))
  :config
  (defun gr/elisp-check-buffer ()
    (interactive)
    (require 'melpazoid)
    (unless (eq major-mode 'emacs-lisp-mode)
      (error "Not elisp mode."))
    (let ((melpa-buf (get-buffer "*melpazoid*"))
          (pl-buf (get-buffer "*Package-Lint*")))
      (if (ignore-errors (or melpa-buf
                             pl-buf
                             ;; flymake-mode
                             flycheck-mode))
          (progn
            (flycheck-mode -1)
            ;;(flymake-mode -1)
            (when melpa-buf
              (kill-buffer melpa-buf))
            (when pl-buf
              (kill-buffer pl-buf))
            (message "Elisp checks off"))
        (progn
          (flycheck-mode)
          ;;(flycheck-list-errors)
          ;;(flymake-mode)
          ;;(flymake-show-buffer-diagnostics)
          ;;(package-lint-current-buffer) ;; melpazoid runs this anyway
          (melpazoid)))))
  )

;; (add-hook 'flymake-mode-hook
;;           (lambda () (setq elisp-flymake-byte-compile-load-path load-path)))

;;;; flycheck and package-lint

(use-package flycheck
  :defer 1
  :custom
  (flycheck-emacs-lisp-load-path 'inherit))

(use-package package-lint :defer t)

;;;; aggressive-indent

(use-package aggressive-indent
  :diminish
  :hook (prog-mode-hook))

;;;; gc-auto-commit

(use-package git-auto-commit-mode)

;;; variable reset

(setq debug-on-error nil)
(put 'list-timers 'disabled nil)
