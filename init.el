;;; init.el                    -*- lexical-binding: t; -*-

;;; Startups

(setq debug-on-error t)

(when (boundp 'igc-step-interval)
  (setq igc-step-interval 0.05))

(use-package no-littering
  :init
  (setq custom-file (concat user-emacs-directory "etc/" "custom.el")
        backup-directory-alist
        `(("." . ,(concat user-emacs-directory "var/"
                          "backups/per-save"))))
  :demand t)

(use-package recentf
  :custom
  (recentf-exclude '("~/Mail/"))
  :defer 2
  :init
  (setq recentf-show-messages nil)
  :config
  (dolist (dir '("etc/" "var/"))
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name
                  (concat user-emacs-directory dir))))
  (recentf-mode 1))

(use-package diminish)

(use-package gcmh
  :disabled
  :demand t
  :config
  (gcmh-mode 1)
  :diminish)

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections (%.4f)"
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done
           gc-elapsed))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;;;; safe-local-variable-values

(setq safe-local-variable-values
      '((dired-omit-size-limit)
        (gr/mmd-citation-use . t)
        (eval . (gr/toggle-capslock))
        (eval . (text-scale-adjust 10))))

;;; Basics

(use-package auth-source
  :config
  (setq epg-gpg-program "gpg2")
  ;; (add-to-list 'auth-sources 'macos-keychain-generic)
  ;; (add-to-list 'auth-sources 'macos-keychain-internet)
  ;; (auth-source-pass-enable)
  )

;; ensure server is running
(with-eval-after-load 'server
  (unless (or (daemonp)
              (server-running-p))
    (server-start)))

(use-package emacs
  :diminish
  eldoc-mode
  visual-line-mode
  abbrev-mode
  auto-fill-mode
  scroll-lock-mode
  :bind
  ("M-SPC" . (lambda () (interactive) (insert "; "))) ;; to account for quick typing when ; is set to Meta
  ("M-_" . (lambda () (interactive) (insert "—")))
  ("M-n" . scroll-up-command)
  ("M-p" . scroll-down-command)
  ("C-x k" . kill-buffer)
  ("C-x C-SPC" . pop-global-mark)
  ("s-[" . previous-buffer)
  ("s-]" . next-buffer)
  ("C-c e" . eval-buffer)
  ("M-o" . other-window)
  ("<f2>" . nil)
  ("M-s-}" . enlarge-window-horizontally)
  ("M-s-{" . shrink-window-horizontally)
  ("C-M-+" . global-text-scale-adjust)
  ("C-M-=" . global-text-scale-adjust)
  ("C-M--" . global-text-scale-adjust)
  ("C-M-0" . global-text-scale-adjust)
  ("C-d" . gr/delete-char)
  (:map ctl-x-map
        ("C-p" . nil) ;; unset mark-page
        ("p" . nil) ;; unset project-prefix-map
        ("v" . nil) ;; unset vc-prefix-map
        ("f" . nil) ;; unset set-fill-column
        ("[" . beginning-of-buffer)
        ("]" . end-of-buffer)
        ("e" . eval-last-sexp)
        ("E" .  kmacro-end-and-call-macro))
  (:map help-mode-map
        ("o" . link-hint-open-link))
  ;; rest of config in early-init.el
  :hook
  (prog-mode-hook . (lambda () (setq show-trailing-whitespace t)))
  (prog-mode-hook . visual-line-mode)
  (text-mode-hook . visual-line-mode)
  (text-mode-hook . (lambda () (modify-syntax-entry ?’ "w")))
  (text-mode-hook . (lambda () (show-paren-local-mode -1)))

  :custom
  (echo-keystrokes 0.3)
  (ad-redefinition-action 'accept)
  (initial-buffer-choice #'gr-initial-buffer)
  ;; (initial-buffer-choice "~/Dropbox/org/dailynotes.org")
  (create-lockfiles nil)
  (initial-major-mode 'text-mode)
  (initial-scratch-message nil)
  (set-mark-command-repeat-pop t)
  (use-dialog-box nil)
  (confirm-kill-emacs 'y-or-n-p)
  (minibuffer-follows-selected-frame nil)
  (sentence-end-double-space nil)
  (find-library-include-other-files nil)
  (vc-follow-symlinks t)
  (save-interprogram-paste-before-kill t)

  :config
  ;; ;; font I want for every theme
  (custom-set-faces
   '(default ((t (:height 130 :family "DejaVu Sans Mono")))))

  (minibuffer-nonselected-mode -1)

  (defun gr-initial-buffer ()
    (if (fboundp 'zk-daily-note)
        (find-file-noselect (zk-daily-note))
      (find-file-noselect "~/Documents/org/dailynotes.org")))

  (defun gr/delete-char ()
    (interactive)
    (cond ((region-active-p)
           (delete-active-region))
          ((and (not (bolp))
                (looking-at "$"))
           (call-interactively #'delete-char)
           (just-one-space 1))
          (t
           (call-interactively #'delete-char))))

  (setq user-emacs-directory "~/.emacs.d/"
        ns-use-proxy-icon nil)

  (defvar gr/default-fonts '(("Menlo" . 130)
                             ("Little Character" . 135)
                             ("DejaVu Sans Mono" . 130)
                             ("Aporetic Sans Mono" . 140)
                             ("Go Mono" . 130)
                             ("JetBrains Mono" . 130)
                             ("Fira Mono" . 130)
                             ("Consolas" . 140)
                             ("Roboto Mono" . 130)
                             ("Inconsolata" . 140)
                             ("IBM Plex Mono" . 130)))

  (defun gr/set-default-font ()
    "Set default font to a face from `gr/default-fonts’."
    (interactive)
    (let* ((face (completing-read "Choose: "
                                  gr/default-fonts nil t))
           (height (assoc-default face gr/default-fonts)))
      (face-spec-set
       'default `((t (:height ,height :family ,face))))))

  (setq-default indent-tabs-mode nil ;; use spaces for tabs
                fill-column 77)

  ;; (defun gr/buffer-face-mode-text ()
  ;;   (interactive)
  ;;   (setq buffer-face-mode-face '( :height 130
  ;;                                  :family "JetBrains Mono"
  ;;                                  :foreground "black"))
  ;;   (buffer-face-mode)
  ;;   (diminish 'buffer-face-mode))

  ;; (add-hook 'text-mode-hook #'gr/buffer-face-mode-text)

  ;; narrow lighter

  (cl-loop
   for x in-ref mode-line-modes
   until
   (when (and (stringp x) (string= x "%n"))
     (setf x '(:eval
               (unless (and (eq (point-min) 1)
                            (eq (point-max) (1+ (buffer-size))))
                 (concat " "
                         (propertize
                          "Nrwd"
                          'face '( :foreground "black"
                                   :background "lightgreen")
                          'help-echo
                          "mouse-2: Remove narrowing from buffer"
                          'mouse-face 'mode-line-highlight
                          'local-map
                          (make-mode-line-mouse-map
                           'mouse-2 'mode-line-widen))))))))

  ;;(setq recenter-positions '(middle bottom top))

  ;; yes-or-no function

  (setq y-or-n-p-use-read-key t ;; needed for embark
        use-short-answers t)

  (define-advice y-or-n-p
      (:around (orig-func &rest args) y-or-n-p-with-return)
    "Allow RET as affirmative to y-or-n-p."
    (let ((query-replace-map (copy-keymap query-replace-map)))
      (keymap-set query-replace-map "RET" #'act)
      (keymap-set query-replace-map "<return>" #'act)
      (apply orig-func args)))

  ;; modes
  (auto-save-visited-mode 1)
  (global-auto-revert-mode 1)
  (delete-selection-mode 1)
  (winner-mode 1)
  (transient-mark-mode 1)
  (pixel-scroll-precision-mode 1)
  ;;(global-visual-line-mode 1))

  (desktop-save-mode -1)
  (global-hl-line-mode -1)
  (blink-cursor-mode -1)

  ;; for left and right fringe/margin
  (define-advice mwheel-scroll
      (:override (event &optional arg) pixel-scroll-precision))

  (set-fringe-mode 5)
  )

(use-package repeat
  :defer 1
  :custom
  (repeat-echo-function 'repeat-echo-mode-line)
  :config
  (repeat-mode 1)
  (put 'other-window 'repeat-map nil))

(ignore-errors
  (with-current-buffer "*Messages*"
    (visual-line-mode)))

(ignore-errors
  (with-current-buffer "*scratch*"
    (visual-line-mode)))


;;;; gr-functions and gr-map

(use-package gr-functions
  :ensure nil
  :bind*
  ("s-n" . gr/make-frame)
  ("s-w" . gr/delete-frame-or-tab)
  ;; ("M-c" . ct/capitalize-word-at-point)
  ("M-u" . ct/upcase-word-at-point)
  ("M-l" . ct/downcase-word-at-point)
  ("M-c" . title-case-word-or-region)
  ("C-M-;" . gr/comment-and-copy)
  ("C-o" . gr/insert-line)
  ("<f12>" . gr/toggle-capslock)
  :init
  (bind-keys
   :prefix-map gr-map
   :prefix "C-."
   ("/" . switch-to-minibuffer-window)
   ;; ("'" . scratch-buffer)  ;; scratch package
   ("C-/" . exit-minibuffer)
   ("C-n" . gr/daily-notes)
   ("n" . gr/daily-notes)
   ;;("N" . gr/insert-date)
   ("i" . gr/open-init-file)
   ("D" . gr/lookup-word-at-point)
   ("L" . toggle-truncate-lines)
   ("h" . gr/toggle-headphones)))

(defun gr/unset-header-line-format ()
  (interactive)
  (setq header-line-format nil))

(defun gr/open-with-app (file app)
  (shell-command
   (format "open -a '%s' '%s'" app
           (expand-file-name file))))

(defun gr/open-with-voice-dream (file)
  (interactive "fOpen with VDR: ")
  (gr/open-with-app file "Voice Dream Reader"))


(use-package text-to-speech
  :ensure nil
  :commands hydra-mac-speak/body)

(use-package dickinson
  :ensure nil
  :defer 1)

(use-package gr-database
  :ensure nil
  :defer 1)

(use-package pdf-pagelabels
  :ensure nil
  :defer 1)

;; (defvar my-m-x-log-file "~/mx.log")
;; (defadvice execute-extended-command
;;     (after log-execute-extended-command activate)
;;   (let ((logfile (find-file-noselect my-m-x-log-file)))
;;     (with-current-buffer logfile
;;       (goto-char (point-max))
;;       (insert (format "%s\n" this-command))
;;       (save-buffer))))

;;;; scratch

(use-package scratch
  :config
  (defun my/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly.
If region is active, add its contents to the new buffer."
    (unless (derived-mode-p
             'text-mode 'prog-mode 'conf-mode 'tex-mode)
      (condition-case nil
          (let ((pick
                 (read-multiple-choice
                  "Switch major mode?"
                  '((?o "org") (?m "markdown")
                    (?l "lisp-interaction") (?e "elisp")
                    (?  "Continue")))))
            (pcase (car pick)
              (?o (org-mode)) (?m (markdown-mode))
              (?l (lisp-interaction-mode)) (?e (emacs-lisp-mode)))
            (read-only-mode 0))
        (quit nil)))
    (let* ((mode major-mode))
      (rename-buffer (format "*Scratch for %s*" mode) t)))
  (setf (alist-get "\\*Scratch for" display-buffer-alist nil nil #'equal)
        '((display-buffer-same-window)))
  :hook (scratch-create-buffer-hook . my/scratch-buffer-setup)
  :bind
  ("C-c C-'" . scratch)
  (:map gr-map
        ("'" . scratch))
  )

;;;; transient / toggle

(use-package transient
  :defines toggle-modes
  :bind (:map gr-map
              ("C-l" . toggle-modes))
  :config
  (transient-bind-q-to-quit)
  (transient-define-prefix toggle-modes ()
    "Turn on and off various frequently used modes."

    [;:pad-keys t
     ["Appearance"
      ("t" "theme" gr/toggle-theme-light-dark)
      ("ls"
       (lambda () (concat "line spc"
                          (when line-spacing
                            (propertize
                             (format " %.2f" line-spacing)
                             'face 'font-lock-comment-face))))
       (lambda () (interactive)
         (setq line-spacing
               (read-number "Spacing: "))))
      ("vl" "visual lines" visual-line-mode)
      ("vt" "trunc lines" toggle-truncate-lines)
      ("vo" "olivetti"    olivetti-mode
       :if (lambda () (fboundp 'olivetti-mode)))]

     ["Org"
      :if-derived org-mode
      ("o\\" "Pretty" org-toggle-pretty-entities)
      ("o/" "Emphasis" (lambda () (interactive)
                         (if (bound-and-true-p org-appear-mode)
                             (progn (org-appear-mode -1)
                                    (setq-local org-hide-emphasis-markers nil))
                           (setq-local org-hide-emphasis-markers (not org-hide-emphasis-markers))
                           ;;(org-appear-mode 1)
                           )))
      ("oi" "Indent" org-indent-mode)
      ("on" "Numbers" org-num-mode)
      ("ow" "Word Count" org-wc-display)]

     ["Markdown"
      :if-derived markdown-mode
      ("o/" "Emphasis" markdown-toggle-markup-hiding)
      ("ou" "url" markdown-toggle-url-hiding)
      ("os" "src" markdown-toggle-fontify-code-blocks-natively)]

     ["Editing"
      ("b" "visual page breaks" toggle-visual-page-breaks-local)
      ("r" "read only" read-only-mode)
      ("n" "line numbers" display-line-numbers-mode)
      ("fc"
       (lambda ()
         (concat "fill column "
                 (propertize (format "[%d]" fill-column)
                             'face 'font-lock-comment-face)))
       set-fill-column)
      ("j" "jinx" jinx-mode :if (lambda () (and (derived-mode-p 'text-mode)
                                                (fboundp 'jinx-mode))))
      ;; ("TAB" "outline" outline-minor-mode
      ;;  :if (lambda () (not (derived-mode-p 'outline-mode))))
      ]

     ["Etc"
      ("hl" "line" hl-line-mode)
      ("hc" "cursor" hide-cursor-mode :if (lambda () (fboundp 'hide-cursor-mode)))
      ;; ("c" "completion" corfu-mode :if (lambda () (fboundp 'corfu-mode)))
      ;; ("a" "autocomp" (lambda () (interactive)
      ;;                   (setq-local corfu-auto (not corfu-auto))
      ;;                   (corfu-mode 0) (corfu-mode 1)
      ;;                   (message "corfu-auto is now %s" corfu-auto))
      ;; :transient t)
      ("dd" (lambda () (concat "debug " (propertize (format "%s" (if debug-on-error "[ON]" "[OFF]"))
                                                    'face 'font-lock-builtin-face)))
       toggle-debug-on-error)
      ("E" "melpazoid" gr/toggle-elisp-check-buffer
       :if (lambda () (derived-mode-p 'prog-mode)))]
     ]
    ))

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

;; truncate buffer name in mode-line to 60 characters
(setq-default mode-line-buffer-identification
              (append '(-60)
                      (propertized-buffer-identification "%b")))

(add-hook 'text-mode-hook '(lambda () (line-number-mode -1)))

(setq-default mode-line-format
              '(" "
                display-time-string ;; left align
                " "
                ;;mode-line-frame-identification
                mode-line-buffer-identification
                " "
                mode-line-position
                "  "
                mode-line-modes
                " "
                ;; vc-mode
                ;; "  "
                mode-line-format-right-align
                mode-line-misc-info
                " "
                ))

(use-package time
  :custom
  (display-time-24hr-format t)
  (display-time-day-and-date nil)
  (display-time-default-load-average nil)
  (display-time-format "[%H:%M]") ;; put time in brackets
  :init
  (display-time-mode 1)
  :config
  (delq 'display-time-string global-mode-string))

;;;; tab-bar

(use-package tab-bar
  :defer 1
  :bind
  ("s-{" . tab-bar-switch-to-prev-tab)
  ("s-}" . tab-bar-switch-to-next-tab)
  ("C-{" . tab-bar-switch-to-prev-tab)
  ("C-}" . tab-bar-switch-to-next-tab)
  ("s-T" . tab-bar-undo-close-tab)
  ("M-s-n" . tab-detach)
  ("C-x t g" . tab-group)
  :custom
  (tab-bar-show 1)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-auto-width nil)
  (tab-bar-tab-name-function 'tab-bar-tab-name-current)
  ;; tab-group config
  (tab-bar-tab-group-face-function 'tab-bar-tab-face-default)
  (tab-bar-format '(tab-bar-format-tabs-groups tab-bar-separator))
  (tab-bar-new-tab-group nil)
  :config

  (add-to-list 'tab-bar-tab-name-format-functions
               'tab-bar-tab-name-format-truncated))

(defun gr/reinstall-package (pkg)
  "Unload, deleted, then reinstall package PKG."
  (interactive (list (intern
                      (completing-read
                       "Reinstall package: "
                       (mapcar #'car package-alist)))))
  (unload-feature pkg)
  (package-reinstall pkg)
  (require pkg))

(use-package markdown-mode)

(use-package hydra :defer 1)

(use-package keycast)

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
  (setq mac-right-command-modifier 'control)
  ;; for the following to work, capslock has to be rebound to left control, using karabiner elements
  (setq mac-right-control-modifier 'meta))

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
(surround highlight "s-h" "\+") ;; see org-emphasis-alist
(surround parens "s-\(" "\(" "\)")
(surround brackets "s-\[" "\[" "\]")

(bind-keys*
 ("s-\/" . surround-italics)
 ("s-\*" . surround-bold)
 ("s-\_" . surround-underline))

;;;; display-buffer-alist

(setq switch-to-buffer-obey-display-actions t)
;; non-nil means that bookmark-jump-to-frame will not work on files/dirs defined below

(setq display-buffer-alist
      `(("*Org-Side-Tree*\\|^<tree>\\|\\*Embark Live"
         (display-buffer-in-side-window)
         (post-command-select-window t)
         (side . left))

        ((major-mode . dired-mode)
         (display-buffer-at-bottom)
         (post-command-select-window t)
         (window-height . 0.45))

        ;; ("*Org Agenda*"
        ;;  (display-buffer-at-bottom)
        ;;  (post-command-select-window t)
        ;;  (window-height . 0.7))

        ((major-mode . magit-status-mode)
         (display-buffer-at-bottom)
         (post-command-select-window t)
         (window-height . 0.6))

        ("CAPTURE-*.org"
         (display-buffer-at-bottom)
         (post-command-select-window t)
         (window-height . 0.4))

        ("\\*elfeed-entry\\|\\*info"
         (display-buffer-at-bottom)
         (window-height . 0.75))

        ("\\*mu4e-main\\*"
         (display-buffer-full-frame))

        ("*Async Shell Command*"
         (display-buffer-no-window))

        ("Org Links"
         (display-buffer-no-window)
         (allow-no-window . t))

        ("Google Translate"
         (display-buffer-at-bottom)
         (post-command-select-window t)
         (window-height . 0.35))

        (,(concat
           "\\*\\("
           (string-join
            '("Messages" "Occur" "Backups:"
              "helpful" "Pp Eval Output"
              "eshell" "Org Select"
              "annotations" "Embark Collect")
            "\\|") "\\)")
         (display-buffer-at-bottom)
         (post-command-select-window t)
         (window-height . 0.6))

        (,(concat
           "\\*\\("
           (string-join
            '("Org-Babel" "trace-output"
              "*Completions*" "Warnings" "Compile-Log"
              "[Hh]elp" "calfw-details")
            "\\|") "\\)")
         (display-buffer-at-bottom)
         (window-height . 0.3))
        ))

(with-eval-after-load 'zk
  (defun zk-index--wide-window-p (buf _act)
    (and (with-current-buffer buf
           (eq major-mode 'zk-index-mode))
         (> (frame-outer-width)
            (frame-outer-height))))

  (defun zk-buffer-p (buf _act)
    (with-current-buffer buf
      (zk-file-p (buffer-file-name))))

  (setq display-buffer-alist
        (append
         '((zk-index--wide-window-p
            (display-buffer-reuse-window
             display-buffer-in-direction)
            (post-command-select-window t)
            (dedicated . t)
            (direction . left)
            (window-width . 0.4))

           ((major-mode . zk-index-mode)
            (display-buffer-in-direction)
            (direction . bottom)
            (post-command-select-window t)
            (dedicated . t)
            (window-height . 0.45))

           (zk-buffer-p
            (display-buffer-reuse-mode-window
             display-buffer-same-window
             display-buffer-in-direction)
            (direction . right)))
         display-buffer-alist)))

;;;; timer

(use-package tmr
  :config
  (add-to-list 'display-buffer-alist
               '("\\\\*tmr-tabulated-view\\\\*"
                 (display-buffer-in-side-window)
                 (side . top)
                 (slot . 4)
                 (window-height . 0.1))))


;;; gr-pr

(use-package gr-pr
  :ensure nil
  :defer 1
  :bind
  ("C-x p n" . gr/pr-new-document)
  ("C-x p f" . gr/pr-find-document)
  ("C-x p t" . gr/pr-clock-report-today)
  ("C-x p w" . gr/pr-clock-report-week)
  ("C-x p r" . gr/pr-refile)
  ("C-x p i" . gr/pr-prepare-invoice)
  ("C-x p c" . gr/pr-consult-org-todo)
  ("C-x p C" . (lambda () (interactive)
                 (gr/pr-consult-org-todo 'all)))
  (:map org-mode-map
        ("C-c C-w" . gr/org-refile))

  :config

  (defun gr/org-refile ()
    (interactive)
    (if (and (member "pr" (org-get-tags))
             (not current-prefix-arg))
        (gr/pr-refile)
      (org-refile)))

  )

;;;; theme setup

(use-package gr-theme-setup
  :ensure nil
  :demand t
  :bind
  (:map gr-map
        ("C-t" . gr/toggle-theme-light-dark))
  ( :repeat-map gr-repeat-map
    ("C-t" . gr/toggle-theme-light-dark)))

(defvar big-head-orig-face nil)

(define-minor-mode big-head-mode
  "Minor mode for big outline headings."
  :init-value nil
  :global t
  :lighter nil
  (if big-head-mode
      (progn
        (setq big-head-orig-face (face-user-default-spec 'outline-1))
        (set-face-attribute 'outline-1 nil :box nil :background "gray80" :height         170))
    (custom-set-faces
     `(outline-1 ,big-head-orig-face))))

;;;; erc

;; (use-package erc
;;   :custom
;;   (erc-server "irc.libera.chat")
;;   (erc-nick "localauthor")
;;   (erc-prompt-for-password nil)
;;   (erc-kill-buffer-on-part t)
;;   (erc-auto-query 'bury)
;;   (erc-autojoin-channels-alist '((Libera.Chat "#emacs" "#org-mode" "#systemcrafters"))))

;;;; elec-pair

(use-package elec-pair
  :hook
  (text-mode-hook . (lambda ()
                      (add-to-list (make-local-variable
                                    'electric-pair-pairs)
                                   '(?' . ?'))))
  (text-mode-hook . (lambda ()
                      (add-to-list (make-local-variable
                                    'electric-pair-pairs)
                                   '(?+ . ?+))))
  :custom
  (electric-pair-inhibit-predicate  #'gr/elec-pair-inhibit)
  :init
  (electric-pair-mode)
  :config
  (defun gr/elec-pair-inhibit (c)
    (or
     (char-equal c ?\>)
     (char-equal c ?\<)
     (electric-pair-conservative-inhibit c)))
  )

(use-package electric
  :custom
  (electric-quote-context-sensitive t)
  (electric-quote-replace-double t)
  :init
  (electric-quote-mode))

;;;; info

(use-package info
  :bind
  (:map Info-mode-map
        ("o" . link-hint-open-link)))

;;;; expand-region

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;;; bookmark

(use-package bookmark
  :init
  (setq bookmark-bmenu-toggle-filenames nil
        bookmark-save-flag 1
        bookmark-fringe-mark nil)
  :config

  (define-advice bookmark-default-handler
      (:around (orig-fun bmk-record)
               gr/bookmark-find-from-dir-or-default)
    "Around advice for bookmark-default-handler.
  Calls through unless bookmark is a directory, in which case, calls find-file."
    (let ((file (bookmark-get-filename bmk-record)))
      (if (file-directory-p file)
          (let ((default-directory file))
            (call-interactively 'find-file))
        (funcall orig-fun bmk-record))))

  (defun gr/bookmark-url-jump (bookmark)
    (interactive)
    (browse-url (car bookmark)))

  (defun gr/bookmark-set-url (url)
    (interactive "sBookmark URL: ")
    (if (assoc url bookmark-alist)
        (user-error "%s is already bookmarked" url)
      (push `(,url . ((handler . gr/bookmark-url-jump)))
            bookmark-alist)))
  )

;;;; register

(use-package consult-register
  :ensure nil
  :bind
  ("M-#" . consult-register-load)
  ;; ("M-'" . consult-register-store)
  ("C-M-#" . consult-register)
  :config
  (setq register-preview-function #'consult-register-format)
  (define-advice register-preview
      (:override (buffer &optional show-empty) consult-register-window)))

;;;; isearch

(use-package isearch
  :ensure nil
  ;; :custom-face
  ;; (lazy-highlight ((t (:background "turquoise2"))))
  :custom
  (search-default-mode 'char-fold-to-regexp) ;; ignore diacritics
  (isearch-lazy-count t)
  ;; (isearch-allow-scroll 'unlimited)
  (isearch-repeat-on-direction-change t)
  :config
  (defun isearch-exit-at-start ()
    "Exit search at beginning of the current match."
    (unless (or isearch-mode-end-hook-quit
                (bound-and-true-p isearch-suspended)
                (not isearch-forward)
                (not isearch-other-end)
                (and (boundp 'avy-command)
                     (eq avy-command 'avy-isearch)))
      (goto-char isearch-other-end)))
  (defun isearch-exit-at-end ()
    "Exit search at the end of the current match."
    (interactive)
    (let ((isearch-other-end (point)))
      (isearch-exit))
    (unless isearch-forward (goto-char isearch-other-end)))
  :hook
  (isearch-mode-end-hook . isearch-exit-at-start)
  :bind
  (:map isearch-mode-map
        ("C-n" . isearch-repeat-forward)
        ("C-p" . isearch-repeat-backward)
        ("C-<return>" . isearch-exit-at-end)))

;;;; re-builder

(use-package re-builder
  :init
  (setq reb-re-syntax 'string))

;;;; init-lock

(use-package init-lock
  :ensure nil
  :custom
  (init-lock-files '("~/.emacs.d/init.el"))
  :commands (init-lock))

;;;; link-hint

(use-package link-hint
  :custom
  (link-hint-message nil)
  :defer 1)

(use-package link-hint-aw-select
  :ensure nil
  :bind
  ("C-M-o" . link-hint-aw-select)
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
  :ensure nil
  :bind
  (:map gr-map
        ("p" . link-hint-preview))
  :hook
  (link-hint-preview-mode-hook . link-hint-preview-toggle-frame-mode-line)
  )

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
  :custom

  (org-directory "~/Documents/org")
  (org-ellipsis " ▼") ;◣ ▼ ▽ ► ➽
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-startup-truncated nil)
  (org-tags-column 0)
  (org-fast-tag-selection-single-key 'expert)
  (org-use-fast-todo-selection 'expert)
  (org-log-done 'time)
  (org-fontify-done-headline nil)
  (org-log-states-order-reversed nil)
  (org-hide-emphasis-markers t)
  (org-emphasis-alist
   '(("*" bold)
     ("/" italic)
     ("_" underline)
     ("=" org-verbatim verbatim)
     ("+" highlight)
     ("~" verbatim)))
  (org-archive-location "%s_archive::datetree/")
  (org-footnote-section nil)
  (org-generic-id-locations-file (concat user-emacs-directory "var/"  "org/.org-generic-id-locations"))

  (org-clock-mode-line-total 'today)

  ;; org-export
  (org-export-allow-bind-keywords t)
  (org-export-with-smart-quotes t)
  (org-export-with-toc nil)
  (org-export-with-section-numbers nil)
  (org-export-with-tags nil)
  (org-export-with-date nil)
  (org-export-with-timestamps nil)
  (org-export-timestamp-file nil)
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

  ;; modules

  (org-modules nil)

  ;; ol
  (org-highlight-links '(bracket angle plain radio tag footnote))
  (org-link-keep-stored-after-insertion nil)
  (org-link-search-must-match-exact-headline t)

  ;; faces
  (org-fontify-whole-heading-line t)

  ;; org-num
  (org-num-skip-tags '("nonum"))
  (org-num-skip-commented t)
  (org-num-skip-footnotes t)

  ;; org-refile
  ;; (org-refile-targets '((nil . (:level . 1))))
  ;; (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)

  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)

  (org-M-RET-may-split-line '((default . nil)))
  ;; note: org-meta-return is remapped to C-<return>
  ;; and M-<return> is directly mapped to respect-content

  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  ("C-c C-l" . org-insert-link-global)
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
            ("C" . "center")))

    (defmacro gr/org-heading-function (name)
      `(defun ,(intern (concat "gr/org-" name "-heading")) ()
         (interactive)
         (let (org-side-tree-narrow-on-jump)
           (if (org-buffer-narrowed-p)
               (progn
                 (setq org-side-tree-narrow-on-jump t)
                 (,(intern (concat "org-side-tree-" name "-heading"))))
             (org-speed-move-safe ',(intern (concat "org-" name "-visible-heading")))
             (when (fboundp 'org-side-tree-update)
               (org-side-tree-update))))))
    (gr/org-heading-function "next")
    (gr/org-heading-function "previous")
    )

  (with-eval-after-load 'org-indent
    (diminish 'org-indent-mode))
  (with-eval-after-load 'org-num
    (diminish 'org-num-mode))

  ;; :hook
  ;; (org-mode-hook . (lambda ()
  ;;                    (set-face-extend 'org-level-1 t)))

  :config
  (unbind-key "C-," org-mode-map)
  (unbind-key "C-'" org-mode-map)
  (add-to-list 'org-file-apps '("\\.docx\\'" . default) 'append)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)))

  (defun org-babel-execute:yaml (body params) body)

  (defun gr/org-table-cell-to-register (register)
    "Store org-table-cell at point as a string in REGISTER.
Interactively, prompt for REGISTER with
`register-read-with-preview'."
    (interactive `(,(register-read-with-preview "Cell to register: ")))
    (set-register register `,(string-trim (org-table-get-field))))

  (keymap-set org-mode-map "C-c w" #'gr/org-table-cell-to-register)

  )



(use-package gr-org-extras
  :ensure nil
  :after org
  :bind
  (:map org-mode-map
        ("C-c n" . narrow-or-widen-dwim)
        ("C-c $" . gr/org-mark-done-and-archive)
        ("RET" . gr/org-return))
  :demand t)

(use-package gr-org-agenda-setup
  :ensure nil
  :after org
  :bind
  (:map gr-map
        ("C-a" . gr/org-agenda)
        ("a" . gr/org-agenda))
  :defer 1)

(use-package gr-org-capture-setup
  :ensure nil
  :after org
  :bind
  (:map gr-map
        ("C-c" . org-capture))
  :defer 1)


;;;; org-superstar

(use-package org-superstar
  :after org
  :hook (org-mode-hook)
  :custom
  ;; fixes org-hide on theme-change
  ;; (org-superstar-leading-bullet ?\s)
  (org-superstar-headline-bullets-list  '("•" "◦" "‣" "⊜" "≚"))
  ;;  "◉" "○" "▪" "➤"
  (org-superstar-item-bullet-alist
   '((?+ . ?◦)
     (?* . ?‣)
     (?- . ?–))))

;;;; org-appear

(use-package org-appear
  :after org
  :custom
  (org-appear-trigger 'on-change)
  (org-appear-delay 0.2)
  (org-appear-autolinks t)
  :hook
  (org-mode-hook)
  (org-appear-mode-hook . (lambda ()
                            (remove-hook 'mouse-leave-buffer-hook #'org-appear--after-change t)))
  :defer 1)


;;; Completion

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
  (setopt orderless-affix-dispatch-alist
          `((?% . ,#'char-fold-to-regexp)
            (?! . ,#'orderless-not)
            (?& . ,#'orderless-annotation)
            ;; (?, . ,#'orderless-initialism) ;; luhmann separator
            (?= . ,#'orderless-literal)
            (?^ . ,#'orderless-literal-prefix)
            (?~ . ,#'orderless-flex)))

  (setopt orderless-style-dispatchers
          '(orderless-affix-dispatch my-orderless-accent-dispatch))

  (defvar my-orderless-accent-replacements
    '(("a" . "[aàáâãäąå]")
      ("e" . "[eèéêėęë]")
      ("i" . "[iìíîïį]")
      ("o" . "[oòóôõöœ]")
      ("u" . "[uùúûüūų]")
      ("c" . "[cçč]")
      ("s" . "[sš]")
      ("z" . "[zž]")
      ("n" . "[nñ]")))

  (defun my-orderless-accent-dispatch (pattern &rest _)
    (seq-reduce
     (lambda (prev val)
       (replace-regexp-in-string (car val) (cdr val) prev))
     my-orderless-accent-replacements
     pattern))
  :demand t)

;; (use-package orderless-kwd
;;   :ensure nil
;;   :config
;;   (add-to-list 'orderless-style-dispatchers #'orderless-kwd-dispatch))

;;;; vertico

(use-package vertico
  :bind* (:map vertico-map
               ("C-x C-j" . consult-dir-jump-file)
               ("C-j" . vertico-exit-input)
               ("C-g" . keyboard-escape-quit)
               ("M-[" . vertico-previous-group)
               ("M-]" . vertico-next-group))
  :hook
  (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)

  :custom
  (vertico-cycle t)
  (vertico-count 12)
  (vertico-sort-function #'vertico-sort-history-alpha)

  :demand t

  :config
  (vertico-mode 1)
  (vertico-multiform-mode)

  (setq vertico-multiform-commands
        `((consult-imenu buffer)
          (consult-buffer buffer)
          ;; (consult-outline buffer
          ;;                  (lambda (_)
          ;;                    (text-scale-set -1)))
          (consult-dir buffer
                       (vertico-sort-function . gr/sort-modified))
          (execute-extended-command
           (:keymap "X" execute-extended-command-cycle))
          (gr/database-find-file
           (vertico-sort-override-function . gr/sort-modified)
           (vertico-count . 20))
          (gr/database-ripgrep-all
           buffer
           (vertico-buffer-display-action . (display-buffer-same-window)))
          (zk-consult-grep
           buffer
           (vertico-buffer-display-action . (display-buffer-in-side-window
                                             (window-height . 0.5)
                                             (side . bottom)))
           )
          ))

  (setq vertico-multiform-categories
        '(;; (file
          ;;  (vertico-count . 12))
          (zk-file
           (vertico-sort-function . gr/sort-modified))
          ;; (org-heading buffer
          ;;              (lambda (_)
          ;;                (text-scale-set -1)))
          ;; (consult-location buffer
          ;;                   (lambda (_)
          ;;                     (text-scale-set -1)))
          (embark-keybinding grid)
          (bookmark buffer)
          (consult-grep buffer)))

  ;; required for file selection from transient
  (setq transient-show-during-minibuffer-read 'fixed)

  (defun sort-directories-first-alpha (files)
    (setq files (vertico-sort-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  (setq vertico-buffer-display-action '(display-buffer-in-side-window
                                        (window-height . 0.3)
                                        (side . bottom)))
  ;; (setq crm-separator ",")
  ;; (setq crm-separator "[ 	]*;[ 	]*")

  ;;; vertico sort modified

  (defun gr/sort-modified (list)
    "Sort LIST of files for latest modified."
    (let ((ht (make-hash-table :test #'equal :size (length list))))
      (dolist (file list)
        (puthash file (file-attribute-modification-time
                       (file-attributes file))
                 ht))
      (sort list
            (lambda (a b)
              (time-less-p
               (gethash b ht)
               (gethash a ht))))))

  (defun vertico-sort-modified ()
    (interactive)
    (setq-local vertico-sort-override-function
                (and (not vertico-sort-override-function)
                     #'gr/sort-modified)
                vertico--input t))

  (keymap-set vertico-map "M-M" #'vertico-sort-modified)

  )

(setopt crm-prompt "[CRM%s] %p")

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

(minibuffer-depth-indicate-mode 1)

;;;; completion-preview-mode

;; ;; Enable Completion Preview mode in code buffers
;; (add-hook 'prog-mode-hook #'completion-preview-mode)

;; ;; ;; also in text buffers
;; ;; (add-hook 'text-mode-hook #'completion-preview-mode)

;; ;; and in \\[shell] and friends
;; (with-eval-after-load 'comint
;;   (add-hook 'comint-mode-hook #'completion-preview-mode))

;; (with-eval-after-load 'completion-preview
;;   ;; Show the preview already after two symbol characters
;;   (setq completion-preview-minimum-symbol-length 2)

;;   ;;   ;; Non-standard commands that should show the preview:

;;   ;;   ;; Org mode has a custom `self-insert-command'
;;   ;;   (push 'org-self-insert-command completion-preview-commands)
;;   ;;   ;; Paredit has a custom `delete-backward-char' command
;;   ;;   (push 'paredit-backward-delete completion-preview-commands)

;;   ;;   ;; Bindings that take effect when the preview is shown:

;;   ;; Cycle the completion candidate that the preview shows
;;   (keymap-set completion-preview-active-mode-map "M-n"
;;               #'completion-preview-next-candidate)
;;   (keymap-set completion-preview-active-mode-map "M-p"
;;               #'completion-preview-prev-candidate)
;;   ;; Convenient alternative to C-i after typing one of the above
;;   (keymap-set completion-preview-active-mode-map "M-i"
;;               #'completion-preview-insert))


;;;; embark

(use-package embark
  :bind
  ("C-," . embark-act)
  ("C->" . embark-act-noquit)
  ("C-<" . embark-act-all)
  ("M-," . embark-dwim)
  ("C-h b" . embark-bindings)
  (:map embark-identifier-map
        ("c" . title-case-word-or-region))
  (:map embark-region-map
        ("c" . title-case-word-or-region))
  (:map embark-general-map
        ("," . embark-select))
  (:map embark-symbol-map
        ("G d" . gr/lookup-word-at-point)
        ("c" . capitalize-region))
  (:map embark-file-map
        ("t" . find-file-other-tab)
        ("n" . find-file-other-frame)
        ("k" . embark-copy-as-kill)
        ("K" . gr/copy-file-as-org-link)
        ("l" . gr/insert-file-as-org-link)
        ("L" . gr/insert-file-as-org-link-full)
        ("M" . mail-add-attachment))
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

  :defer 1

  :config

  (defun embark-act-noquit ()
    "Run action but don't quit the minibuffer afterwards."
    (interactive)
    (embark-act t))

  (defun embark--simplify-path (_type target)
    "Simplify and '//' or '~/' in the TARGET file path."
    ;; FIX for: https://github.com/oantolin/embark/issues/704
    (cons 'file (abbreviate-file-name
                 (expand-file-name
                  (substitute-in-file-name target)))))

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
    (keymap-set embark-file-map "o" (embark-aw-select find-file))
    (keymap-set embark-buffer-map "o" (embark-aw-select switch-to-buffer))
    (keymap-set embark-bookmark-map "o" (embark-aw-select bookmark-jump)))

  )

(use-package embark-org
  :ensure nil
  :after (embark org)
  :bind
  (:map embark-org-heading-map
        ("$" . gr/org-mark-done-and-archive-datetree))
  (:map embark-org-link-map
        ("l" . org-insert-link)
        ("x" . embark-open-externally)
        ("M" . gr/org-link-mail-attach-file)
        ("t" . find-file-other-tab))
  (:map embark-file-map
        ("K" . gr/copy-file-as-org-link)
        ("l" . gr/insert-file-as-org-link)
        ("L" . gr/insert-file-as-org-link-full))
  :config

  (defun gr/org-link-mail-attach-file (file)
    (interactive "fAttach file: ")
    (when (f-file-p file)
      (mail-add-attachment file)))

  (defun gr/format-file-as-org-link (filenames &optional full)
    "Format FILENAMES as org-links with optional short filename description."
    (let (links)
      (dolist (file (ensure-list filenames))
        (let ((description
               (unless (or full
                           (file-directory-p file))
                 (concat "][" (file-name-nondirectory file)))))
          (push (concat "[[" file
                        description
                        "]]")
                links)))
      (string-join links "\n\n")))

  (defun gr/insert-file-as-org-link-full (filenames &optional wildcard)
    "Insert FILENAMES as org-links with full filename description."
    (interactive
     (find-file-read-args "Find file: "
                          (confirm-nonexistent-file-or-buffer)))
    (insert (gr/format-file-as-org-link filenames t)))

  (defun gr/insert-file-as-org-link (filenames &optional wildcard)
    "Insert FILENAMES as org-links with optional short filename description."
    (interactive
     (find-file-read-args "Find file: "
                          (confirm-nonexistent-file-or-buffer)))
    (insert (gr/format-file-as-org-link filenames)))

  (defun gr/copy-file-as-org-link (filenames &optional wildcard)
    "Copy FILENAMES as org-links with optional short filename description."
    (interactive
     (find-file-read-args "Find file: "
                          (confirm-nonexistent-file-or-buffer)))
    (kill-new (gr/format-file-as-org-link filenames)))

  (add-to-list 'embark-multitarget-actions #'gr/insert-file-as-org-link)
  (add-to-list 'embark-multitarget-actions #'gr/copy-file-as-org-link)
  (add-to-list 'embark-multitarget-actions #'gr/insert-file-as-org-link-full)
  )

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode)
  :demand t) ; only necessary if you have the hook

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
  ("M-g k" . gr/consult-global-mark)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ;; M-s bindings in `search-map'
  ("M-s d" . consult-find)
  ("M-s D" . consult-locate)
  ("M-s f" . consult-find)
  ("M-s s" . consult-locate)
  ("M-s g" . consult-ripgrep)
  ("M-s G" . consult-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-goto-line)
  ("M-s L" . consult-line-multi)
  ("M-s m" . multi-occur)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  (:map consult-narrow-map
        ("M-?" . consult-narrow-help))
  (:map gr-map
        ;; ("b" . consult-bookmark)
        ("C-b" . consult-buffer)
        ("b" . consult-buffer))
  :bind*
  ;; ("C-c C-SPC" . gr/consult-global-mark)
  ;; ("C-x C-SPC" . gr/consult-global-mark)
  ("C-:" . consult-imenu)
  ("C-;" . gr/consult-outline-dwim)
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode)

  :custom
  (completion-in-region-function 'consult-completion-in-region)
  (consult-fontify-preserve nil)
  (consult-project-function nil)
  (consult-async-split-style 'semicolon)
  (consult-preview-key "M-\\")
  (consult-locate-args "mdfind -name")

  :config
  ;; consult-preview settings

  (setq xref-show-definitions-function #'consult-xref
        xref-show-xrefs-function #'consult-xref)

  (consult-customize
   consult-theme
   consult-git-grep consult-grep consult-mark consult-line
   consult-xref consult-ripgrep consult-global-mark
   consult-goto-line gr/consult-global-mark
   :preview-key 'any)

  ;;make C-s and C-r search forward and backward in consult-line
  ;;changed to make C-s call previous search term
  ;; (defvar my-consult-line-map
  ;;   (let ((map (make-sparse-keymap)))
  ;;     (keymap-set map "C-s" #'previous-history-element)
  ;;     ;;(keymap-set map "C-s" #'next-line)
  ;;     ;;(keymap-set map "C-r" #'previous-line)
  ;;     map))

  ;; (consult-customize consult-line :keymap my-consult-line-map)

  (defun gr/consult-outline-dwim ()
    "When org-mode, consult-org-heading, else consult-outline"
    (interactive)
    (if (derived-mode-p 'org-mode)
        (consult-org-heading)
      (consult-outline)))

  (defun gr/consult-global-mark ()
    (interactive)
    (let ((switch-to-buffer-obey-display-actions nil))
      (consult-global-mark)))

  (defun gr/consult-ripgrep-select-dir ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'consult-ripgrep)))

  (defun gr/consult-find-select-dir ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'consult-find)))

  (with-eval-after-load 'org
    (defvar org-source
      (list :name     "Org"
            :category 'buffer
            :narrow   ?o
            :face     'consult-buffer
            :history  'buffer-name-history
            :state    #'consult--buffer-state
            :items
            (lambda ()
              (consult--buffer-query :mode 'org-mode
                                     :as #'consult--buffer-pair
                                     :predicate
                                     (lambda (buf)
                                       (unless (or (zk-file-p (buffer-file-name buf))
                                                   (file-in-directory-p (or (buffer-file-name buf) "") pr-clients-dir)
                                                   (member (buffer-file-name buf)
                                                           (mapcar (lambda (file)
                                                                     (expand-file-name file org-directory))
                                                                   org-agenda-files)))
                                         buf))
                                     ))))

    (add-to-list 'consult-buffer-sources 'org-source))

  (with-eval-after-load 'zk-consult
    (setq consult-source-buffer
          `( :name     "Buffer"
             :narrow   ?b
             :category buffer
             :face     consult-buffer
             :history  buffer-name-history
             :state    ,#'consult--buffer-state
             :default  t
             :items
             ,(lambda () (consult--buffer-query :sort 'visibility
                                                :as #'consult--buffer-pair
                                                :predicate
                                                (lambda (buf)
                                                  (unless (zk-file-p (buffer-file-name buf))
                                                    buf)))))))

  ;; hides some sources
  (dolist (src '(consult-source-hidden-buffer
                 consult-source-modified-buffer
                 consult-source-other-buffer
                 consult-source-recent-file
                 consult-source-buffer-register
                 consult-source-file-register
                 consult-source-bookmark))
    (set src (plist-put (symbol-value src) :hidden t)))

  ;; remove project sources
  (dolist (src '(consult-source-project-buffer-hidden
                 consult-source-project-recent-file-hidden
                 consult-source-project-root-hidden))
    (delq src consult-buffer-sources))


  )

;;;; consult-dir

(use-package consult-dir
  :bind ("C-x C-d" . consult-dir)
  :custom
  (consult-dir-sources '(consult-dir--source-writings
                         consult-dir--source-classes
                         consult-dir--source-bookmark
                         consult-dir--source-recentf))
  :config

  (defvar gr-writing-project-dirs '("~/Documents/Academic Work/Articles/Crusoe's Shelf/"
                                    "~/Documents/Academic Work/Articles/Bewilderment/"))

  (defvar gr-current-term "Autumn 2026")

  (defvar consult-dir--source-classes
    `( :name "Classes"
       :narrow ?c
       :category file
       :face consult-file
       :items ,(lambda ()
                 (delq nil (mapcar
                            (lambda (x)
                              (when (file-directory-p x)
                                (cons (concat gr-current-term ": "
                                              (file-name-nondirectory x))
                                      (concat x "/"))))
                            (directory-files
                             (concat "~/ownCloud/" gr-current-term "/")
                             t "[^.DS_eort]")))))
    "Class directory source for `consult-dir--pick'.")

  (defvar consult-dir--source-writings
    `( :name "Projects"
       :narrow ?p
       :category file
       :face consult-file
       :items ,(lambda ()
                 (mapcar
                  (lambda (x)
                    (cons (file-name-nondirectory (directory-file-name x))
                          x))
                  gr-writing-project-dirs)))
    "Project directory source for `consult-dir--pick'.")
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

(add-hook 'dired-after-readin-hook #'recentd-track-opened-file)
(add-hook 'kill-buffer-hook #'recentd-track-closed-file)

;;;; marginalia

(use-package marginalia
  :bind
  ("M-A" . marginalia-cycle)
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :hook
  (after-init-hook . marginalia-mode)
  :commands marginalia-mode
  :demand t
  :custom
  (marginalia-align 'left)
  :config

  (defun gr/marginalia--annotate-local-file (cand)
    "Annotate local file CAND."
    (marginalia--in-minibuffer
      (when-let* ((attrs (ignore-errors
                           (file-attributes (substitute-in-file-name
                                             (marginalia--full-candidate cand))
                                            'integer))))
        (if (eq marginalia-align 'right)
            (marginalia--fields
             ;; File owner at the left
             ((marginalia--file-size attrs) :face 'marginalia-size :width -7)
             ((marginalia--time (file-attribute-modification-time attrs))
              :face 'marginalia-date :width -12))
          (marginalia--fields
           ((marginalia--file-size attrs) :face 'marginalia-size :width -7)
           ((marginalia--time (file-attribute-modification-time attrs))
            :face 'marginalia-date :width -12)
           ;; File owner at the right
           ((marginalia--file-owner attrs) :face 'marginalia-file-owner))))))

  (defun gr/marginalia-annotate-file (cand)
    "Annotate file CAND with its size, modification time and other attributes.
  These annotations are skipped for remote paths."
    (if-let* ((remote (or (marginalia--remote-file-p cand)
                          (when-let* ((win (active-minibuffer-window)))
                            (with-current-buffer (window-buffer win)
                              (marginalia--remote-file-p (minibuffer-contents-no-properties)))))))
        (marginalia--fields (remote :format "*%s*" :face 'marginalia-documentation))
      (gr/marginalia--annotate-local-file cand)))

  (add-to-list 'marginalia-annotators
               '(file none gr/marginalia-annotate-file marginalia-annotate-file builtin))

  ;; set certain completion category annotations to ‘none’
  ;; (in this case, just buffer)
  (dolist (cat (list 'buffer))
    (let ((ann (assq cat marginalia-annotators)))
      (setcdr ann (list 'none 'builtin (intern (format "marginalia-annotate-%s" cat))))))
  )


;;;; cape

(use-package cape
  :bind (("M-i" . completion-at-point)
         ("C-c p p" . completion-at-point) ;; capf
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p h" . cape-history)
         ("C-c p i" . cape-dict)
         ("C-c p :" . cape-emoji))
  ;; :custom
  ;; (cape-auto-trigger "<")
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(setq tab-always-indent 'complete)


;;;; tempel

(use-package tempel
  ;;Require trigger prefix before template name when completing.

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
         (:map tempel-map
               ("<tab>" . tempel-next)
               ("<backtab>" . tempel-previous)
               ("C-]" . tempel-next)))
  :hook
  (prog-mode-hook . tempel-setup-capf)
  (text-mode-hook . tempel-setup-capf)

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches.  We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  )


;;; Citation / Bibliography

;;;; mmd-citation-support

(use-package mmd-citation-support
  :after citar
  :ensure nil
  :bind*
  ("M-'" . gr/citar-insert-citation)
  ("C-'" . gr/citar-insert-citation)
  ("C-M-'" . gr/citar-insert-previous-citation)
  ("C-\"" . gr/citar-insert-previous-citation)
  :bind
  (:map embark-mmd-citation-map
        ("r" . mmd-citation-details)
        ("c" . gr/mmd-citation-convert))
  (:map citar-map
        ("r" . mmd-citation-details))
  (:map citar-citation-map
        ("r" . mmd-citation-details)
        ("c" . gr/mmd-citation-convert))
  :hook
  (completion-at-point-functions . gr/mmd-citation-completion-at-point)
  :defer 2
  )

;;;; citar

(defvar gr/bibliography '("~/Documents/Academic Work/gr-bibliography.bib"))

(use-package citar
  :after (oc gr-database)
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
  :custom
  (citar-notes-paths '("~/Documents/ZK"))
  (citar-additional-fields '("doi" "url"))

  (citar-library-file-extensions '("pdf" "epub"))
  (citar-library-paths-recursive t)
  (citar-library-paths '("~/Documents/Books and Readings/"
                         "~/Documents/Scholarship and Theory/"
                         "~/Documents/Vilnius University Docs/"
                         "~/Documents/Academic Work/"
                         "~/Calibre Library/"))

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

  :commands gr/window-popup-citar-open-files

  :init
  (setq citar-citeproc-csl-style
        "chicago-fullnote-bibliography-short-title-subsequent.csl")

  (setopt citar-bibliography gr/bibliography)

  :config
  (setq citar-indicators
        (list citar-indicator-files
              citar-indicator-notes
              citar-indicator-cited))

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
  :defer 2)

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
  :init
  (citar-embark-mode)
  :diminish)

(use-package citar-org
  :ensure nil
  :after (citar)
  :bind
  (:map citar-org-citation-map
        ("<mouse-1>" . nil)
        ("<mouse-3>" . nil)))

;;;; org-cite

(use-package oc
  :ensure nil
  :init
  (setq org-cite-csl-styles-dir "~/.csl"
        org-cite-csl-locales-dir "~/.csl/locales"
        org-odt-preferred-output-format "docx"
        org-odt-styles-file "~/Documents/Academic Work/template.ott"
        org-cite-global-bibliography gr/bibliography
        org-cite-csl-link-cites nil)
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor nil
        org-cite-activate-processor nil
        org-cite-export-processors '((t csl "~/.csl/chicago-fullnote-bibliography-short-title-subsequent.csl")))
  :defer 1)

(use-package oc-csl
  :ensure nil)

;;;; citeproc / parsebib

(use-package citeproc)

(use-package parsebib)




;;;; ebib

(use-package ebib
  :bind
  (:map ebib-index-mode-map
        ;; ("C-h" . (lambda ()
        ;;            (interactive)
        ;;            (embark-bindings-in-keymap
        ;;             ebib-index-mode-map)))
        ("?" . hydra-ebib/body)
        ("h" . hydra-ebib/body)
        ("N" . ebib-add-entry)
        ("k" . ebib-copy-current-field-contents)
        ("D" . ebib-delete-entry)
        ("d" . nil)
        ("c" . ebib-filters-cancel-filter)
        ("z" . nil)
        ("s" . ebib-filter-any)
        ("O" . ebib-filters-apply-filter)
        ("s-s" . ebib-save-curent-database))
  (:map ebib-entry-mode-map
        ;; ("C-h" . (lambda ()
        ;;            (interactive)
        ;;            (embark-bindings-in-keymap
        ;;             ebib-entry-mode-map)))
        ("?" . hydra-ebib/body)
        ("d" . nil)
        ("k" . ebib-copy-current-field-contents)
        ("D" . ebib-delete-entry)
        ("j" . ebib-jump-to-entry)
        ("e" . ebib-edit-current-field)
        ("s" . ebib-filter-any)
        ("O" . ebib-filters-apply-filter)
        ("s-s" . ebib-save-curent-database)
        ("q" . ebib-quit-entry-buffer)
        )
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


(use-package gr-ebib-extras
  :ensure nil
  :bind
  (:map ebib-index-mode-map
        ("o" . ebib-citar-open-resource)
        ("q" . ebib-bury)
        ("Q" . ebib-smart-quit))
  (:map ebib-entry-mode-map
        ("o" . ebib-citar-open-resource))
  :commands (ebib-open ebib-isbn-web-search)
  :config
  (with-eval-after-load 'hydra
    (defhydra hydra-ebib (:hint nil :color blue)
      "
  _j_: Jump to Entry   _k_: Add Keyword    _!_: Auto-Citekey     _s_: DOI Lookup
  _O_: Apply Filter    _F_: Import PDF     _E_: Edit Citekey     _S_: ISBN Lookup
  _C_: Cancel Filter   _D_: Delete Field   _X_: Delete Entry     _I_: Auto Import
  "
      ("k" ebib-add-keywords-to-entry)
      ("!" ebib-generate-autokey)
      ("X" ebib-delete-entry)
      ("E" ebib-edit-keyname)
      ("F" ebib-import-pdf)
      ("D" ebib-delete-current-field-contents)
      ("j" ebib-jump-to-entry)
      ("O" ebib-filters-apply-filter)
      ("o" ebib-citar-open-resource)
      ("C" ebib-filters-cancel-filter)
      ;; ("s" ebib-save-current-database)
      ("I" ebib-import-from-doi-or-isbn)
      ("S" ebib-isbn-web-search)
      ("s" crossref-lookup)
      ("q" nil))))

(use-package ebib-zotero
  :ensure nil
  :after gr-ebib-extras
  :bind
  (:map ebib-entry-mode-map
        ("F" . ebib-import-pdf)
        ("I" . ebib-import-from-doi-or-isbn)
        ("K" . ebib-zotero-formatted-file-name))
  (:map ebib-index-mode-map
        ("F" . ebib-import-pdf)
        ("I" . ebib-import-from-doi-or-isbn)
        ("K" . ebib-zotero-formatted-file-name))
  :commands (ebib-import-from-doi-or-isbn ebib-zotero-import-pdf ebib-import-pdf))

;; (use-package pdf-drop-mode
;;   :ensure nil
;;   :vc (:url "https://github.com/rougier/pdf-drop-mode")
;;   :defer 1
;;   :custom
;;   (pdf-drop-search-methods '(doi/metadata
;;                              doi/title
;;                              doi/user))
;;   :config
;;   (pdf-drop-mode)
;;   (setq pdf-drop-search-hook #'my/pdf-process))

;; (defun my/pdf-process (file doi)
;;   (ebib-zotero-import-identifier (cdr doi) file))

;;;; biblio / sci-hub

(use-package scihub
  :ensure nil
  :vc (:url "https://github.com/emacs-pe/scihub.el")
  :custom
  (scihub-homepage "https://sci-hub.in/")
  (scihub-download-directory (expand-file-name "~/Documents/Inbox/")))

(use-package biblio
  ;;:after ebib
  ;; :custom
  ;; (biblio-crossref-user-email-address vu-email)
  :defer 1
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
      (ebib-import-from-doi-or-isbn doi)))

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

;;; Writing

;;;; zk

(use-package zk-setup
  :ensure nil
  ;; :after hydra
  :bind
  ;;("C-z" . hydra-zk/body)
  (:map gr-map
        ("C-k" . (lambda ()
                   (interactive)
                   (zk-find-file-by-id "201801190001")))
        ("." . zk-index-switch-to-index)
        ("C-z" . zk-index-switch-to-index)
        ("N" . zk-daily-note))
  :demand t)

;;;; org-side-tree

(use-package org-side-tree
  :ensure nil
  ;; :hook
  ;; (org-side-tree-mode-hook . org-indent-mode)
  :bind
  (:map gr-map
        ("s" . org-side-tree)
        ("C-s" . org-side-tree))
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

  ;; :custom-face
  ;; (org-side-tree-heading-face ((t (:inherit font-lock-builtin-face))))

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
                            (";;;;;;; " . 5)))))
  :diminish outline-minor-mode)


;;;; olivetti mode

(use-package olivetti
  :diminish
  :hook
  (text-mode-hook)
  (prog-mode-hook)
  (nov-mode-hook)
  (quick-sdcv-mode-hook)
  :config
  (setq-default olivetti-body-width .95)
  (setq olivetti-minimum-body-width 77)
  (setq olivetti-recall-visual-line-mode-entry-state t))

;;;; quick-sdcv-mode - stardict dictionary

;; note: dictionaries are in ~/.stardic/dic

(use-package quick-sdcv
  :bind
  ;; (:map gr-map
  ;;       ;; overwritten by zk-daily-note
  ;;       ("d" . quick-sdcv-search-input))
  (:map embark-identifier-map
        ("d" . quick-sdcv-search-input))
  (:map quick-sdcv-mode-map
        ("n" . outline-next-visible-heading)
        ("p" . outline-previous-visible-heading)
        ("q" . quit-window))
  :custom
  (quick-sdcv-dictionary-prefix-symbol "►")
  (quick-sdcv-ellipsis org-ellipsis)
  (quick-sdcv-buffer-name-prefix "*Dictionary")
  (quick-sdcv-buffer-name-suffix "*"))

;;;; ispell / abbrev custom

(use-package abbrev
  :ensure nil
  :custom
  (save-abbrevs 'silently)
  :init
  (setq-default abbrev-mode t))

(use-package hippie-expand
  :ensure nil
  :bind
  ([remap dabbrev-expand] . hippie-expand)
  :custom
  (hippie-expand-verbose t)
  :config
  (delete 'try-expand-line hippie-expand-try-functions-list))

(use-package ispell
  :bind
  (:map ctl-x-map
        ("i" . endless/ispell-word-then-abbrev)
        ("C-i" . endless/ispell-word-then-abbrev))
  (:map embark-symbol-map
        ("$" . ispell-region))
  (:map embark-identifier-map
        ("$" . ispell-region))
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary "en_US")
  (ispell-personal-dictionary "~/.hunspell_en_US")
  :commands endless/ispell-word-then-abbrev
  :init
  (setenv "DICTIONARY" "en_US")
  :config

  ;; (setq ispell-local-dictionary-alist
  ;;       '("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en_US") nil utf-8))

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
    (car-safe (save-excursion (ispell-get-word nil "[-’'.@]"))))

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

;;;; jinx

(use-package jinx)

;;;; org-reveal

(use-package ox-reveal
  :after org
  :custom
  (org-reveal-root "~/.reveal.js")
  (org-reveal-single-file t)
  (org-reveal-theme "moon")
  (org-reveal-hlevel 3))

;;;; LaTeX / AUCTeX

(use-package auctex
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

;; pdf-tools
;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;       TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;       TeX-source-correlate-start-server t)

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(setq org-preview-latex-default-process 'dvisvgm)

;;;; websites

(use-package ox-hugo
  :after org
  :autoload gr/blog-test-localauthor gr/blog-deploy-localauthor gr/web-deploy gr/web-test
  :init
  (defun gr/blog-deploy-localauthor ()
    "Deploy blog."
    (interactive)
    (shell-command "cd ~/Documents/Sites/localauthor && ./deploy.sh"))

  (defun gr/blog-test-localauthor ()
    "Open blog on localhost."
    (interactive)
    (let ((browse-url-browser-function 'browse-url-default-browser))
      (if
          (equal 1 (shell-command "pgrep 'hugo -t hugo-la-rocinante'"))
          (start-process-shell-command "hugo server" "*hugo server*" "cd ~/Documents/Sites/localauthor && hugo server --noHTTPCache --ignoreCache --disableFastRender")
        nil)
      (browse-url "http://localhost:1313/")))

  (defun gr/web-deploy ()
    "Deploy grantrosson.com."
    (interactive)
    (shell-command "cd ~/Documents/Sites/gr-web && ./deploy.sh"))

  (defun gr/web-test ()
    "Open grantrosson.com on localhost."
    (interactive)
    (let ((browse-url-browser-function 'browse-url-default-browser))
      (if
          (equal 1 (shell-command "pgrep 'hugo'"))
          (start-process-shell-command "hugo server" "*hugo server*" "cd ~/Documents/Sites/gr-web && hugo server --noHTTPCache --ignoreCache --disableFastRender")
        nil)
      (browse-url "http://localhost:1313/")))
  )

(use-package simple-httpd
  :defer 3)

;;;; org-wc

(use-package org-wc
  :after org
  :bind
  (:map gr-map
        ("w" . org-wc-display)))

;;;; visual-page-breaks

(use-package visual-page-breaks
  :ensure nil
  :diminish visual-page-breaks-mode
  :bind
  (:map gr-map
        ("W" . toggle-visual-page-breaks-local))
  :demand t
  :config
  ;; (visual-page-breaks-mode -1)
  (with-eval-after-load 'zk
    (setq visual-page-breaks-alist
          '(((zk-file-p)
             (lines . 30)
             "|---------------------- CARD %p ---------------[%w]-----|")))))

;;;; docsim

(use-package docsim
  ;; for finding similar notes, using docsim cli
  :ensure nil
  :vc (:url "https://github.com/hrs/docsim.el")
  :after zk
  :custom
  (docsim-search-paths (list zk-directory))
  (docsim-get-title-function 'gr/docsim--get-title-function-zk)
  :commands (docsim-search
             docsim-search-buffer
             zk-docsim)
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

(use-package calfw
  :disabled
  :ensure nil
  :vc (:url "https://github.com/localauthor/emacs-calfw")
  ;; :load-path "elpa/calfw/"
  :bind
  (:map calfw-calendar-mode-map
        ("S" . org-gcal-sync)
        ("RET" . calfw-show-details-command)
        ("<" . gr/calfw-prev)
        (">" . gr/calfw-next)
        ("g" . calfw-refresh-calendar-buffer)
        ("v" . calfw-cycle-view)
        ("V" . calfw-cycle-view-reverse))
  (:map gr-map
        ("C" . gr/calfw-open-org-calendar))
  :custom
  (calfw-display-calendar-holidays nil)

  ;; :custom-face
  ;; (calfw-face-toolbar-button-off ((t (:inherit font-lock-builtin-face))))

  :config
  (defun gr/calfw-open-org-calendar (p)
    (interactive "P")
    (require 'calfw-org)
    (when p
      (select-frame (make-frame-command)))
    (save-excursion
      (let* ((source1 (calfw-org-create-source))
             (curr-keymap (if calfw-org-overwrite-default-keybinding
                              calfw-org-custom-map
                            calfw-org-schedule-map))
             (cp (calfw-create-calendar-component-buffer
                  :view 'month
                  :contents-sources (list source1)
                  :custom-map curr-keymap
                  :sorter 'calfw-org-schedule-sorter)))
        (unless p
          (tab-bar-new-tab))
        (switch-to-buffer (calfw-cp-get-buffer cp))
        (calfw-refresh-calendar-buffer nil))))
  )

(use-package calfw-org
  :disabled
  :ensure nil
  :after calfw
  :load-path "elpa/calfw/"
  :bind
  (:map calfw-calendar-mode-map
        ("c" . gr/calfw-org-capture))
  :init

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

(use-package gr-org-gcal-setup
  :ensure nil
  :defer 1)

;;;; mu4e

(use-package gr-mu4e-setup
  :ensure nil
  :commands (mu-build-master)
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

;;;; elfeed

(use-package gr-elfeed-setup
  :ensure nil
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
        ("i". ibuffer-toggle-filter-group)
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
           ;; ("Article" (or (and (directory . "/Academic Work/*")
           ;;                     (not (name . "magit")))))
           ("Teaching" (or (and (directory . "/Spring 2026/*")
                                (not (mode . special-mode))
                                (not (name . "^\\*scratch"))
                                (not (name . "magit")))))
           ("Writing" (or (and (directory . "/Writings/*")
                               (not (mode . special-mode))
                               (not (name . "^\\*scratch"))
                               (not (name . "magit")))))
           ("PR Work" (or (and (directory . "/PR Work/*")
                               (not (mode . special-mode))
                               (not (name . "^\\*scratch"))
                               (not (name . "magit")))))
           ("ZK" (or (name . "*ZK")
                     (and (directory . "/ZK/")
                          (filename . "\\.org$")
                          (not (name . "^\\*scratch"))
                          (not (name . "magit")))))
           ("Slack" (mode . slack-message-buffer-mode))
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
           ("scratches" (name . "^\\*scratch"))
           ("***" (or (mode . org-side-tree-mode)
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
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (add-to-list 'completion-ignored-extensions ".DS_Store")

  (defvar gr/dired-open-externally-regexp
    (rx (or "pdf"
            "docx")))

  (defun gr/dired-find-file-other-window ()
    "In dired, open directories in same window, files in other window."
    (interactive)
    (let* ((switch-to-buffer-obey-display-actions nil)
           (file (dired-get-file-for-visit))
           (ext (file-name-extension file)))
      (cond ((file-directory-p file)
             (dired--find-possibly-alternative-file file))
            ((and (not current-prefix-arg)
                  ext
                  (string-match gr/dired-open-externally-regexp
                                ext))
             (call-process "open" nil 0 nil (expand-file-name file)))
            (t
             (dired--find-file #'find-file-other-window file)))))
  )

(use-package dired-x
  :ensure nil
  :hook
  (dired-mode-hook . dired-omit-mode)
  :custom
  (dired-guess-shell-alist-user
   '(("\\.pdf\\'" "open")
     ("\\.docx\\'" "open")))
  :config
  (setq dired-omit-files "\\.DS_Store\\|\\.dropbox\\|Icon\\\015")
  ;; show backup files
  (mapc
   (lambda (x) (delete x dired-omit-extensions))
   '("~")))

(use-package dired-subtree
  :after (dired)
  :bind
  (:map dired-mode-map
        ("i" . dired-subtree-toggle))
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package dired-narrow
  :after (dired)
  :bind
  (:map dired-mode-map
        ("/" . dired-narrow)))

;;;; avy

(use-package avy
  :bind
  ("M-g w" . avy-goto-word-1)
  (:map gr-map
        ;; ("C-." . avy-goto-word-0)
        ;; ("C-l" . avy-goto-line)
        ("C-g" . avy-goto-char-timer)
        ("C-." . avy-goto-char-timer))
  (:map isearch-mode-map
        ("C-." . avy-isearch))
  :bind*
  ("C-. C-," . avy-goto-char-timer)
  :custom
  (avy-background t)
  (avy-timeout-seconds 0.25)
  (avy-style 'at-full)
  (avy-single-candidate-jump t)
  (avy-all-windows 'all-frames)
  (avy-keys '(?a ?d ?f ?l ?r ?u ?g ?e ?i ?c ?s))

  (avy-dispatch-alist '((?, . avy-action-embark)
                        (?j . avy-action-aw-select)
                        (?2 . avy-action-split-below)
                        (?n . avy-action-open-in-new-frame)
                        (?m . avy-action-mark)
                        (?w . avy-action-copy)
                        (?k . avy-action-kill-stay)
                        (?K . avy-action-kill-whole-line)
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

  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

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
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))))
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
  (:map embark-symbol-map
        ("h" . helpful-symbol))
  (:map embark-become-help-map
        ("v" . helpful-variable)
        ("f" . helpful-callable)
        ("h" . helpful-symbol))
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
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t)
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)))

;;; eww /shr / web browsing

(use-package shr
  :custom
  (shr-inhibit-images nil)
  (shr-max-image-proportion 0.7))

(use-package eww
  :bind
  (:map gr-map
        ("G" . eww-duckduckgo))
  (:map embark-region-map
        ("G w" . eww-wiki)
        ("G g" . eww-duckduckgo))
  (:map embark-symbol-map
        ("G w" . eww-wiki)
        ("G g" . eww-duckduckgo))
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
  :hook
  (eww-mode-hook . olivetti-mode)
  (eww-mode-hook .   (lambda () (setq olivetti-body-width .80)))
  (eww-after-render-hook . eww-readable)

  :custom
  (eww-search-prefix "https://html.duckduckgo.com/html/?q=")
  (eww-download-directory (expand-file-name "~/Downloads"))

  :config
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
  )

;;;; pass

(use-package pass
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

;;;; wind-move

(use-package windmove
  :bind*
  ("C-M-j" . windmove-left)
  ("C-M-k" . windmove-down)
  ("C-M-i" . windmove-up)
  ("C-M-l" . windmove-right))

;;;; ace-window

(use-package ace-window
  :bind
  ("C-x o" . ace-window)

  ;; :custom-face
  ;; (aw-leading-char-face ((t (:family "Menlo" :foreground "red" :height   2.5))))

  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?l))
  (aw-scope 'visible)
  (aw-dispatch-always t)
  (aw-ignore-current nil)
  (aw-ignore-on t)
  ;; doesn't work with yabai
  ;; (ace-window-posframe-mode -1)

  :config
  (setq aw-dispatch-alist
        '((?b aw-switch-buffer-in-window "Select Buffer in Target")
          (?w aw-swap-window "Swap Current and Target")
          (?m aw-copy-window "Move Current to Target")
          (?2 aw-split-window-vert "Split Vert Window")
          (?3 aw-split-window-horz "Split Horz Window")
          (?k aw-delete-window "Delete Window")
          (?0 aw-delete-window "Delete Window")
          (?? aw-show-dispatch-help)))


  (define-advice aw--switch-buffer
      (:override nil aw--consult-buffer)
    (call-interactively #'consult-buffer))

  )

;;;; popper

(use-package popper
  :bind (("C-\\"   . popper-toggle)
         ("M-\\"   . popper-cycle)
         ("C-M-\\" . popper-toggle-type)
         )
  :init
  (setq popper-reference-buffers
        '("\\*xref\\*"
          "\\*chatgpt\\*"
          "\\*gptel-\\*"
          "\\*slack"
          "elfeed-entry"
          dired-mode
          magit-status-mode
          org-side-tree-mode
          occur-mode
          grep-mode
          zk-index-mode
          eshell-mode
          quick-sdcv-mode
          org-agenda-mode
          compilation-mode))
  (setq popper-display-control nil)
  :config

  (popper-mode 1)

  (defun gr/popper-group-function ()
    ;; the condition has to match both the window and the popup?
    (cond
     ((or (zk-file-p (buffer-file-name))
          (string-match-p zk-index-buffer-name (buffer-name)))
      'zk)
     (t 'default-group)))

  ;; when popper-toggle is called on a regular window,
  ;; the group function is called in that window,
  ;; to find what group that window is associated with
  ;; if the group function returns anything non-nil
  ;; it considers that a group, whether it is or not

  (setq popper-group-function #'gr/popper-group-function)

  (defun popper--modified-mode-line ()
    "Return modified mode-line string."
    (when popper-mode-line
      (if (consp mode-line-format)
          (if (member popper-mode-line mode-line-format)
              mode-line-format
            (append (cl-subseq (default-value 'mode-line-format) 0 popper-mode-line-position)
                    ;; use cons instead of list
                    ;; using list makes global-mode-string disappear
                    ;; in popper buffers
                    ;; when mode-line-format includes
                    ;; mode-line-format-right-align
                    (cons popper-mode-line
                          (nthcdr popper-mode-line-position
                                  (default-value 'mode-line-format)))))
        mode-line-format)))
  )

;;;; google-translate

(use-package google-translate
  :bind
  (:map gr-map
        ("t" . gr/translate))
  (:map embark-identifier-map
        ("t" . gr/translate))
  (:map embark-region-map
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
  :custom
  (google-translate-translation-directions-alist
   '(("lt" . "en")
     ("en" . "lt"))))

;;;; cyclekey

(use-package cyclekey
  :ensure nil
  :vc (:url "https://github.com/shankar2k/cyclekey")
  :bind
  (:map gr-map
        ("l" . cyclekey-cycle))
  ( :repeat-map cyclekey-repeat-map
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
  :custom
  (golden-ratio-scroll-screen-ratio 3)
  (golden-ratio-scroll-highlight-flag 'both)

  ;; :custom-face
  ;; (golden-ratio-scroll-highlight-line-face ((t (:background   "darkseagreen2" :foreground "black" :weight normal :inherit highlight))))

  :defer 1
  :config
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)

  )

;;;; osx-reveal-in-finder

(use-package reveal-in-osx-finder
  :after (embark embark-org)
  :bind
  (:map embark-file-map
        ("O" . gr/embark-reveal-in-osx-finder))
  (:map embark-org-link-map
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

(defun gr/api-key-from-auth-source (host)
  (plist-get
   (car (auth-source-search
         :host host))
   :secret))

(use-package gptel
  :bind
  ("C-c C-<return>" . gptel-menu)
  ("C-c <return>" . gptel-send)
  ("C-M-g" . gptel-abort)
  ;;("C-h C-q" . gptel-quick)
  (:map gr-map
        ("g" . gptel))
  (:map embark-file-map
        ("g" . gptel-add-file))
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model "gpt-4.1-mini")
  :init
  (setq gptel-expert-commands t
        gptel-track-response nil
        gptel-include-reasoning nil
        gptel-use-tools nil)
  :config
  (require 'gr-gptel-setup)

  ;; (transient-suffix-put 'gptel-menu (kbd "-m") :key "M")
  ;; (transient-suffix-put 'gptel-menu (kbd "-T") :key "T")


  (gptel-make-gemini "Gemini"
    :key (gptel-api-key-from-auth-source "api.gemini.ai")
    :stream t)

  (gptel-make-anthropic "Claude"
    :key (gptel-api-key-from-auth-source "api.anthropic.ai")
    :stream t)

  (gptel-make-perplexity "Perplexity"
    :key (gptel-api-key-from-auth-source "api.perplexity.ai")
    :stream t)
  )

(use-package gptel-org
  :ensure nil
  :after gptel
  :custom
  ;; use whole doc by default
  (gptel-org-branching-context nil)
  :config

  ;; (progn (declare-function org-element-lineage-map "org-element-ast")
  ;;        (defalias 'gptel-org--element-lineage-map 'org-element-lineage-map))

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
      (let ((gptel-model "gpt-4.1-nano") ;; cheap and fast
            (prompt (read-string "Ask: " nil 'gptel-ask--history)))
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


;;;; org-chef

(use-package org-chef)

;;;; transpose-frame

(use-package transpose-frame
  :vc (:url "https://github.com/emacsorphanage/transpose-frame"))


;;;; tab-sets

(use-package tab-sets
  :ensure nil
  :bind
  (:map gr-map
        ("r" . tab-sets-open)
        ("R" . tab-sets-save))
  :custom
  (tab-sets-data-file (concat user-emacs-directory "var/tab-sets.eld"))
  (tab-sets-same-frame nil)
  (tab-sets-bookmark-store nil)
  :demand t
  :config
  (tab-sets-setup-embark)
  ;; (tab-sets-reconcile-bookmarks)

  (with-eval-after-load 'consult
    (add-to-list
     'consult-bookmark-narrow
     '(?t "Tab-Set" tab-sets-bookmark-handler)))
  )

;;;; triples

(use-package triples)

;;;; doc-tags

(use-package doc-tags
  :ensure nil
  :bind
  (:map gr-map
        ("C-f" . doc-tags-find-file)
        ("f" . doc-tags-find-file))
  (:map embark-file-map
        ("a" . doc-tags-add-doc))
  :custom
  (doc-tags-db-file "~/.emacs.d/var/doc-tags.db"))


;;;; macos-finder-tags

(use-package macos-finder-tags
  :ensure nil
  :bind
  ("M-s t" . macos-finder-tags))


;;;; emacs-reader

;; (use-package reader
;;   :vc (:url "https://codeberg.org/divyaranjan/emacs-reader"
;;   	    :make "all"))

;;;; empv - video player

(use-package empv
  :config
  (setopt empv-invidious-instance "https://inv.thepixora.com/api/v1")
  (add-to-list 'empv-mpv-args "--ytdl-format=bestvideo+bestaudio/best[ext=mp4]/best"))


;;;; consult-recoll

(use-package consult-recoll
  :custom
  (consult-recoll-inline-snippets t)
  :config
  (defun gr-recoll-format (_title url _mime-type)
    (replace-regexp-in-string "file:///Users/grantrosson/Documents" "" url))

  (setq consult-recoll-format-candidate #'gr-recoll-format)
  (consult-recoll-embark-setup)

  (defun gr/consult-recoll-open-pdf (file &optional page)
    (shell-command (format "open '%s'" file)))

  (add-to-list 'consult-recoll-open-fns '("application/pdf" . gr/consult-recoll-open-pdf))
  )

;;; Dev

;;;; esup

(use-package esup
  :custom
  (esup-user-init-file (concat user-emacs-directory "init.el"))
  :config
  (setq esup-depth 0))

;;;; emacs-benchmark

(use-package elisp-benchmarks)

;;;; melpazoid

(use-package melpazoid
  :ensure nil
  :vc (:url "https://github.com/riscy/melpazoid")
  "elpa/melpazoid/melpazoid/melpazoid.el"
  :bind
  (:map gr-map
        ("E" . gr/toggle-elisp-check-buffer))
  :config
  (defun gr/toggle-elisp-check-buffer ()
    "Do checks on elisp buffer."
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
  :custom
  (flycheck-emacs-lisp-load-path 'inherit))

(use-package package-lint)

;;;; aggressive-indent

(use-package aggressive-indent
  :hook (prog-mode-hook)
  :diminish)

;;;; org-transclusion

(use-package org-transclusion
  :disabled
  :bind
  (:map org-transclusion-map
        ("d" . nil)
        ("D" . org-transclusion-remove)
        ("C-<left>" . org-transclusion-promote-subtree)
        ("C-<right>". org-transclusion-demote-subtree)
        ("o" . org-transclusion-move-to-source))
  (:map zk-file-map
        ("T". zk-org-transclusion-insert-link))
  (:map zk-id-map
        ("T". zk-org-transclusion-insert-link))
  :config
  (require 'org-transclusion-indent-mode)

  (define-fringe-bitmap 'empty-line
    [#b01000000
     #b01000000
     #b01000000
     #b01000000
     #b01000000
     #b01000000
     #b01000000
     #b01000000]
    nil nil 'center)

  (define-fringe-bitmap 'org-transclusion-fringe-bitmap
    [#b00000000
     #b00000000
     #b00010000
     #b00111000
     #b00010000
     #b00000000
     #b00000000
     #b00000000]
    nil nil 'center)

  (defun zk-org-transclusion-add-file (link plist)
    (when-let* ((link (org-element-property :path link))
                (id (and (string-match zk-id-regexp link)
                         (match-string 0 link)))
                (file-path (zk--parse-id 'file-path id))
                (new-link (with-temp-buffer
                            (insert "[[file:")
                            (insert file-path)
                            (insert "]]")
                            (beginning-of-buffer)
                            (org-element-link-parser))))
      (or (org-transclusion-add-src-lines new-link plist)
          (org-transclusion-add-org-file new-link plist))))

  (add-to-list 'org-transclusion-add-functions
               #'zk-org-transclusion-add-file)

  (defun zk-org-transclusion-insert-link (arg)
    "Insert heading and zk-link for org-transclusion."
    (interactive
     (list (list (funcall zk-select-file-function "Insert link: "))))
    (let ((title (zk--parse-file 'title arg)))
      (insert (format "* %s\n#+transclude: " title))
      (end-of-line)
      (zk--insert-link arg)
      (insert " :lines 2-")
      (org-transclusion-add)))
  )

;;;; whitespace-mode

(use-package whitespace
  :custom
  (whitespace-style '(face trailing lines)))


;;;; popup-frame

;; Run commands in a popup frame
;; from https://protesilaos.com/codelog/2024-09-19-emacs-command-popup-frame-emacsclient/ a

(defun gr/popup-frame-delete (&rest _)
  "Kill selected selected frame if it has parameter `gr/popup-frame'.
Use this function via a hook."
  (when (frame-parameter nil 'gr/popup-frame)
    (delete-frame)))

(defmacro gr/popup-frame-define (command title &optional delete-frame)
  "Define interactive function to call COMMAND in frame with TITLE.
Make the new frame have the `gr/popup-frame' parameter.
Set DELETE-FRAME to non-nil for minibuffer-centric functions, to close after completion."
  `(defun ,(intern (format "gr/popup-frame-%s" command)) ()
     ,(format "Run `%s' in a popup frame with `gr/popup-frame' parameter.
Also see `gr/popup-frame-delete'." command)
     (interactive)
     (let* ((display-buffer-alist '((""
                                     (display-buffer-full-frame))))
            (frame (make-frame
                    ;; prevent yabai management
                    ;; name defined in
                    ;; ~/Repos/emacs-build/yabai-emacs-window-handler.sh
                    '((title . ,title)
                      (window-system . ns)
                      (gr/popup-frame . t)))))
       (select-frame frame)
       (switch-to-buffer " gr/popup-frame-hidden-buffer")
       (condition-case nil
           (progn
             (call-interactively ',command)
             (delete-other-windows))
         ((quit error user-error)
          (delete-frame frame)))
       (when ,delete-frame
         (sit-for .2)
         (delete-frame frame)))))

;;;;; find file

(defun gr/find-file ()
  (interactive)
  (let ((default-directory "~/"))
    (call-interactively #'find-file)))

(gr/popup-frame-define gr/find-file "small-popup")

;;;;; org-capture popups

(gr/popup-frame-define org-capture "small-popup")

(declare-function org-capture "org-capture" (&optional goto keys))
(defvar org-capture-after-finalize-hook)

(add-hook 'org-capture-after-finalize-hook #'gr/popup-frame-delete)

;; see gr-org-capture
(defun gr/org-capture-scratch ()
  (interactive)
  (org-capture nil "s"))

(gr/popup-frame-define gr/org-capture-scratch "small-popup")

;;;;; translate

(defun gr/translate-popup ()
  (interactive)
  (let ((choice
         (completing-read "Select: " '("EN->LT" "LT->EN"))))
    (pcase choice
      ("EN->LT" (google-translate-query-translate-reverse))
      ("LT->EN" (google-translate-query-translate)))))

(gr/popup-frame-define gr/translate-popup "small-popup")

;;;;; agenda, mu4e, citar popups, et al

(gr/popup-frame-define zk-daily-note "medium-popup")

(gr/popup-frame-define gr/org-agenda "medium-popup")

(gr/popup-frame-define gr/mu4e-unread "large-popup")

(gr/popup-frame-define password-store-copy "small-popup" 'delete-frame)

(gr/popup-frame-define citar-open-files "small-popup" 'delete-frame)

(gr/popup-frame-define execute-extended-command "small-popup")

(gr/popup-frame-define gptel-ask "small-popup")

;; emacsclient -e '(gr/popup-frame-org-capture)'
;; add command above to ~/.skhdrc
;; add frame name to ~/Repos/emacs-build/yabai-emacs-window-handler


;;; shell path

;; note: when starting emacs from the terminal, it inherits paths from the shell environment
;; so, if I start through skhd (using that program to run emacs-start script), emacs inherits
;; the environment variables set in that program’s plist, here:
;; ~/Library/LaunchAgents/com.koekeishiya.skhd.plist
;; I edited the envvar SHELL in that plist, to ensure that skhd is run using dash (faster)
;; but I also injected PATH there;; maybe that’s not necessary?

;;; variable reset

(setq debug-on-error nil)
(put 'list-timers 'disabled nil)
(put 'scroll-left 'disabled nil)

