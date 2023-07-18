;; init.el                    -*- lexical-binding: t; -*-

;;; Misc Startups

(use-package gcmh
  :load-path "~/.emacs.d/lisp/"
  :diminish
  :config
  (gcmh-mode 1))

(setq debug-on-error t)
(setq inhibit-startup-echo-area-message "grantrosson")

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;;; safe-local-variable-values

(setq safe-local-variable-values
      '((eval gr/daily-notes-new-headline)
        (dired-omit-size-limit)
        (zk-link-and-title-format . "+%t [[%i]]+")
        (gr/mmd-citation-use . t)
        (eval . (gr/toggle-capslock))
        (eval . (text-scale-adjust 10))))

;;; Basics

;;;; Emacs

(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "lisp")))
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "my-lisp")))
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "priv-lisp")))

(use-package exec-path-from-shell
  :defer 1
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  (setq exec-path-from-shell-warn-duration-millis 800)
  :config
  (exec-path-from-shell-copy-env "PKG_CONFIG_PATH")
  (exec-path-from-shell-initialize))

(use-package diminish
  :defer 1)

(use-package emacs
  :diminish
  eldoc-mode
  visual-line-mode
  abbrev-mode
  auto-fill-mode
  :bind
  ("C-x [" . beginning-of-buffer)
  ("C-x ]" . end-of-buffer)
  ("C-c e" . eval-buffer)
  ("C-x e" . eval-last-sexp)
  ("C-x E" .  kmacro-end-and-call-macro)
  ("M-o" . other-window)
  (:map help-mode-map
        ("o" . link-hint-open-link))
  ;; rest of config in early-init.el
  :hook
  (prog-mode-hook . (lambda () (setq show-trailing-whitespace t)))
  (prog-mode-hook . visual-line-mode)
  (text-mode-hook . visual-line-mode))

(use-package elec-pair
  :disabled
  :init
  (electric-pair-mode)
  :custom
  (electric-pair-inhibit-predicate 'ignore))

(use-package info
  :bind
  (:map Info-mode-map
        ("o" . link-hint-open-link)))

(with-current-buffer "*Messages*"
  (visual-line-mode))

(with-current-buffer "*scratch*"
  (visual-line-mode))

(use-package tab-bar
  :bind
  ("s-{" . tab-bar-switch-to-prev-tab)
  ("s-}" . tab-bar-switch-to-next-tab)
  ("C-{" . tab-bar-switch-to-prev-tab)
  ("C-}" . tab-bar-switch-to-next-tab)
  ("M-s-n" . gr/tab-to-frame)
  :custom-face
  (tab-bar ((t (:font "Menlo" :height .75))))
  (tab-bar-tab ((t (:background "grey60" :box (:line-width 1 :style released-button)))))
  (tab-bar-tab-inactive ((t (:background "lightgrey" :box (:line-width 1 :style pressed-button)))))
  :config
  (setq
   tab-bar-show t
   tab-bar-close-button-show nil
   tab-bar-new-button-show nil
   tab-bar-new-tab-to 'rightmost
   tab-bar-new-tab-choice "*scratch*"
   tab-bar-auto-width nil
   tab-bar-tab-name-function 'tab-bar-tab-name-truncated)

  (defun tab-bar-disable-in-frame ()
    (when tab-bar-mode
      (toggle-frame-tab-bar)))

  (defun gr/tab-to-frame ()
    "Open current tab in new frame."
    (interactive)
    (let* ((buffer (current-buffer)))
      (when (< 1 (length (tab-bar-tabs (window-frame))))
        (tab-close))
      (gr/make-frame)
      (switch-to-buffer buffer)))
  )

(use-package hydra :defer 1)

(use-package keycast :defer 1)

(use-package ruta-bg
  :demand t
  :ensure nil)

;;;; themes

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(load-theme 'gr-light t)

(use-package ef-themes :defer 1)

(defun gr/toggle-theme ()
  (interactive)
  (if (equal custom-enabled-themes '(gr-light))
      (progn
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme 'ef-bio :no-confirm))
    (progn
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme 'gr-light :no-confirm))))

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
 ("s-q" . save-buffers-kill-emacs)
 ("s-f" . consult-line)
 ("s-w" . gr/delete-frame-or-tab)
 ("s-t" . tab-new)
 ("s-n" . gr/make-frame))

(setq ns-alternate-modifier 'meta)
(setq ns-command-modifier 'super)

(when (eq system-type 'darwin)
  (setq mac-right-command-modifier 'control))

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

(setq switch-to-buffer-obey-display-actions nil)
;; with t, popwin-dummy buffer shows up with elfeed-entry

(defun make-display-buffer-matcher-function (major-modes)
  (lambda (buffer-name action)
    (with-current-buffer buffer-name (apply #'derived-mode-p major-modes))))

;; NOTE: windows designated as popups are controlled by popper, so these
;; settings have no effect on them; eg zk-index, dired buffers, Messages;
;; see 'gr/popper-select-buffer-at-bottom

(setq display-buffer-alist
      `(("*Async Shell Command*"
         (display-buffer-no-window))

        ("\\*elfeed-entry"
         (display-buffer-at-bottom)
         (window-height . 0.6))

        ("Dictionary"
         (display-buffer-at-bottom)
         (window-height . 0.6))

        ("*mu4e-main*"
         (display-buffer-full-frame))

        ("Org Links" display-buffer-no-window
         (allow-no-window . t))

        (,(concat
           "\\*\\("
           (string-join
            '("Completions""Backups:" "helpful"
              "CAPTURE" "Pp Eval Output" "eshell" "Backtrace"
              "Messages" "Metahelp" "Python"
              "Warnings" "Go Translate" "Google Translate"
              "Org Select" "Compile-Log" "[Hh]elp" "annotations"
              "calfw-details" "Embark Collect")
            "\\|") "\\)")
         (display-buffer-at-bottom)
         (window-height . 0.45))
        ))

;;;; gr-functions and gr-map

(use-package gr-functions
  :ensure nil
  :init
  (define-prefix-command 'gr-map)
  (bind-keys :map global-map
             :prefix-map gr-map
             :prefix "C-."
             ("C-." . avy-goto-char-timer)
             ("/" . switch-to-minibuffer-window)
             ("C-/" . exit-minibuffer)
             ("n" . gr/daily-notes)
             ("o" . link-hint-aw-select)
             ("O" . link-hint-other-tab)
             ("i" . gr/open-init-file)
             ;;("C-t" . gr/open-tasks-file)
             ("C-t" . gr/toggle-theme)
             ("f" . gr/open-fragments-file)
             ("C-f" . gr/open-fragments-file-other-frame)
             ("m" . mu4e)
             ("M" . gr/open-mu4e)
             ("a" . gr/org-agenda)
             ("c" . gr/calfw-open-org-calendar)
             ("b" . consult-bookmark)
             ("g" . eww-duckduckgo)
             ("j" . gr/org-journal-new-entry)
             ("e" . ebib)
             ("W" . org-wc-display)
             ("w" . prot-eww-map)
             ("D" . gr/lookup-word-at-point)
             ("d" . sdcv-search)
             ("L" . toggle-truncate-lines)
             ("t" . google-translate-buffer)
             ("s" . hydra-mac-speak/body)
             ("p" . password-store-copy)))

;;;; isearch

(use-package isearch
  :ensure nil
  :bind
  (:map isearch-mode-map
        ("C-n" . isearch-repeat-forward)
        ("C-p" . isearch-repeat-backward)))

;;;; re-builder

(use-package re-builder
  :defer 1
  :init
  (setq reb-re-syntax 'string))

;;;; init-lock

(use-package init-lock
  :load-path "my-lisp/init-lock"
  :defer t
  :custom
  (init-lock-files '("~/.emacs.d/init.el")))

;;;; link-hint

(use-package link-hint
  :defer 1
  :custom
  (link-hint-message nil))

(use-package link-hint-aw-select
  :load-path "my-lisp/link-hint-aw-select"
  :bind
  (:map gr-map
        ("o" . link-hint-aw-select))
  :config
  ;; open org-links in same window
  ;; allows link-hint--aw-select-org-link to work properly
  (with-eval-after-load 'org
    (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)))

(use-package link-hint-preview
  :load-path "my-lisp/link-hint-preview"
  :bind
  (:map gr-map
        ("p" . link-hint-preview))
  :hook
  (link-hint-preview-mode-hook . tab-bar-disable-in-frame)
  (link-hint-preview-mode-hook . link-hint-preview-toggle-frame-mode-line))

;;;; recentf

(use-package recentf
  :defer 1
  :config
  (recentf-mode))

;;; Org

;;;; org-mode

(use-package org
  :load-path "elpa/org/lisp"
  :diminish
  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  ("C-c C" . org-clock-goto)
  (:map org-mode-map
        ("RET" . scimax/org-return)
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
        ("C-<return>" . org-meta-return)
        ("M-<return>" . org-insert-heading-respect-content)
        ("C-c $" . gr/org-mark-done-and-archive-datetree)
        ("" . org-cycle-agenda-files))
  :mode (("\\.org$" . org-mode))
  :init
  (with-eval-after-load 'org
    (setq org-structure-template-alist
          '(("c" . "comment")
            ("q" . "quote")
            ("n" . "note")
            ("s" . "src")
            ("v" . "verse")
            ("el" . "src emacs-lisp")
            ("C" . "center"))))
  (with-eval-after-load 'org-indent
    (diminish 'org-indent-mode))
  :config
  (unbind-key "C-," org-mode-map)
  (unbind-key "C-'" org-mode-map)
  (add-to-list 'org-file-apps '("\\.docx\\'" . default) 'append)

  :custom-face
  (org-drawer ((t (:height .8))))
  (org-special-keyword ((t (:height .8))))
  (org-hide ((t (:foreground "white"))))

  :custom
  (org-ellipsis " ▼") ;◣ ▼ ▽ ► ➽
  (default-major-mode 'org-mode)
  (org-directory "~/Dropbox/org")
  (org-use-speed-commands t)
  (org-speed-commands-user '(("k" . ignore) ("=" . ignore) ("o" . ignore)))
  (org-startup-indented t)
  (org-table-use-standard-references 'from)
  (org-catch-invisible-edits 'smart)
  (org-tags-column -67)
  (org-tag-alist '(("export")("noexport") ("noheadline")))
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers nil)
  (org-link-keep-stored-after-insertion t)
  (org-link-search-must-match-exact-headline t)
  (org-support-shift-select nil)
  (org-return-follows-link t)
  (org-export-backends '(ascii html latex md odt org))
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
     ("+" (t (:background "gray85" :height .9)))
     ("~" verbatim)))
  (org-fold-core-style 'text-properties)
  (org-startup-with-latex-preview nil)
  (org-use-fast-todo-selection 'expert)
  (org-edit-src-content-indentation 0)
  (org-src-preserve-indentation nil)
  (org-log-states-order-reversed nil)

  (org-refile-targets '((nil :maxlevel . 2)
                        (org-agenda-files :maxlevel . 2)))

  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)))
  (defun org-babel-execute:yaml (body params) body))

(use-package org-agenda-setup
  :ensure nil
  :defer 1
  :after org)

(use-package org-capture-setup
  :ensure nil
  :after org
  :defer 1)

(use-package org-gcal-setup
  :ensure nil
  :defer 1)

(use-package gr-org-extras
  :ensure nil
  :defer 1)

;;;; org-superstar

(use-package org-superstar
  :defer t
  :after org
  :hook (org-mode-hook)
  :config
  (setq org-superstar-headline-bullets-list  '("◉" "○" "▪" "◦" "•" "▫" "•" "▫"))
  (setq org-superstar-item-bullet-alist
        '((?+ . ?◦)
          (?* . ?➤)
          (?- . ?–))))

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

;;; Completion

;;;; vertico

(use-package vertico
  :init (vertico-mode)
  :bind* (:map vertico-map
               ("C-j" . vertico-exit-input)
               ("C-g" . keyboard-escape-quit))
  :custom
  (vertico-cycle t)
  (vertico-count 7)

  :config
  (vertico-multiform-mode)

  (setq vertico-multiform-commands
        '((consult-imenu buffer)
          (execute-extended-command unobtrusive)
          (consult-dir buffer)))

  (setq vertico-multiform-categories
        '((file buffer
                (vertico-sort-function . sort-directories-first-alpha))
          (zk-file buffer)
          (bookmark buffer)
          (consult-grep buffer)))

  (defun sort-directories-first-alpha (files)
    (setq files (vertico-sort-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  (defun gr/vertico-display-action (buffer alist)
    (display-buffer-reuse-window
     buffer
     (append alist '((window-height . 0.3)))))

  (setq vertico-buffer-display-action '(display-buffer-in-side-window
                                        (window-height . 0.3)
                                        (side . bottom)))
  )

(setq crm-separator ",")

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
        ("M" . embark-attach-file)
        ("p" . gr/embark-save-absolute-path)
        ("P" . gr/embark-insert-absolute-path))
  (:map embark-region-map
        ("t" . title-case-region)
        ("G w" . eww-wiki)
        ("G g" . eww-duckduckgo)
        ("z" . zk-search))
  (:map embark-url-map
        ("s" . browse-url-generic))
  :custom
  (embark-help-key "?")
  (embark-keymap-prompter-key ",")
  (embark-quit-after-action t)
  (embark-confirm-act-all nil)
  (embark-indicators '(embark-minimal-indicator embark-highlight-indicator))
  (prefix-help-command #'embark-prefix-help-command)
  :config

  (defun embark-act-noquit ()
    "Run action but don't quit the minibuffer afterwards."
    (interactive)
    (let ((embark-quit-after-action nil))
      (embark-act)))

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

  (defvar-keymap this-buffer-map
    :doc "Commands to act on current file or buffer."
    :parent embark-general-map
    "RET" #'eval-buffer
    "e" #'eval-buffer
    "R" #'rename-file
    "D" #'delete-file
    "W" #'write-file
    "$" #'ispell
    "!" #'shell-command
    "&" #'async-shell-command
    "x" #'embark-open-externally         ; useful for PDFs
    "c" #'copy-file
    "k" #'kill-buffer
    "z" #'bury-buffer
    "s" #'embark-eshell
    "|" #'embark-shell-command-on-buffer
    "g" #'revert-buffer
    "p" #'pwd
    "SPC" #'mark-whole-buffer
    "<" #'previous-buffer
    ">" #'next-buffer)

  (add-to-list 'embark-repeat-actions #'previous-buffer)
  (add-to-list 'embark-repeat-actions #'next-buffer)

  )

(use-package embark-org
  :ensure nil
  :after embark
  :bind
  (:map embark-org-link-map
        ("x" . embark-open-externally)))

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
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s m" . multi-occur)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  (:map consult-narrow-map
        ("?" . consult-narrow-help))
  :bind*
  ("C-c [" . consult-global-mark)
  ("C-:" . consult-imenu)
  ("C-;" . consult-outline)
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode)
  :custom
  (completion-in-region-function 'consult-completion-in-region)
  (consult-fontify-preserve nil)
  (consult-project-function nil)
  (consult-async-split-style 'semicolon)
  (consult-preview-key "C-{")

  :config

  ;; consult-preview settings

  ;; (consult-customize
  ;;  consult-git-grep consult-grep consult-global-mark consult-ripgrep
  ;;  consult-bookmark consult--source-buffer consult-recent-file consult-xref
  ;;  consult--source-bookmark consult-buffer
  ;;  :preview-key '("C-{"
  ;;                 :debounce 1.5 any))

  ;;make C-s and C-r search forward and backward in consult-line
  ;;changed to make C-s call previous search term
  (defvar my-consult-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-s" #'previous-history-element)
      ;;(define-key map "\C-s" #'next-line)
      ;;(define-key map "\C-r" #'previous-line)
      map))
  (consult-customize consult-line :keymap my-consult-line-map)

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
  :bind ("C-x C-d" . consult-dir)
  (:map vertico-map
        ("C-x C-j" . consult-dir-jump-file))
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

(add-hook 'dired-after-readin-hook 'recentd-track-opened-file)
(add-hook 'kill-buffer-hook 'recentd-track-closed-file)

;;;; marginalia

(use-package marginalia
  :bind
  ("M-A" . marginalia-cycle)
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;;; orderless

(use-package orderless
  :init
  (setq orderless-matching-styles '(orderless-prefixes
                                    orderless-regexp)
        completion-styles '(orderless partial-completion initials basic)
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

;;;; cape

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  :bind (("M-i" . completion-at-point)
         ("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)))

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(setq tab-always-indent 'complete)

;;;; savehist

(use-package savehist
  :defer 1
  :config
  (savehist-mode 1)
  (setq savehist-additional-variables
        '(citar-history search-ring regexp-search-ring)))

;;;; prescient / company-prescient

(use-package prescient
  :defer t
  :config
  (prescient-persist-mode 1)
  :custom
  (prescient-aggressive-file-save t))

(use-package vertico-prescient
  :defer t
  :config
  (vertico-prescient-mode))

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
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :hook
  (prog-mode-hook . tempel-setup-capf)
  (text-mode-hook . tempel-setup-capf)
  )

;;; Citation / Bibliography
;;;; citar

(defvar gr/bibliography '("~/Dropbox/gr-bibliography.bib"))

(use-package citar
  :after (oc misc-file-handling devonthink-dir)
  :bind
  (:map org-mode-map
        ("C-c \\" . gr/citar-insert-citation))
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
  (citar-bibliography gr/bibliography)
  (citar-library-paths (list-dirs-recursively devonthink-dir))
  (citar-notes-paths '("~/Dropbox/ZK/Zettels"))
  (citar-additional-fields '("doi" "url" "crossref"))
  (citar-library-file-extensions '("pdf" "epub"))
  (citar-file-note-extensions '("org" "md"))
  (citar-file-open-function 'citar-file-open-external)
  (citar-file-additional-files-separator " ")
  (citar-open-prompt t)
  (citar-format-reference-function 'citar-citeproc-format-reference)
  (citar-display-transform-functions nil)
  (citar-select-multiple t)
  (citar-open-resources '(:files :notes :create-notes))
  (citar-citeproc-csl-style
   "chicago-fullnote-bibliography-short-title-subsequent.csl")

  :config

  (add-to-list 'citar-library-paths "~/Dropbox/Dickinson Primary/")

  (defun ex/search-pdf-contents (keys &optional str)
    "Search pdfs."
    (interactive (list (citar-select-refs)))
    (let* (files
           (hash (citar-file--directory-files
                  citar-library-paths
                  keys
                  '("pdf")
                  citar-file-additional-files-separator))
           (search-str (or str (read-string "Search string: ")))
           (files (progn (dolist (key keys)
                           (push (gethash key hash) files))
                         (flatten-list files))))
      (pdf-occur-search files search-str t)))

  ;;(add-to-list 'embark-multitarget-actions #'ex/search-pdf-contents)

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

  )

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

;;;; org-cite

(use-package oc
  :ensure nil
  :defer 1
  :init
  (setq org-cite-csl-styles-dir "~/.csl"
        org-cite-csl-locales-dir "~/.csl/locales"
        org-odt-preferred-output-format nil ;; "docx"
        org-odt-styles-file "~/Dropbox/Academic/template.ott"
        org-cite-global-bibliography gr/bibliography)
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
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
        ("?" . (lambda () (interactive) (embark-bindings-in-keymap ebib-index-mode-map)))
        ("h" . hydra-ebib/body)
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
                   (embark-bindings-in-keymap ebib-entry-mode-map)))
        ("?" . hydra-ebib/body)
        ("d" . nil)
        ("j" . ebib-jump-to-entry)
        ("e" . ebib-edit-current-field)
        ("O" . ebib-filters-apply-filter)
        ("s-s" . ebib-save-curent-database)
        ("q" . ebib-quit-entry-buffer)
        ("k" . nil))
  :hook
  (ebib-entry-mode-hook . visual-line-mode)
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
  (:map citar-map
        ("e" . citar-ebib-jump-to-entry))
  :config
  (with-eval-after-load 'hydra
    (defhydra hydra-ebib (:hint nil :color blue)
              "
  _j_: Jump to Entry   _k_: Add Keyword    _!_: Auto-Citekey     _s_: DOI Lookup
  _O_: Apply Filter                        _E_: Edit Citekey     _S_: ISBN Lookup
  _C_: Cancel Filter   _D_: Delete Field   _X_: Delete Entry     _I_: Auto Import
  "
              ("k" ebib-add-keywords-to-entry)
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
              ("q" nil))))

(use-package ebib-zotero
  :ensure nil
  :commands ebib-auto-import
  :bind
  (:map ebib-index-mode-map
        ("I" . ebib-zotero-import-identifier)))

(use-package pdf-drop-mode
  :vc (:url "https://github.com/rougier/pdf-drop-mode"
            :rev :newest)
  :defer 1
  :config
  (pdf-drop-mode)
  (setq pdf-drop-search-hook #'my/pdf-process))

(defun my/pdf-process (file doi)
  (ebib-zotero-import-identifier (cdr doi) file))

;;;; biblio / sci-hub

(use-package scihub
  :vc (:url "https://github.com/emacs-pe/scihub.el"
            :rev :newest)
  :defer 1
  :custom
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

;;;; zk

(use-package zk-setup
  :ensure nil
  :bind
  ("C-z" . hydra-zk/body))

;;;; sdcv-mode - stardict dictionary

(use-package sdcv-mode
  :vc (:url "https://github.com/gucong/emacs-sdcv"
            :rev :newest)
  :defer 1
  :custom
  (sdcv-buffer-name "*Dictionary*"))

;; note: dictionaries are in ~/.stardic/dic

;;;; ispell

(use-package ispell
  :defer t
  :bind
  ([remap dabbrev-expand] . hippie-expand)
  (:map ctl-x-map
        ("C-i" . endless/ispell-word-then-abbrev))
  :init
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t)
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

  ;; abbreviations and corrections stored in ~/.emacs.d/abbrev_defs

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
        (progn
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
                              bef aft (if p "loc" "glob")))))))
      (forward-word -2)))
  )

;;; org-reveal

(use-package ox-reveal
  ;;:defer 1
  :config
  (add-to-list 'org-export-backends 'reveal)
  :custom
  (org-reveal-single-file t)
  (org-reveal-hlevel 2))

;; backup if org-reveal-single-file doesn't work
(defun gr/org-reveal-export-and-inline ()
  "Export current org buffer to standalone html file.
Uses 'inliner' npm utility to inline CSS, images, and javascript."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "Not an org buffer."))
  (setq org-reveal-single-file nil)
  (let ((file (file-relative-name (concat default-directory (org-export-output-file-name " 1.html"))))
        (new (file-relative-name (concat default-directory (org-export-output-file-name ".html")))))
    (org-export-to-file 'reveal file)
    (async-shell-command (format "inliner '%s' > '%s'" file new))
    (dired-jump nil new))
  (setq org-reveal-single-file t))

(defun gr/html-single-file (file)
  (interactive "fFile: ")
  (let ((new (concat default-directory "single.html")))
    (async-shell-command (format "inliner '%s' > '%s'" (expand-file-name file) new))))

;;;; pdf-tools

(use-package pdf-tools
  :bind
  (:map pdf-view-mode-map
        ("h" . pdf-annot-add-highlight-markup-annotation)
        ("l" . pdf-annot-list-annotations)
        ("t" . pdf-annot-add-text-annotation)
        ("D" . pdf-annot-delete)
        ([remap beginning-of-buffer] . image-bob)
        ([remap end-of-buffer] . image-eob)
        ("C-s" . pdf-occur))
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :hook
  (pdf-annot-list-mode-hook . (lambda () (pdf-annot-list-follow-minor-mode)))
  (pdf-occur-buffer-mode-hook . next-error-follow-minor-mode)
  (pdf-view-mode-hook . (lambda () (setq-local make-backup-files nil)))
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights")

  :config
  (pdf-tools-install)
  (require 'tablist)
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-view-use-scaling t)
  ;; turn off cua so copy works
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  ;;(add-hook 'pdf-view-mode-hook (lambda () (linum-mode -1)))
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

  ;; set RET to save annotations
  (with-eval-after-load 'pdf-annot
    (define-key pdf-annot-edit-contents-minor-mode-map (kbd "RET") 'pdf-annot-edit-contents-commit)
    (define-key pdf-annot-edit-contents-minor-mode-map (kbd "S-RET") 'newline)

    ;; funtion to save after adding comment
    (define-advice pdf-annot-edit-contents-commit
        (:after nil bjm/save-buffer-no-args)
      "Save buffer ignoring arguments"
      (save-buffer)))

  )

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

;; set wider margins in latex
;; (setq org-latex-packages-alist '(("margin=1in" "geometry" nil)))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("memoir" "\\documentclass[11pt]{memoir}"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

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

(defun gr/bluepencil-deploy ()
  (interactive)
  (shell-command "cd ~/Dropbox/Sites/bluepencil/bluepencil-web && ./deploy.sh"))

(defun gr/bluepencil-test ()
  (interactive)
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (if
        (equal 1 (shell-command "pgrep 'hugo'"))
        (start-process-shell-command "hugo server" "*hugo server*" "cd ~/Dropbox/Sites/bluepencil/bluepencil-web && hugo server")
      nil)
    (browse-url "http://localhost:1313/")))

;;;; org-wc

(use-package org-wc
  :after org
  :defer 1)

;; org-wc-display is useful

;;;; emacs-benchmark

(use-package elisp-benchmarks :defer t)

;;; my-lisp

(use-package elfeed-setup
  :ensure nil
  :defer t
  :commands gr/elfeed-open-new-window)

(use-package misc-file-handling
  :ensure nil
  :defer t)

(use-package text-to-speech
  :ensure nil
  :defer t
  :commands hydra-mac-speak/body)

(use-package devonthink-dir
  :ensure nil
  :defer 1)

;;;; priv-lisp

(use-package dickinson
  :ensure nil
  :defer 1)

(use-package mu4e-setup
  :ensure nil
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

;;;; esup

(use-package esup
  :defer t
  :custom
  (esup-user-init-file (concat user-emacs-directory "init.el"))
  :config
  (setq esup-depth 0))

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
                  ("Article" (or (and (filename . "/Academic/*")
                                      (not (name . "magit")))))
                  ("Spring 2023" (or (and (filename . "/Spring 2023/*")
                                          (not (name . "magit")))))
                  ("Writing" (or (and (filename . "/Writings/*")
                                      (not (name . "magit")))))
                  ("Blue Oceans" (or (and (filename . "/Blue Oceans PR/*")
                                          (not (name . "magit")))))
                  ("ZK" (or (name . "*ZK")
                            (and (filename . "/Zettels/")
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
                             (name . "init.el")
                             (name . "^\\*Messages")
                             (name . "^\\*mu4e-")
                             (name . "^\\*calfw-calendar")
                             (name . "*Calculator*")
                             (name . "org_archive")
                             (name . "*davmail-server*")
                             (name . "gcal")
                             (name . ".persp")))
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
  :ensure nil
  :bind
  ("C-x C-j" . dired-jump)
  ("C-x d" . dired-jump)
  (:map dired-mode-map
        ("RET" . gr/dired-find-file-other-window)
        ("C-x C-q" . dired-toggle-read-only))
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  (dired-mode-hook . force-truncate-lines)
  :custom
  (dired-listing-switches "-algho --group-directories-first")
  (delete-by-moving-to-trash t)
  (dired-hide-details-mode t)
  (dired-free-space nil)
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)

  (defun gr/dired-find-file-other-window ()
    "In dired, open directories in same window, files in other window."
    (interactive)
    (let ((file (dired-get-file-for-visit)))
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
  ;; show backup and elc files
  (mapc
   (lambda (x) (delete x dired-omit-extensions))
   '("~" ".elc")))

;; to allow --group-directories-first to work on osx
(setq insert-directory-program "/usr/local/bin/gls")


;;;; avy

(use-package avy
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
  :hook
  (helpful-mode-hook . visual-line-mode)
  :config
  (with-eval-after-load 'semantic/symref/grep
    (add-to-list 'semantic-symref-filepattern-alist '(helpful-mode "*.el" "*.ede" ".emacs" "_emacs")))
  )

;;;; wgrep

(use-package wgrep :defer t)

;;;; web browsing

(use-package eww
  :bind
  (:map eww-mode-map
        ("o" . link-hint-open-link))
  :config
  (setq shr-inhibit-images nil)
  (setq eww-search-prefix "https://html.duckduckgo.com/html/?q=")
  (setq eww-download-directory (expand-file-name "~/Downloads")))

(use-package prot-eww
  :ensure nil
  :defer 1
  :config
  (setq prot-eww-save-history-file
        (locate-user-emacs-file "prot-eww-visited-history"))
  (setq prot-eww-save-visited-history t)
  (setq prot-eww-bookmark-link nil)
  (define-prefix-command 'prot-eww-map)
  (setq shr-folding-mode t
        shr-use-colors t
        shr-bullet "• ")
  :hook
  (prot-eww-history-mode-hook . hl-line-mode)
  :bind
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
     (let* ((word (if (use-region-p)
                      (buffer-substring
                       (region-beginning)
                       (region-end))
                    (thing-at-point 'word)))
            (text (read-string ,prompt nil nil word)))
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
    (message (format "browse-url set to `%s'" choice))))

(setq browse-url-generic-program "/usr/bin/open")
(setq browse-url-browser-function #'browse-url-default-browser)


;;;; pass

(use-package pass
  :defer t
  :after (embark consult)
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

;;(use-package posframe :defer 1)

(use-package ace-window
  :bind
  ("C-x o" . ace-window)
  :custom-face
  (aw-leading-char-face ((t (:family "Menlo" :foreground "red" :height 4.0))))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?l))
  (aw-scope 'visible)
  (aw-dispatch-always t)
  :config
  ;;  (ace-window-posframe-mode 1)
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
    (cond ((bound-and-true-p ivy-mode)
           (ivy-switch-buffer))
          ((bound-and-true-p ido-mode)
           (ido-switch-buffer))
          (t
           (call-interactively 'consult-buffer))))

  )

;;;; popper

(use-package popper
  :bind (("C-\\"   . popper-toggle-latest)
         ("M-\\"   . popper-cycle)
         ("C-M-\\" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        `("^magit\\:"
          "Output"
          ,(concat
            "\\*\\("
            (string-join
             '("chatgpt" "Messages" "Warnings" "Dictionary"
               "Outline" "Occur" "sdcv" "vterm"
               "xref" "Backtrace" "ZK-Index" "Apropos"
               "eshell" "PDF-Occur" "Org Agenda"
               "compilation" "elfeed-entry" "calfw-details"
               "Python" "grep" "undo-tree" "Async Shell Command"
               "Embark Collect" "Google Translate" "annotations"
               "Ilist" "Backups")
             "\\|") "\\)")
          dired-mode
          undo-tree-mode
          compilation-mode))

  (defun gr/popper-zk-index ()
    ;; doesn't really work
    (interactive)
    (cond ((zk-file-p)
           (pop-to-buffer zk-index-buffer-name))
          ((eq (current-buffer) (get-buffer "*ZK-Index*"))
           (delete-window))
          (t
           (popper-toggle-latest))))

  (defun gr/popper-select-buffer-at-bottom (buffer &optional alist)
    (let ((height (cond
                   ((or (string-match "magit.*\\|\\*ZK-Index\\*"
                                      (buffer-name buffer))
                        (with-current-buffer buffer
                          (derived-mode-p 'dired-mode)))
                    '(window-height . 0.45))
                   (t
                    '(window-height . 0.4)))))
      (select-window (display-buffer-at-bottom
                      buffer
                      (append alist
                              `(,height))))))

  (setq popper-display-control t)
  (setq popper-display-function #'gr/popper-select-buffer-at-bottom)

  (setq popper-echo-dispatch-keys '(?a ?s ?d ?f ?j ?k ?l))
  (setq popper-echo-dispatch-persist nil)

  (popper-mode 1))

;;;; expand-region

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;;; bookmark

(use-package bookmark
  :defer t
  :init
  (setq bookmark-bmenu-toggle-filenames nil
        bookmark-save-flag 1))

;;;; google-translate

(use-package google-translate
  :defer t
  :custom
  (google-translate-default-source-language "lt")
  (google-translate-default-target-language "en")
  (google-translate-backend-method 'curl)
  (google-translate-pop-up-buffer-set-focus t))

(with-eval-after-load 'google-translate-core-ui
  ;; override to prevent insertion of original text
  (define-advice google-translate--translating-text
      (:override (gtos format))
    " "))

(use-package google-translate-smooth-ui
  :ensure nil
  :defer t
  :custom
  (google-translate-translation-directions-alist
   '(("lt" . "en")
     ("en" . "lt"))))

;;;; outline-minor-mode

(use-package outline
  :ensure nil
  :diminish outline-minor-mode
  :bind
  ("C-S-<right>" . outline-demote)
  ("C-S-<left>" . outline-promote)
  ("C-<right>" . outline-demote)
  ("C-<left>" . outline-promote)
  (:map outline-minor-mode-cycle-map
        ("<backtab>" . outline-cycle-buffer)
        ("<left-margin> <mouse-1>" . nil)
        ("<left-margin> S-<mouse-1>" . nil)
        ("<right-margin> <mouse-1>" . nil)
        ("<right-margin> S-<mouse-1>" . nil))
  :hook
  (emacs-lisp-mode-hook . outline-minor-mode)
  (emacs-lisp-mode-hook . (lambda () (setq-local outline-regexp ";;;\\(;* [^ \t\n]\\)")))
  :custom
  (outline-minor-mode-highlight 'override)
  (outline-minor-mode-cycle t))

;;;; whitespace-mode

(use-package whitespace
  :defer 1
  :custom
  (whitespace-style '(face trailing lines)))

;;;; flycheck and package-lint

(use-package flycheck-package
  :defer 1
  :custom
  (flycheck-emacs-lisp-load-path 'inherit))

(use-package package-lint :defer t)

;;;; melpazoid

(use-package melpazoid
  :vc (:url "https://github.com/riscy/melpazoid"
            :main-file "melpazoid/melpazoid.el"
            :rev :newest)
  :defer t
  :commands gr/elisp-check-buffer
  :config
  (defun gr/elisp-check-buffer ()
    (interactive)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (package-refresh-contents)
    (if (ignore-errors (or flycheck-mode
                           flymake-mode))
        (progn
          (flycheck-mode -1)
          (flymake-mode -1)
          (message "Elisp checks off"))
      (progn
        (flycheck-mode)
        (flycheck-list-errors)
        (flymake-mode)
        (flymake-show-buffer-diagnostics)
        (package-lint-current-buffer)
        (melpazoid)))))

(add-hook 'flymake-mode-hook
          (lambda () (setq elisp-flymake-byte-compile-load-path load-path)))


;;;; accent

(use-package accent
  :bind
  (:map gr-map
        ("l" . accent-menu))
  :config
  (setq accent-diacritics '((a (ą á à))
                            (e (ė é è ë))
                            (i (į í))
                            (o (ó))
                            (u (ū ų ú ü))
                            (s (š))
                            (c (č))
                            (z (ž))
                            (A (Ą Á À))
                            (E (€ Ė É È))
                            (I (Į Í))
                            (O (Ó))
                            (U (Ū Ų Ú Ü))
                            (S (Š))
                            (C (Č))
                            (Z (Ž)))))

;;;; hide-cursor-mode

(defvar-local hide-cursor--original nil)

(define-minor-mode hide-cursor-mode
  "Hide or show the cursor.

    When the cursor is hidden `scroll-lock-mode' is enabled, so that
    the buffer works like a pager."
  :global nil
  (if hide-cursor-mode
      (progn
        (scroll-lock-mode 1)
        (setq-local hide-cursor--original
                    cursor-type)
        (setq-local cursor-type nil))
    (scroll-lock-mode -1)
    (setq-local cursor-type (or hide-cursor--original
                                t))))

(define-key global-map (kbd "<f7>") 'hide-cursor-mode)

(add-hook 'mu4e-view-mode-hook 'hide-cursor-mode)

;;;; aggressive-indent

(use-package aggressive-indent
  :diminish
  :hook (prog-mode-hook))

;;;; move-text

(use-package move-text
  :bind*
  ("C-<up>" . move-text-up)
  ("C-<down>" . move-text-down))

;;;; mastodon

(use-package mastodon
  :defer t
  :custom
  (mastodon-instance-url "https://zirk.us")
  (mastodon-active-user "grantrosson"))

;;;; golden-ratio-scroll-screen

(use-package golden-ratio-scroll-screen
  :defer 1
  :config
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))

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

;;;; simplenote

(use-package simplenote2
  :vc (:url "https://github.com/localauthor/simplenote2.el"
            :rev :newest)
  :defer t
  :bind
  (:map gr-map
        ("N" . simplenote2-open))
  :config
  (setq simplenote2-email "grantrosson@gmail.com")
  (setq simplenote2-password (string-trim-right (shell-command-to-string "security find-generic-password -a grantrosson@gmail.com -s simplenote -w")))
  (defun simplenote2-open ()
    (interactive)
    (simplenote2-setup)
    (simplenote2-list)))

;;;; chatgpt

(use-package chatgpt-shell
  :defer t
  :custom
  (chatgpt-shell-openai-key
   (lambda ()
     (auth-source-pick-first-password :host "api.openai.com")))
  :config
  (add-to-list 'chatgpt-shell-system-prompts '("Writing" . "You are a large language model and a writing assistant.")))

;;;; docsim

(use-package docsim
  ;; for finding similar notes, using docsim cli
  :vc (:url "https://github.com/hrs/docsim.el"
            :rev :newest)
  :defer t
  :after zk
  :commands (docsim-search
             docsim-search-buffer)
  :config
  (setq docsim-search-paths (list zk-directory))
  (setq docsim-get-title-function 'gr/docsim--get-title-function-zk)

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
           (files (mapcar 'car results)))
      (find-file
       (funcall zk-select-file-function
                "Similar Notes:"
                results
                'zk-docsim--group
                'identity))))


  (defun zk-docsim ()
    "Find notes similar to current buffer using docsim."
    (interactive)
    (gr/docsim-search (current-buffer)))
  )

;;; variable resets

(setq debug-on-error nil)
