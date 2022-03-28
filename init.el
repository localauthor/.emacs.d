;;; init.el                    -*- lexical-binding: t; -*-

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
;;(setq gc-cons-threshold (* 100 1000 1000))

(load-library "~/.emacs.d/lisp/gcmh.el")
(gcmh-mode 1)

(add-variable-watcher 'zk-index-desktop-directory (lambda (&rest x) (message "Variable changed: %S" x)))

(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; added to run native comp
(setq comp-speed 2)
(setq comp-deferred-compilation t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-repository-branch "develop")
(setq straight-host-usernames '((github . "localauthor")))

;; Replace use-package with straight-use-package
;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(setq use-package-hook-name-suffix nil)

;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))

;; Debug if there's an error during setup. Set to nil at end of init.el
(setq debug-on-error t)

(setq load-prefer-newer t)

;; set mode for *scratch* buffer
(setq initial-major-mode 'emacs-lisp-mode)
(setq initial-scratch-message nil)

(setq warning-suppress-types '(comp))

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
      '((eval setq-local zk-directory default-directory)
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

;;(straight-use-package '(org-plus-contrib :includes org))

;; the following sets the recipe used for org
;; anything in the use-package in myinit.org is subsequent to this
;; I specify ":files" to ensure oc-csl works properly

;; the recipe in straight gives an error, SSL certificate expired
;; couldn't figure out what that meant
;; setting the repo as below works

(straight-use-package '(org :repo "git://git.sv.gnu.org/emacs/org-mode.git"
                             :files (:defaults "lisp/*.el" ("etc/styles" "etc/styles/*") ("etc/csl/" "etc/csl/*"))))

(require 'org)

;; org-element wasn't being loaded on time, for some reason
(require 'org-element)

;; (if (version< emacs-version "28")
;;     (org-babel-load-file (expand-file-name "~/.emacs.d/lisp/myinitOSX13.org"))
;;   (org-babel-load-file (expand-file-name "~/Dropbox/org/myinit.org")))

;; (when (version< "28" emacs-version)
;;   (progn
;;     (defvar read-symbol-positions-list nil)
;;     (pixel-scroll-precision-mode)
;;     (setq mml-attach-file-at-the-end t)))


;;; Basics
;;;; Emacs

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/my-lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/priv-lisp"))

(setq elisp-flymake-byte-compile-load-path load-path)

(require 'priv-variables)

(use-package exec-path-from-shell
  :defer 1
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  ;; (setq exec-path-from-shell-warn-duration-millis 999)
  :config (exec-path-from-shell-initialize))

(use-package emacs
  :straight nil
  :bind
  ("C-c e" . eval-buffer)
  ("C-x e" . eval-last-sexp)
  ("C-x E" .  kmacro-end-and-call-macro)
  (:map Info-mode-map
        ("o" . link-hint-open-link))
  (:map help-mode-map
        ("o" . link-hint-open-link))
  :config
  (prefer-coding-system 'utf-8)
  (setq-default buffer-file-coding-system 'utf-8
                default-buffer-file-coding-system 'utf-8
                coding-system-for-read 'utf-8
                coding-system-for-write 'utf-8
                locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-language-environment 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)

  (setq make-backup-files nil) ;; stop creating backup~ files
  (setq auto-save-default nil) ;; stop creating #autosave# files
  (setq create-lockfiles nil)  ;; stop creating .# files

  (setq inhibit-splash-screen t)
  (desktop-save-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq scroll-bar-mode nil)

  (setq use-dialog-box nil)

  (setq recenter-positions '(top middle bottom))

  (add-hook 'after-make-frame-functions
            #'(lambda (frame)
                (modify-frame-parameters frame
                                         '((vertical-scroll-bars . nil)
                                           (horizontal-scroll-bars . nil)))))

  (global-auto-revert-mode t)

  (delete-selection-mode 1)
  (global-visual-line-mode 1)
  (global-hl-line-mode 0)

  (winner-mode 1)

  (transient-mark-mode 1)

  (setq minibuffer-follows-selected-frame nil)

  (setq tab-always-indent 't)
  (setq-default indent-tabs-mode nil)   ;; use spaces for tabs
  (setq sentence-end-double-space nil)

  (setq-default fill-column 77)

  (setq search-default-mode nil) ;; use literal strings in isearch, not regexps

  (setq ad-redefinition-action 'accept)
  (setq warning-suppress-types '((emacs) (comp) (:warning) comp))
  (setq ring-bell-function 'ignore)

  (setq vc-follow-symlinks t)
  (setq show-trailing-whitespace t)

  (add-to-list 'completion-ignored-extensions ".DS_Store")

  (setq custom-file "~/.emacs.d/custom.el")
  (setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

  (setq epg-gpg-program "/usr/local/bin/gpg")
  (setq xref-search-program 'ripgrep)

  (setq erc-server "irc.libera.chat"
        erc-nick "localauthor"
        erc-autojoin-channels-alist '(("#emacs" "#org-mode" "#systemcrafters")))

  ;; yes-or-no function

  (setq use-short-answers t) ;; new in emacs 28, replaces (fset 'yes-or-no-p 'y-or-n-p)

  (defun y-or-n-p-with-return (orig-func &rest args)
    "All RET as affirmative to y-or-n-p."
    (let ((query-replace-map (copy-keymap query-replace-map)))
      (define-key query-replace-map (kbd "RET") 'act)
      (define-key query-replace-map (kbd "<return>") 'act)
      (apply orig-func args)))

  (advice-add 'y-or-n-p :around #'y-or-n-p-with-return)


  ;; trash function

  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")

  (when (memq window-system '(mac ns))
    (defun nsystem-move-file-to-trash (path)
      "Moves file at PATH to  macOS Trash following `move-file-to-trash' convention.

      Relies on the command-line utility 'trash' to be installed.
      Get it from:  <http://hasseg.org/trash/>"
      (shell-command (concat "trash -vF \"" path "\""
                             "| sed -e 's/^/Trashed: /'")
                     nil ;; Name of output buffer
                     "*Trash Error Buffer*")))

  ;; after splitting, switch to new window
  ;; (global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
  ;; (global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

  ;; (global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

  )

(use-package tab-bar
  :init
  (tab-bar-mode 1)
  (tab-bar-history-mode)
  (setq tab-bar-show t
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-new-tab-to 'rightmost
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
  :bind
  ("s-{" . tab-bar-switch-to-prev-tab)
  ("s-}" . tab-bar-switch-to-next-tab)
  :config
  (set-face-attribute 'tab-bar nil
                      :font "Menlo" :height .75)
  (set-face-attribute 'tab-bar-tab nil
                      :background "grey75"
                      :box '(:line-width 1 :style released-button))
  (set-face-attribute 'tab-bar-tab-inactive nil :background "lightgrey"
                      :box '(:line-width 1 :style pressed-button)))

(use-package hydra
  :defer t)

;;;; Faces / Themes Setup

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(load-theme 'gr-light t)
;;(load-theme 'gr-dark t)

;; (progn
;;   (load-theme 'modus-vivendi t)
;;   (set-face-attribute 'show-paren-match nil :underline nil :foreground "#ffffff" :background "systemGreenColor"))

;; set region highlighting, per
;; https://github.com/DarwinAwardWinner/dotemacs#dont-use-ns_selection_fg_color-and-ns_selection_bg_color

(require 'frame)

(defun fix-mac-region-colors (&optional frame)
  "Set region face to a fixed color regardless of Mac Dark Mode.
On Mac OS, Dark Mode messes with the color of the region in weird
ways that makes it visually unusable. Instead, we set the region
color to a static color that matches the non-dark-mode region
color."
  (interactive)
  (with-selected-frame (or frame (selected-frame))
    (when (and (equal (face-attribute 'region :distant-foreground)
                      "ns_selection_fg_color")
               (equal (face-attribute 'region :background)
                      "ns_selection_bg_color"))
      (set-face-attribute
       'region nil
       :distant-foreground 'unspecified
       :background "#a9a9b8"))))

;; Fix for future frames
(add-hook 'after-make-frame-functions #'fix-mac-region-colors)

;; Fix for current frames
(mapc #'fix-mac-region-colors (frame-list))

(setq org-hide-emphasis-markers nil)

;; I set this here so that the down-arrow remains the right color and size
;; even if I change themes
(set-face-attribute 'org-ellipsis nil :inherit 'fixed-pitch :foreground "grey50" :underline nil :height 1.1)


;;;; MacOS Keybindings

;; MacOS Keyboard Shortcuts
(bind-keys*
 ("s-v" . yank)
 ("s-x" . kill-region)
 ("s-c" . kill-ring-save)
 ("s-s" . save-buffer)
 ("s-z" . undo)
 ("s-Z" . redo)
 ("s-q" . save-buffers-kill-emacs)
 ("s-f" . consult-line)
 ("s-w" . delete-frame)
 ("s-n" . gr/new-window))

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

(defun gr/initial-window-setup ()
  (interactive)
  (set-frame-size (selected-frame) 100 60)
  (set-frame-position (selected-frame) 100 20)
  (find-file "~/Dropbox/org/fragments.org")
  (split-window-below)
  (find-file-other-window "~/Dropbox/org/tasks.org"))

(setq initial-buffer-choice #'gr/initial-window-setup)

(if (daemonp)
    (add-hook 'window-setup-hook #'gr/initial-window-setup))

(setq initial-frame-alist '((width . 100) (height . 60) (left . 200) (top . 20) (menu-bar-lines . 0) (ns-appearance . dark)))

(setq default-frame-alist '((width . 80) (height . 35) (left . 100) (top . 100) (menu-bar-lines . 0) (ns-appearance . dark)))

(setq ns-use-proxy-icon nil)
(setq frame-title-format '("%b"))

;; if the whole window is 160 or more (char, not px), then a buffer will split to the right, instead of below;
(setq split-height-threshold nil)
(setq split-width-threshold 160)


;;;; display-buffer-alist

(setq display-buffer-alist
      '(

        ("magit:"
         (display-buffer-at-bottom)
         (window-height . 0.5)
         (side . bottom))

        ;; ("\\*Vertico"
        ;;  (display-buffer-in-side-window
        ;;   (window-height . 6)
        ;;   (side . bottom))
        ;;  (window-parameters (mode-line-format . none))
        ;;  )

        ("\\*PDF-Occur"
         (+select-buffer-in-side-window)
         (side . left)
         (window-width . 0.15)
         )

        ;; ("annots\\*$"
        ;;  (display-buffer-in-direction
        ;;   +select-buffer-in-direction)
        ;;  (direction . left)
        ;;  (window-width . 0.2)
        ;;  (inhibit-same-window . t))

        ;; ("\\*Contents"
        ;;  (display-buffer-in-side-window)
        ;;  (side . bottom)
        ;;  (slot . 0)
        ;;  (window-height . 0.1)
        ;;  (inhibit-same-window . t))

        ("\\*elfeed-entry"
         (display-buffer-at-bottom)
         (window-height . 0.6)
         (side . bottom))

        ("\\*Async"
         (display-buffer-at-bottom)
         (window-height . 0.3)
         (side . bottom))

        ("\\*Luhmann-Index"
         (display-buffer-at-bottom)
          (window-height . 0.4)
          (side . bottom))

        ("\\*ZK-Index"
         (display-buffer-at-bottom)
          (window-height . 0.4)
          (side . bottom))

        ("\\*Backups:"
         (display-buffer-at-bottom)
         (window-height . 0.3)
         (side . bottom))

        ("\\*helpful"
         (display-buffer-at-bottom)
         (window-height . 0.38)
         (side . bottom))

        ("\\*CAPTURE"
         (display-buffer-at-bottom)
         (window-height . 0.3)
         (side . bottom))

        ("\\*Pp Eval Output"
         (display-buffer-at-bottom)
         (window-height . 0.38)
         (side . bottom))

        ("\\*\\(eshell\\|Backtrace\\|Messages\\|Metahelp\\|Python\\|Org Agenda\\|Warnings\\|Go Translate\\|Google Translate\\|Org Select\\|Compile-Log\\|[Hh]elp\\|annotations\\|calfw-details\\)\\*"
         (display-buffer-at-bottom)
         (window-height . 0.38)
         (side . bottom))

        ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
         (display-buffer-at-bottom)
         (window-height . 0.38))
        )
      )


(defun my-switch-to-buffer-list (buffer alist)
  (select-window  (display-buffer-use-some-window buffer alist)))


(defun +select-buffer-in-side-window (buffer alist)
  "Display buffer in a side window and select it"
  (let ((window (display-buffer-in-side-window buffer alist)))
    (select-window window)
    ))

(defun +select-buffer-at-bottom (buffer alist)
  "Display buffer in a side window and select it"
  (let ((window (display-buffer-at-bottom buffer alist)))
    (select-window window)
    ))

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
           ("C-." . execute-extended-command)
           ("/" . switch-to-minibuffer-window)
           ("C-/" . exit-minibuffer)

           ("i" . gr/open-init-file)
           ("I" . open-user-init-file)

           ("C-t" . gr/open-tasks-file)
           ("t" . gr/open-tasks-upcoming-agenda-other-frame)

           ("C-f" . gr/open-fragments-file)
           ("f" . gr/open-fragments-file-other-frame)

           ("m" . gr/open-mu4e)
           ("M" . mu4e)

           ("a" . gr/org-agenda)
           ("c" . gr/calfw-open-org-calendar)

           ("b" . consult-bookmark)

           ("j" . gr/org-journal-new-entry)

           ("e" . gr/elfeed-open-new-window)
           ("C-e" . gr/elfeed-open)

           ("W" . gr/word-count-subtree)

           ("l" . gr/symbol-menu/body)

           ;;("T" . gr/google-translate-lt-en)
           ("T" . google-translate-smooth-translate)
           ;; ("T" . go-translate-popup-current)

           ;;("p" . hydra-persp/body)
           ("n" . hydra-annotate/body)
           ("s" . hydra-mac-speak/body)
           ("L" . toggle-truncate-lines)
           ("D" . gr/lookup-word-at-point)
           ("d" . sdcv-search)
           ("P" . password-store-copy-field)
           ;; ("C-c" . 'flyspell-popup-correct)
           ("C-c" . 'flyspell-auto-correct-previous-word))

;;;;; gr/symbol-menu

(defhydra gr/symbol-menu (:hint nil :color blue)
  "
Symbols and Diacritics
    _a_: ą   _e_: ė   _u_: ū   _U_: ų  _E_: €
    _s_: š   _c_: č   _z_: ž   _i_: į
    "
  ("q" nil)
  ("a" (insert "ą"))
  ("e" (insert "ė"))
  ("u" (insert "ū"))
  ("U" (insert "ų"))
  ("s" (insert "š"))
  ("c" (insert "č"))
  ("z" (insert "ž"))
  ("i" (insert "į"))
  ("E" (insert "€"))
)


;;;; link-hint

(use-package link-hint
  :defer t
  :custom
  (link-hint-message nil))

(use-package link-hint-aw-select
  :straight nil
  :defer t
  :bind
  (:map gr-map
        ("o" . link-hint-aw-select)))

;;;; diminish

(use-package diminish
  :defer t
  :config
  (eval-after-load 'org-indent '(diminish 'org-indent-mode))
  (diminish 'gcmh-mode)
  (diminish 'visual-line-mode)
  (diminish 'buffer-face-mode)
  (diminish 'outline-minor-mode)
  (diminish 'abbrev-mode)
  (eval-after-load 'gumshoe-mode '(diminish 'global-gumshoe-mode))
  )

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

(use-package org
  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  ("C-c C" . org-clock-goto)
  (:map org-mode-map
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
        ("" . org-cycle-agenda-files))
  :mode (("\\.org$" . org-mode)
         ;;("\\.md$" . org-mode)
         )
  :init
  (with-eval-after-load "org"
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("n" . "notes")))

  ;; open org-links in same window; allows link-hint--aw-select-org-link to work properly
  (with-eval-after-load "org"
    (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file))

  ;; :hook
  ;; (org-mode-hook . variable-pitch-mode)
  :config
  (unbind-key "C-," org-mode-map)
  (unbind-key "C-'" org-mode-map)

  :custom-face
  (org-drawer ((t (:foreground "gray92" :height .7))))
  (org-special-keyword ((t (:foreground "gray50" :height .7))))
  :custom
  (org-ellipsis " ▼") ;◣ ▼ ▽ ► ➽
  (default-major-mode 'org-mode)
  (org-directory "~/Dropbox/org")
  (org-use-speed-commands t)
  (org-speed-commands-user '(("k" . ignore) ("=" . ignore) ("o" . ignore)))
  (org-startup-indented t)
  (org-catch-invisible-edits 'smart)
  (org-tags-column -77)
  (org-tag-alist '(("noexport") ("noheadline")))
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)
  (org-hide-leading-stars nil)
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
  (org-src-presrve-indentation nil)
  (org-log-states-order-reversed nil)
  )

(defun gr/org-src-block-auto-indent ()
  (interactive)
  (when (org-in-src-block-p)
    (org-edit-special)
    (indent-region (point-min) (point-max))
    (org-edit-src-exit)))

;;(define-key org-mode-map (kbd "C-i") #'gr/org-src-block-auto-indent)

;; moves the pointer off the item marker after converting a heading to an
;; item, because I found it annoying having to move the pointer over in order
;; to begin typing; also just learned how to advise functions, instead of
;; trying to rewrite the function, which I tried before in this case and
;; found too difficult to pursue

;; (advice-add #'org-toggle-item :after #'org-end-of-line)

;;(require 'org-habit)
;;(add-to-list 'org-modules 'ol-habit)

(defun gr/copy-file-path ()
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (let ((filepath (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filepath
      (kill-new filepath)
      (message "Copied buffer file path '%s' to the clipboard." filepath))))


(defun gr/copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(use-package org-agenda-setup
  :straight nil
  :after org
  defer t)

(use-package org-capture-setup
  :straight nil
  :after org
  defer t)

(use-package org-gcal-setup
  :straight nil)

;;;; org move item/heading up/do

;; Distinguishes between item and heading

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (if (and (derived-mode-p 'org-mode)
           (org-at-heading-p))
      (call-interactively #'org-move-subtree-up)
    (when (derived-mode-p 'prog-mode 'text-mode)
      (progn (transpose-lines 1)
             (forward-line -2)
             (indent-according-to-mode))
      )))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (if (and (derived-mode-p 'org-mode)
           (org-at-heading-p))
      (call-interactively #'org-move-subtree-down)
    (when (derived-mode-p 'prog-mode 'text-mode)
      (progn (forward-line 1)
             (transpose-lines 1)
             (forward-line -1)
             (indent-according-to-mode))
      )))


;;;; org narrow/widen

;; Simplified/smart narrow-widen, bound to `C-x n`; call prefix argument `C-u` to narrow further

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
  Dwim means: region, org-src-block, org-subtree, or
  defun, whichever applies first. Narrowing to
  org-src-block actually calls `org-edit-src-code'.

  With prefix P, don't widen, just narrow even if buffer
  is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;;(define-key endless/toggle-map "n" #'narrow-or-widen-dwim)
;; This line actually replaces Emacs' entire narrowing
;; keymap, that's how much I like this command. Only
;; copy it if that's what you want.

(define-key ctl-x-map "n" #'narrow-or-widen-dwim)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (define-key LaTeX-mode-map "\C-xn"
              nil)))
;;(global-set-key (kbd "C-x n") 'narrow-or-widen-dwim)
(global-set-key (kbd "C-c n") 'narrow-or-widen-dwim)


;;;; org-refile

;; for Capture and Refile; to enable finding and selecting headings when refiling a capture;

(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)
                           ;;                             ("~/Dropbox/org/tasks.org_archive" :maxlevel . 9)
                           ;;                             ("~/Dropbox/org/myinit.org_archive" :maxlevel . 9)
                           ))

(setq org-outline-path-complete-in-steps nil)          ; Refile in a single go
(setq org-refile-use-outline-path 'file)               ; Show full paths for refiling
(setq org-refile-allow-creating-parent-nodes 'confirm) ;

;; Search and Jump to Any Heading in Org-Refile-Targets files

;; SOURCE: https://yiming.dev/blog/2018/03/02/my-org-refile-workflow/

;; NOTE: If a heading isn't found, either the file is not listed in the variable 'org-refile-targets', or the filelist cache needs to be updated ('org-refile-get-targets')

(defun +org-search ()
  (interactive)
  (org-refile '(4)))

;; The org-refile can be slow if it reloads the cache every time, so enable caching

;;  (setq org-refile-use-cache t)

;; And set the cache to reload when emacs is idle for five minutes

;; (run-with-idle-timer 300 t (lambda ()
;;                             (org-refile-cache-clear)
;;                             (org-refile-get-targets)))

;; This means that the headings listed in the cache will not always be up to date, up to the minute
;; If something is missing, call the function 'org-refile-get-targets'


;;;; org-archive

(defun gr/org-archive (arg)
  "Without C-u: mark heading DONE and archive to datetree.
With C-u: archive subtree to same hierarchy as in original file."
  (interactive "P")
  (cond ((equal arg '(4)) (gr/org-archive-subtree-hierarchy))
        (t (gr/org-mark-done-and-archive-datetree))))

(setq org-archive-location "%s_archive::")

(defun gr/org-mark-done-and-archive-datetree ()
  (interactive)
  (let ((org-archive-location "%s_archive::datetree/"))
    (org-todo 'done)
    (org-archive-subtree)))

(define-key org-mode-map "\C-c\$" 'gr/org-mark-done-and-archive-datetree)

;; Archive subtrees under the same hierarchy as the original org file.
;; Link: https://gist.github.com/Fuco1/e86fb5e0a5bb71ceafccedb5ca22fcfb
;; Link: https://github.com/daviderestivo/galactic-emacs/blob/master/lisp/org-archive-subtree.el

(defun gr/org-archive-subtree-hierarchy (&optional find-done)
  "Move the current subtree to the archive.
The archive can be a certain top-level heading in the current
file, or in a different file. The tree will be moved to that
location, the subtree heading be marked DONE, and the current
time will be added.

When called with a single prefix argument FIND-DONE, find whole
trees without any open TODO items and archive them (after getting
confirmation from the user). When called with a double prefix
argument, find whole trees with timestamps before today and
archive them (after getting confirmation from the user). If the
cursor is not at a headline when these commands are called, try
all level 1 trees. If the cursor is on a headline, only try the
direct children of this heading."
  (interactive "P")
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
                    'region-start-level 'region))
            org-loop-over-headlines-in-active-region)
        (org-map-entries
         `(progn (setq org-map-continue-from (progn (org-back-to-heading) (point)))
                 (org-archive-subtree ,find-done))
         org-loop-over-headlines-in-active-region
         cl (if (org-invisible-p) (org-end-of-subtree nil t))))
    (cond
     ((equal find-done '(4))  (org-archive-all-done))
     ((equal find-done '(16)) (org-archive-all-old))
     (t
      ;; Save all relevant TODO keyword-related variables.
      (let* ((tr-org-todo-keywords-1 org-todo-keywords-1)
             (tr-org-todo-kwd-alist org-todo-kwd-alist)
             (tr-org-done-keywords org-done-keywords)
             (tr-org-todo-regexp org-todo-regexp)
             (tr-org-todo-line-regexp org-todo-line-regexp)
             (tr-org-odd-levels-only org-odd-levels-only)
             (this-buffer (current-buffer))
             (time (format-time-string
                    (substring (cdr org-time-stamp-formats) 1 -1)))
             (file (abbreviate-file-name
                    (or (buffer-file-name (buffer-base-buffer))
                        (error "No file associated to buffer"))))
             (location (org-archive--compute-location
                        (or (org-entry-get nil "ARCHIVE" 'inherit)
                            org-archive-location)))
             (afile (car location))
             (heading (cdr location))
             (infile-p (equal file (abbreviate-file-name (or afile ""))))
             (newfile-p (and (org-string-nw-p afile)
                             (not (file-exists-p afile))))
             (buffer (cond ((not (org-string-nw-p afile)) this-buffer)
                           ((find-buffer-visiting afile))
                           ((find-file-noselect afile))
                           (t (error "Cannot access file \"%s\"" afile))))
             (org-odd-levels-only
              (if (local-variable-p 'org-odd-levels-only (current-buffer))
                  org-odd-levels-only
                tr-org-odd-levels-only))
             level datetree-date datetree-subheading-p)
        (when (string-match "\\`datetree/\\(\\**\\)" heading)
          ;; "datetree/" corresponds to 3 levels of headings.
          (let ((nsub (length (match-string 1 heading))))
            (setq heading (concat (make-string
                                   (+ (if org-odd-levels-only 5 3)
                                      (* (org-level-increment) nsub))
                                   ?*)
                                  (substring heading (match-end 0))))
            (setq datetree-subheading-p (> nsub 0)))
          (setq datetree-date (org-date-to-gregorian
                               (or (org-entry-get nil "CLOSED" t) time))))
        (if (and (> (length heading) 0)
                 (string-match "^\\*+" heading))
            (setq level (match-end 0))
          (setq heading nil level 0))
        (save-excursion
          (org-back-to-heading t)
          ;; Get context information that will be lost by moving the
          ;; tree.  See `org-archive-save-context-info'.
          (let* ((all-tags (org-get-tags))
                 (local-tags
                  (cl-remove-if (lambda (tag)
                                  (get-text-property 0 'inherited tag))
                                all-tags))
                 (inherited-tags
                  (cl-remove-if-not (lambda (tag)
                                      (get-text-property 0 'inherited tag))
                                    all-tags))
                 (context
                  `((category . ,(org-get-category nil 'force-refresh))
                    (file . ,file)
                    (itags . ,(mapconcat #'identity inherited-tags " "))
                    (ltags . ,(mapconcat #'identity local-tags " "))
                    (olpath . ,(mapconcat #'identity
                                          (org-get-outline-path)
                                          "/"))
                    (time . ,time)
                    (todo . ,(org-entry-get (point) "TODO")))))
            ;; We first only copy, in case something goes wrong
            ;; we need to protect `this-command', to avoid kill-region sets it,
            ;; which would lead to duplication of subtrees
            (let (this-command) (org-copy-subtree 1 nil t))
            (set-buffer buffer)
            ;; Enforce Org mode for the archive buffer
            (if (not (derived-mode-p 'org-mode))
                ;; Force the mode for future visits.
                (let ((org-insert-mode-line-in-empty-file t)
                      (org-inhibit-startup t))
                  (call-interactively 'org-mode)))
            (when (and newfile-p org-archive-file-header-format)
              (goto-char (point-max))
              (insert (format org-archive-file-header-format
                              (buffer-file-name this-buffer))))
            (when datetree-date
              (require 'org-datetree)
              (org-datetree-find-date-create datetree-date)
              (org-narrow-to-subtree))
            ;; Force the TODO keywords of the original buffer
            (let ((org-todo-line-regexp tr-org-todo-line-regexp)
                  (org-todo-keywords-1 tr-org-todo-keywords-1)
                  (org-todo-kwd-alist tr-org-todo-kwd-alist)
                  (org-done-keywords tr-org-done-keywords)
                  (org-todo-regexp tr-org-todo-regexp)
                  (org-todo-line-regexp tr-org-todo-line-regexp))
              (goto-char (point-min))
              (org-show-all '(headings blocks))
              (if (and heading (not (and datetree-date (not datetree-subheading-p))))
                  (progn
                    (if (re-search-forward
                         (concat "^" (regexp-quote heading)
                                 "\\([ \t]+:\\(" org-tag-re ":\\)+\\)?[ \t]*$")
                         nil t)
                        (goto-char (match-end 0))
                      ;; Heading not found, just insert it at the end
                      (goto-char (point-max))
                      (or (bolp) (insert "\n"))
                      ;; datetrees don't need too much spacing
                      (insert (if datetree-date "" "\n") heading "\n")
                      (end-of-line 0))
                    ;; Make the subtree visible
                    (outline-show-subtree)
                    (if org-archive-reversed-order
                        (progn
                          (org-back-to-heading t)
                          (outline-next-heading))
                      (org-end-of-subtree t))
                    (skip-chars-backward " \t\r\n")
                    (and (looking-at "[ \t\r\n]*")
                         ;; datetree archives don't need so much spacing.
                         (replace-match (if datetree-date "\n" "\n\n"))))
                ;; No specific heading, just go to end of file, or to the
                ;; beginning, depending on `org-archive-reversed-order'.
                (if org-archive-reversed-order
                    (progn
                      (goto-char (point-min))
                      (unless (org-at-heading-p) (outline-next-heading)))
                  (goto-char (point-max))
                  ;; Subtree narrowing can let the buffer end on
                  ;; a headline.  `org-paste-subtree' then deletes it.
                  ;; To prevent this, make sure visible part of buffer
                  ;; always terminates on a new line, while limiting
                  ;; number of blank lines in a date tree.
                  (unless (and datetree-date (bolp)) (insert "\n"))))
              ;; Paste
              (org-paste-subtree (org-get-valid-level level (and heading 1)))
              ;; Shall we append inherited tags?
              (and inherited-tags
                   (or (and (eq org-archive-subtree-add-inherited-tags 'infile)
                            infile-p)
                       (eq org-archive-subtree-add-inherited-tags t))
                   (org-set-tags all-tags))
              ;; Mark the entry as done
              (when (and org-archive-mark-done
                         (let ((case-fold-search nil))
                           (looking-at org-todo-line-regexp))
                         (or (not (match-end 2))
                             (not (member (match-string 2) org-done-keywords))))
                (let (org-log-done org-todo-log-states)
                  (org-todo
                   (car (or (member org-archive-mark-done org-done-keywords)
                            org-done-keywords)))))

              ;; Add the context info.
              (dolist (item org-archive-save-context-info)
                (let ((value (cdr (assq item context))))
                  (when (org-string-nw-p value)
                    (org-entry-put
                     (point)
                     (concat "ARCHIVE_" (upcase (symbol-name item)))
                     value))))
              ;; Save the buffer, if it is not the same buffer and
              ;; depending on `org-archive-subtree-save-file-p'.
              (unless (eq this-buffer buffer)
                (when (or (eq org-archive-subtree-save-file-p t)
                          (eq org-archive-subtree-save-file-p
                              (if (boundp 'org-archive-from-agenda)
                                  'from-agenda
                                'from-org)))
                  (save-buffer)))
              (widen))))
        ;; Here we are back in the original buffer.  Everything seems
        ;; to have worked.  So now run hooks, cut the tree and finish
        ;; up.
        (run-hooks 'org-archive-hook)
        (let (this-command) (org-cut-subtree))
        (when (featurep 'org-inlinetask)
          (org-inlinetask-remove-END-maybe))
        (setq org-markers-to-move nil)
        (when org-provide-todo-statistics
          (save-excursion
            ;; Go to parent, even if no children exist.
            (org-up-heading-safe)
            ;; Update cookie of parent.
            (org-update-statistics-cookies nil)))
        (message "Subtree archived %s"
                 (if (eq this-buffer buffer)
                     (concat "under heading: " heading)
                   (concat "in file: " (abbreviate-file-name afile)))))))
    (org-reveal)
    (if (looking-at "^[ \t]*$")
        (outline-next-visible-heading 1))))

(use-package dash :defer t)

(defadvice gr/org-archive-subtree-hierarchy (around fix-hierarchy activate)
  (let* ((fix-archive-p (and (not current-prefix-arg)
                             (not (use-region-p))))
         (afile  (car (org-archive--compute-location
                       (or (org-entry-get nil "ARCHIVE" 'inherit) org-archive-location))))
         (buffer (or (find-buffer-visiting afile) (find-file-noselect afile))))
    ad-do-it
    (when fix-archive-p
      (with-current-buffer buffer
        (goto-char (point-max))
        (while (org-up-heading-safe))
        (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
               (path (and olpath (split-string olpath "/")))
               (level 1)
               tree-text)
          (when olpath
            (org-mark-subtree)
            (setq tree-text (buffer-substring (region-beginning) (region-end)))
            (let (this-command) (org-cut-subtree))
            (goto-char (point-min))
            (save-restriction
              (widen)
              (-each path
                (lambda (heading)
                  (if (re-search-forward
                       (rx-to-string
                        `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
                      (org-narrow-to-subtree)
                    (goto-char (point-max))
                    (unless (looking-at "^")
                      (insert "\n"))
                    (insert (make-string level ?*)
                            " "
                            heading
                            "\n"))
                  (cl-incf level)))
              (widen)
              (org-end-of-subtree t t)
              (org-paste-subtree level tree-text))))))))


;;;; org-journal

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
  (org-journal-find-file 'find-file)
  )


;;;; org-superstar

(use-package org-superstar
  :disabled
  :defer t
  :after org
  :init
  :hook (org-mode-hook . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list  '("◉" "○" "▪" "◦" "•" "▫" "•" "▫"))
  (setq org-superstar-item-bullet-alist
        '((?+ . ?◦)
          (?* . ?➤)
          (?- . ?–)))
  )


;;;; org-transclusion

(use-package org-transclusion
  :straight (org-transclusion :host github :repo "nobiot/org-transclusion")
  )


;;;; org-contrib

(use-package org-contrib
  :straight (org-contrib :files ("lisp/org-contrib.el" "lisp/ox-extra.el"))
  :defer t
  :commands (org-export-ignore-headlines)
  :config
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines))

  ;; change "ignore" tag to "noheadline"
  (defun org-export-ignore-headlines (data backend info)
    "Remove headlines tagged \"ignore\" retaining contents and promoting children.
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
  (vertico-count 7)
  )

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
  :demand t
  :bind
  ("C-," . embark-act)
  ("C->" . embark-act-noquit)
  ("C-<" . embark-act-all)
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
  )

  ;; from https://karthinks.com/software/fifteen-ways-to-use-embark/
  (eval-when-compile
    (defmacro embark-aw-select (fn)
      `(defun ,(intern (concat "embark-aw-select-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))

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

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

;;;; consult

(use-package consult
  :demand t
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
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (list (kbd "M-[")
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
    (unless (bound-and-true-p consult--preview-function) ;; Disable preview for Consult commands
      (save-selected-window
        (let ((embark-quit-after-action))
          (embark-dwim)))))

  ;; persp-source consult-buffer

  (defvar persp-source
    `(:name "Persp"
            :narrow ?p
            :hidden t
            :category buffer
            :state ,#'consult--buffer-state
            :items ,(lambda () (mapcar #'buffer-name (persp-buffer-list-restricted)))))
;;  (add-to-list 'consult-buffer-sources 'persp-source 'append)


  ;; consult-clock-in

  ;; (setq org-clock-persist t)
  ;; (with-eval-after-load 'org
  ;;   (org-clock-persistence-insinuate))

  ;; (defun consult-clock-in (&optional match scope resolve)
  ;;   "Clock into an Org heading."
  ;;   (interactive (list nil nil current-prefix-arg))
  ;;   (require 'org-clock)
  ;;   (org-clock-load)
  ;;   (let ((list (org-element-map (org-element-parse-buffer 'headline) 'headline
  ;;                 (lambda (hl) (org-element-property :duration hl)))))
  ;;     (message (format "list: %s" list)))
  ;;   (save-window-excursion
  ;;     (consult-org-heading
  ;;      match
  ;;      (or scope
  ;;          (thread-last org-clock-history
  ;;                       (mapcar 'marker-buffer)
  ;;                       (mapcar 'buffer-file-name)
  ;;                       (delete-dups)
  ;;                       (delq nil))
  ;;          (user-error "No recent clocked tasks")))
  ;;     (org-clock-in nil (when resolve
  ;;                         (org-resolve-clocks)
  ;;                         (org-read-date t t)))))

  ;; (consult-customize consult-clock-in
  ;;                    :prompt "Clock in: "
  ;;                    :preview-key (kbd "M-.")
  ;;                    :group
  ;;                    (lambda (cand transform)
  ;;                      (if transform
  ;;                          (substring cand (next-single-property-change 0 'consult-org--buffer cand))
  ;;                        (let ((m (car (get-text-property 0 'consult-org--heading cand))))
  ;;                          (progn
  ;;                            (message (format "list: %s" list))
  ;;                            (cond ((member m list) "Recent")
  ;;                                  (t (buffer-name (marker-buffer m)))))))))

  ;; let ((parsetree (org-element-parse-buffer 'headline)))

  ;; (org-element-map (org-element-parse-buffer 'headline) 'headline
  ;;   (lambda (hl) (org-element-property :duration hl))))


  ;; set consult-buffer sources to hidden, except consult--source-buffer

  ;; (with-eval-after-load "consult"
  ;;   (dolist (src consult-buffer-sources)
  ;;     (unless (eq src consult--source-buffer)
  ;;         (set src (plist-put (symbol-value src) :hidden t)))))


  ;; add action to consult-outline to narrow selected heading and expand it

  ;; (defun my/consult-outline-narrow-heading (heading)
  ;;   "Narrow to and expand HEADING."
  ;;   (embark-consult-goto-location heading)
  ;;   (outline-mark-subtree)
  ;;   (and
  ;;    (use-region-p)
  ;;    (narrow-to-region (region-beginning) (region-end))
  ;;    (deactivate-mark))
  ;;   (outline-show-subtree))

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
           (cons '(consult-location . embark-consult-outline-map) embark-keymap-alist)))
      (apply fn args)))

  (advice-add 'consult-outline :around #'with-embark-consult-outline-map)

  ;; map TAB to select for consult-completing-read-multiple

  (defun consult-vertico--crm-select ()
    "Select/deselect candidate."
    (interactive)
    (when (let ((cand (vertico--candidate)))
            (and (vertico--match-p cand) (not (equal cand ""))))
      (vertico-exit)))

  (defun consult-vertico--crm-exit ()
    "Select/deselect candidate and exit."
    (interactive)
    (when (let ((cand (vertico--candidate)))
            (and (vertico--match-p cand) (not (equal cand ""))))
      (run-at-time 0 nil #'exit-minibuffer))
    (vertico-exit))

  (define-key consult-crm-map [remap vertico-insert] #'consult-vertico--crm-select)
  (define-key consult-crm-map [remap exit-minibuffer] #'consult-vertico--crm-exit)

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
  )

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
  (setq completion-styles '(orderless partial-completion initials)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion initials))))))

;;;; savehist

(use-package savehist
  :init
  (savehist-mode 1)
  :config
  (setq savehist-additional-variables '(citar-history search-ring regexp-search-ring))
  )


;;;; corfu / cape

(use-package corfu
  ;; annoyingly finds zk-link after completion
  :disabled
  :init (corfu-global-mode 1)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay .7)
  (corfu-auto-prefix 1)
  (corfu-quit-no-match .5)
  (corfu-quit-at-boundary nil)
  (corfu-preview-current nil)
  (corfu-commit-predicate nil)
  :config
  (set-face-attribute 'corfu-default nil
                      :background "cornsilk"
                      :font "Menlo")
  (set-face-attribute 'corfu-current nil
                      :background "light blue"))

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; (setq tab-always-indent 'complete)

(use-package cape
  :disabled
  :init
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)))


;;;; company

(use-package company
  :defer t
  :diminish
  :init
  (global-company-mode 1)
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
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-tooltip-limit 12)
  (company-idle-delay 1)
  (company-echo-delay 0)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case 'keep-prefix)
  (company-sort-prefer-same-case-prefix t)
  (company-format-margin-function nil)
  (company-dabbrev-other-buffers t)
  ;;(company-transformers '(company-sort-by-occurrence))
  :config
  (setq company-backends '(company-capf company-bbdb company-files (company-elisp company-dabbrev-code company-gtags company-etags company-keywords) company-dabbrev company-yasnippet))
  (setq company-files-exclusions '(".git/" ".DS_Store"))
  )

(use-package company-posframe
  :diminish
  :bind
  (:map company-posframe-active-map
        ([mouse-1] . company-abort)
        ([?\e] . company-abort))
  :init
  (company-posframe-mode 1)
  :custom
  (company-posframe-show-indicator t)
  (company-posframe-show-metadata nil)
  (company-posframe-quickhelp-delay nil)
  )

;; kills company buffer after completion, to prevent it from showing up when new window is created, like elfeed or mu4e
(add-hook 'company-after-completion-hook (lambda (arg) (when (get-buffer " *company-posframe-buffer*")
(kill-buffer " *company-posframe-buffer*"))))

;;this function is supposed to sort all completions with uppercase to the end; doesn't work so far
(defun my-sort-uppercase (candidates)
  (let (case-fold-search
        (re "\\`[[:upper:]]*\\'"))
    (sort candidates
          (lambda (s1 s2)
            (and (string-match-p re s2)
                 (not (string-match-p re s1)))))))

;; (push 'my-sort-uppercase company-transformers)

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
;; (defvar company-mode/enable-yas t
;;   "Enable yasnippet for all backends.")

;; (defun company-mode/backend-with-yas (backend)
;;   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
;;       backend
;;     (append (if (consp backend) backend (list backend))
;;             '(:with company-yasnippet))))

;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))


;;;; prescient / company-prescient

(use-package prescient
  :config
  (prescient-persist-mode 1)
  :custom
  (prescient-aggressive-file-save t))

(use-package company-prescient
  :config
  ;;(company-prescient-mode 1)
  )

;;; Packages
;;;; magit

(use-package magit
  :bind
  ("C-c m" . magit-status)
  :custom-face
  (diff-refine-added ((t (:background "yellow" :foreground "red"))))
  :custom
  (magit-diff-refine-hunk t))

(setq-default mode-line-format
              (delete '(vc-mode vc-mode) mode-line-format))


;;;; git-gutter

(use-package git-gutter
  :init
  :diminish (git-gutter-mode)
  :hook
  (emacs-lisp-mode-hook . git-gutter-mode)
  (org-mode-hook . git-gutter-mode)
  :config
  (set-face-foreground 'git-gutter:modified "orange")
  (set-face-foreground 'git-gutter:added    "forestgreen")
  (set-face-foreground 'git-gutter:deleted  "red")
  (set-fringe-mode '(8 . 0))
  )


;;;; ace-link

(use-package ace-link
  :disabled
  :defer t
  :init
  (ace-link-setup-default)
  )

;;;; esup

(use-package esup
  :defer t
  :config
  (setq esup-depth 0)
  )

;;;; re-builder

(use-package re-builder
  :init
  (setq reb-re-syntax 'string)
  )

;;;; ibuffer

(use-package ibuffer
  :straight (:type built-in)
  :bind
  (:map ibuffer-mode-map
        ("TAB". ibuffer-toggle-filter-group))
  :hook
  (ibuffer-hook . gr/truncate-lines)
  (ibuffer-hook . gr/ibuffer-set-filter-group)
  :custom
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-auto-mode t))

;; remaps C-x b to
;;(defalias 'list-buffers 'ibuffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package ibuffer-sidebar
  :bind ("C-x C-u" . ibuffer-sidebar-toggle-sidebar)
  :hook
  (ibuffer-sidebar-mode-hook . gr/truncate-lines)
  (ibuffer-sidebar-mode-hook . gr/ibuffer-set-filter-group)
  :commands (ibuffer-sidebar-toggle-sidebar))

(defun gr/ibuffer-set-filter-group ()
  (ibuffer-switch-to-saved-filter-groups "default")
  (setq ibuffer-hidden-filter-groups (list "***" "ORG" "ZK" "el"))
  (ibuffer-update nil t)
  )

(use-package ibuffer-vc
  :defer t
  :config

  (defun gr/ibuffer-vc-run ()
    "Set up `ibuffer-vc."
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'recency)
      (ibuffer-do-sort-by-recency)))

  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))

     (t (format "%8d" (buffer-size)))))
  )

(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 60 60 :left)
              " "
              (size-h 9 -1 :right)
              " "
                                        ; (mode 9 -1 :left :elide)
                                        ;" "
              ;;              (vc-status 16 16 :left)
              ;;              " "
              vc-relative-file)
        (mark " "
              (name 16 -1)
              " " filename)))

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

(defun gr/truncate-lines ()
  (interactive)
  (if (bound-and-true-p truncate-lines)
      ()
    (toggle-truncate-lines)))

(defun force-truncate-lines ()
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
  :diminish
  )


;;;; avy

(use-package avy
  :defer t
  :bind
  ("M-g w" . avy-goto-word-1)
  :bind*
  ;;("C-l" . gr/avy-goto)
  ("C-. C-," . gr/avy-goto-string)
  ("C-'" . gr/avy-goto-string)
  :custom
  (avy-timeout-seconds 0.4)
  :config
  (setq avy-keys '(?g ?d ?k ?l ?r ?u ?e ?i ?w ?p ?f ?s))

  (setq avy-dispatch-alist '((?, . avy-action-embark)
                             (?j . avy-action-aw-select)
                             (?2 . avy-action-split-below)
                             (?n . avy-action-open-in-new-frame)
                             (?  . avy-action-mark)
                             (?c . avy-action-copy)
                             (?x . avy-action-kill-stay)
                             (?y . avy-action-yank)
                             (?$ . avy-action-ispell)
                             (?z . avy-action-zap-to-char)
                             (?h . avy-action-helpful)
                             ;;(?  . avy-action-mark-to-char)
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
      (switch-to-buffer-other-frame new-buffer))
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
  :defer t
  :init
  (global-undo-tree-mode))

;;;; python

(use-package python-mode
  :disabled
  :defer t
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

(defun eww-wiki (text)
  "Function used to search Wikipedia for the given text."
  (interactive (list (read-string "Wiki for: ")))
  (eww (format "https://en.m.wikipedia.org/wiki/Special:Search?search=%s"
               (url-encode-url text))))

(defun eww-duckduckgo (text)
  "Function used to search DuckDuckGo for the given text."
  (interactive (list (read-string "Search for: ")))
  (eww (format "https://duckduckgo.com/?q=%s"
               (url-encode-url text))))

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

(use-package ctable)

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
  ;; :init
  ;; (progn
  ;;   (global-set-key [remap other-window] 'ace-window))
  :bind
  ;; ([remap other-window] . ace-window)
  ("C-x o" . ace-window)
  :custom-face
  (aw-leading-char-face ((t (:family "Menlo" :foreground "red" :height 4.0))))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
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
          (?x aw-delete-window "Delete Window")

          ;;(?F aw-split-window-fair "Split Fair Window")
          ;;(?m aw-move-window "Move Curr. to Targ.")
          ;;(?n aw-flip-window "Flip Window")
          ;;(?J aw-switch-buffer-other-window "Select Buffer in Targ.")
          ;;(?o delete-other-windows "Delete Other Windows")a
          ;;(?T aw-transpose-frame "Transpose Frame")
          ;; ?i ?r ?t are used by hyperbole.el
          ;;(?e aw-execute-command-other-window "Execute Command Other Window")

          (?? aw-show-dispatch-help)))
  )


;;;; popper

(use-package popper
  :bind (("C-\\"   . popper-toggle-latest)
         ("M-\\"   . popper-cycle)
         ("C-M-\\" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("^\\*Messages\\*"
          "^\\*Warnings\\*"
          "^\\*Backtrace\\*"
          "^\\*ZK-Index\\*"
          "^\\*ZK-Desktop"
          "^\\*Luhmann-Index\\*"
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
          "^magit\\:"
          "Output"
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
  (setq bookmark-bmenu-toggle-filenames nil)
  (setq bookmark-save-flag 1)
  :custom-face
  (bookmark-face ((t nil))))

(use-package bookmark+
  :disabled
  :straight (bookmark-plus :host github :repo "emacsmirror/bookmark-plus"))


;;;; transient

(use-package transient
  :straight (:type built-in)
  :init
  (require 'transient))


;;;; recentf

(use-package recentf
  :defer 1
  :init
  (recentf-mode))


;;;; dogears

(use-package dogears
  :defer t
  :init
  (dogears-mode)
  :bind
  ("M-s-{" . dogears-back)
  ("M-s-}" . dogears-forward)
  :custom
  (dogears-ignore-modes '(fundamental-mode
                          dogears-list-mode
                          mu4e-main-mode
                          mu4e-view-mode
                          mu4e-compose-mode
                          mu4e-headers-mode
                          mu4e-loading-mode
                          elfeed-show-mode
                          elfeed-view-mode))
  )

(use-package fringe-helper)

(fringe-helper-define 'bookmark-fringe-mark nil
  "xx.xx..."
  ".xx.xx.."
  "..xx.xx."
  "...xx.xx"
  "..xx.xx."
  ".xx.xx.."
  "xx.xx...")

(defvar consult--source-dogears
  (list :name     "Dogears"
        :narrow   ?d
        :category 'dogears
        :items    (lambda ()
                    (mapcar
                     (lambda (place)
                       (propertize (dogears--format-record place)
                                   'consult--candidate place))
                     dogears-list))
        :action   (lambda (cand)
                    (dogears-go (get-text-property 0 'consult--candidate cand)))))

(defun consult-dogears ()
  (interactive)
  (consult--multi '(consult--source-dogears)))


;;;; emacs-everywhere

(use-package emacs-everywhere
  :straight (emacs-everywhere :host github :repo "tecosaur/emacs-everywhere"))


;;;; go-translate

  (use-package go-translate
    :disabled
    :bind
    ;;("C-c t" . go-translate)
    ("C-c T" . go-translate-popup-current)
    :config
    (setq go-translate-local-language "en")
    (setq go-translate-target-language "lt")
    (setq go-translate-token-current (cons 430675 2721866130))
    (setq go-translate-extra-directions '(("lt" . "en") ("fr" . "en")))
    :custom
    (go-translate-buffer-follow-p t)       ; focus the result window
    (go-translate-buffer-source-fold-p t)   ; fold the source text in the result window
    (go-translate-auto-guess-direction 'nil))


(use-package go-translate
  :disabled
  :bind
  ("C-c T" . gts-do-translate)
  :config
  (setq gts-translate-list '(("lt" "en") ("fr" "en")))

  (setq gts-default-translator
        (gts-translator

         :picker ; used to pick source text, from, to. choose one.

         ;;(gts-noprompt-picker)
         ;;(gts-noprompt-picker :texter (gts-whole-buffer-texter))
         ;;(gts-prompt-picker)
         ;;(gts-prompt-picker :single t)
         (gts-prompt-picker :texter (gts-current-or-selection-texter) :single t)

         :engines ; engines, one or more. Provide a parser to give different output.

         (list
          ;;(gts-bing-cn-engine)
          ;;(gts-google-engine)
          ;;(gts-google-rpc-engine)
          ;;(gts-deepl-engine :auth-key [YOUR_AUTH_KEY] :pro nil)
          ;;(gts-google-engine :parser (gts-google-summary-parser))
          (gts-google-engine :parser (gts-google-parser))
          ;;(gts-google-rpc-engine :parser (gts-google-rpc-summary-parser))
          ;;(gts-google-rpc-engine :parser (gts-google-rpc-parser))
          )

         :render ; render, only one, used to consumer the output result. Install posframe yourself when use gts-posframe-xxx

         ;;(gts-buffer-render)
         (gts-posframe-pop-render)
         ;;(gts-posframe-pop-render :backcolor "#333333" :forecolor "#ffffff")
         ;;(gts-posframe-pin-render)
         ;;(gts-posframe-pin-render :position (cons 1200 20))
         ;;(gts-posframe-pin-render :width 80 :height 25 :position (cons 1000 20) :forecolor "#ffffff" :backcolor "#111111")
         ;;(gts-kill-ring-render)
         ))
  )


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
  ;; :hook
  ;; (emacs-lisp-mode-hook . enable-paredit-mode)
  ;; (eval-expression-minibuffer-setup-hook . enable-paredit-mode)
  ;; (ielm-mode-hook . enable-paredit-mode)
  ;; (lisp-mode-hook . enable-paredit-mode)
  ;; (lisp-interaction-mode-hook . enable-paredit-mode)
  )


;;;; golden-ratio-scroll-screen

(use-package golden-ratio-scroll-screen
  :defer t
  :config
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))


;;;; explain-pause-mode

(use-package explain-pause-mode
  :defer t
  :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
  :diminish)

;;;; outshine-mode

(use-package outshine
  :diminish
  :bind
  ("C-S-<right>" . outline-demote)
  ("C-S-<left>" . outline-promote)
  :hook
  (emacs-lisp-mode-hook . outshine-mode)
  (outline-minor-mode-hook . outshine-mode))

;;;; whitespace-mode

(use-package whitespace
  ;; :hook
  ;; (emacs-lisp-mode-hook . whitespace-mode)
  :custom
  (whitespace-style '(face trailing lines))
  )

;;;; flycheck and package-lint

(use-package flycheck-package
  :hook (emacs-lisp-mode-hook . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit))

(use-package package-lint
  :defer t)


;;;; visual-fill-column

(use-package visual-fill-column
  ;;only used in zk, as dir-local
  ;;because it doesn't work with git-gutter
  :custom
  (visual-fill-column-width 90))

;;; Citation / Bibliography
;;;; citar

(defvar gr/bibliography '("~/Dropbox/gr-bibliography.bib"))

(use-package citar
  :straight (:host github :repo "bdarcus/citar" :fork t)
  :after (oc misc-file-handling)
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
        ("e" . citar-ebib-jump-to-entry)
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
  (citar-file-extensions '("pdf" "epub"))
  (citar-file-note-extensions '("org" "md"))
  (citar-file-open-function 'citar-file-open-external)
  (citar-file-additional-files-separator " ")
  (citar-file-open-prompt t)
  (citar-citeproc-csl-styles-dir "~/.csl")
  (citar-citeproc-csl-locales-dir "~/.csl/locales")
  (citar-format-reference-function 'citar-citeproc-format-reference)
  (citar-citeproc-csl-style "chicago-fullnote-bibliography-short-title-subsequent.csl")
  (citar-display-transform-functions '((t . citar-clean-string)))
  (citar-open-note-function 'gr/citar-zk-open-note)
  ;; (citar-templates
  ;;  '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
  ;;    (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
  ;;    (preview . "${author editor} (${year issued date}) ${title}, ${journal publisher container-title collection-title}.\n")
  ;;    (note . "${=key=} - ${title} (${year})")))
  :init
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

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
        (if (y-or-n-p "No note associated - create one?")
            (let* ((template (citar-get-template 'note))
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
           (files (citar-file--files-for-multiple-entries refs citar-library-paths '("pdf")))
           (string (read-string "Search string: ")))
      (pdf-occur-search files string t)))

  (defun gr/citar-file--make-filename-regexp (keys extensions &optional additional-sep)
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
   (if keys (regexp-opt keys "[0-9]\\{12\\}?.*\\(?1:") ".*?\\(?1:[a-z]+[0-9]\\{4\\}[a-z]?\\)")
   (when additional-sep (concat "\\(?3:" additional-sep "[^z-a]*\\)?"))
   "\\."
   (if extensions (regexp-opt extensions "\\(?2:") "\\(?2:[^.]*\\)")
   "\\'"))

  (advice-add 'citar-file--make-filename-regexp :override 'gr/citar-file--make-filename-regexp)

  )

;;;; org-cite

(use-package oc
  :straight nil
  :init
  ;; added to make org-cite work
  (require 'oc-csl)
  (setq org-cite-csl-styles-dir "~/.csl"
        org-cite-csl-locales-dir "~/.csl/locales"
        org-odt-preferred-output-format "docx"
        org-odt-styles-file "~/Dropbox/Academic Writings/template.ott"
        org-cite-global-bibliography gr/bibliography)
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        org-cite-export-processors '((t csl "~/.csl/chicago-fullnote-bibliography-short-title-subsequent.csl")))
  )

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
  :commands (ebib-open)
  :bind
  (:map ebib-index-mode-map
        ("?" . (lambda () (interactive) (embark-bindings-in-keymap ebib-index-mode-map)))
        ("h" . hydra-ebib/body)
        ("k" . nil)
        ("D" . ebib-delete-entry)
        ("I" . ebib-zotero-import-identifier)
        ("d" . nil)
        ("c" . ebib-filters-cancel-filter)
        ("z" . nil)
        ("s" . ebib-filter-any)
        ("O" . ebib-filters-apply-filter)
        ("o" . ebib-citar-open)
        ("q" . ebib-smart-quit)
        )
  (:map ebib-entry-mode-map
        ("?" . (lambda ()
                 (interactive)
                 (embark-bindings-in-keymap ebib-entry-mode-map)))
        ("h" . hydra-ebib/body)
        ("d" . nil)
        ("e" . ebib-edit-current-field)
        ("O" . ebib-filters-apply-filter)
        ("o" . ebib-citar-open)
        ("q" . ebib-quit-entry-buffer)
        ("k" . nil))
  :hook
  (ebib-index-mode-hook . (lambda () (gr/truncate-lines)))
  :custom
  (ebib-preload-bib-files gr/bibliography)
  (ebib-autogenerate-keys t)
  (ebib-uniquify-keys nil)
  ;; (ebib-index-default-sort '("timestamp" . descend))
  (ebib-use-timestamp t)
  (ebib-index-columns '(("Author/Editor" 40 t)
                        ("Entry Key" 15 t)
                        ;;("Year" 6 t)
                        ("Title" 50 t))))

  (defhydra hydra-ebib (:hint nil :color blue)
    "
  _j_: Jump to Entry   _k_: Add Keyword    _!_: Auto-Citekey     _s_: DOI Lookup
  _O_: Apply Filter                        _E_: Edit Citekey     _S_: ISBN Lookup
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
    ("o" ebib-citar-open)
    ("C" ebib-filters-cancel-filter)
    ;; ("s" ebib-save-current-database)
    ("I" ebib-zotero-import-identifier)
    ("S" ebib-isbn-search)
    ("s" crossref-lookup)
    ("q" nil))

(use-package ebib-extras
  :straight nil
  :defer t
  :config
  (require 'ebib-zotero))

;;;; biblio / sci-hub

(use-package scihub
  :straight (:host github :repo "emacs-pe/scihub.el"))

(use-package biblio
  :defer t
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

  (setq biblio-selection-mode-actions-alist
        '(("Copy DOI" . gr/biblio--copy-doi-ext)
          ("Grab from Sci-Hub" . gr/biblio--get-from-sci-hub-ext)
          ("Find open access copy on Dissemin" . biblio-dissemin--lookup-record)))

  )

(use-package ebib-biblio
  :straight nil
  :after (ebib biblio)
  :bind (:map biblio-selection-mode-map
              ("e" . ebib-biblio-selection-import)))

;;;; pullbib

(use-package pullbib
  :disabled
  :straight (pullbib :host github :repo "publicimageltd/pullbib" :fork t)
  :config
  (setq pullbib-url-map '(("http://127.0.0.1:23119/better-bibtex/export/library?/1/library.bibtex" . "~/Dropbox/gr-bibliography.bib"))))

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
  :straight nil)

;;; Writing

;;;; zk

(use-package zk
  :straight (zk :local-repo "~/.emacs.d/my-lisp/zk/"
                :files (:defaults "zk-consult.el"))
  ;; :init
  ;; (require 'zk-consult)
  ;; (require 'zk-extras)
  :bind
  (:map zk-id-map
        ("s" . zk-search)
        ("r" . zk-consult-grep)
        ("o" . link-hint--aw-select-zk-link))
  :hook (completion-at-point-functions . zk-completion-at-point)
  :custom
  (zk-directory "~/Dropbox/ZK/Zettels")
  (zk-file-extension "md")
  (zk-default-backlink "201801190001")
  (zk-link-and-title 'ask)
  (zk-new-note-link-insert 'ask)
  (zk-link-and-title-format "%t [[%i]]")
  (zk-tag-grep-function #'zk-consult-grep-tag-search)
  (zk-grep-function #'zk-grep) ;; #'zk-consult-grep
  (zk-current-notes-function nil)
  (zk-select-file-function 'zk-consult-select-file)
  (zk-consult-preview-functions
   '(zk-find-file
     zk-find-file-by-full-text-search
     zk-current-notes
     zk-unlinked-notes))
  :config
  (zk-setup-auto-link-buttons)
  (zk-setup-embark)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . outline-mode))
  (add-to-list 'embark-become-keymaps 'embark-become-zk-file-map)
  (add-to-list 'consult-buffer-sources 'zk-consult-source 'append))

(use-package zk-index
  :straight (zk-index :local-repo "~/.emacs.d/my-lisp/zk/"
                      :files ("zk-index.el"))
  :bind (:map zk-index-map
              ("g" . consult-line)
              ("?" . hydra-zk-index/body))
  :config
  (zk-index-setup-embark)
  :custom
  (zk-index-desktop-directory zk-directory))

(use-package zk-luhmann
  :straight (zk-luhmann :local-repo "~/.emacs.d/my-lisp/zk-luhmann")
  :bind (:map zk-index-map
              ("L" . zk-luhmann-index-sort)
              ("l" . zk-luhmann-index)
              ("C-f" . zk-luhmann-index-forward)
              ("C-b" . zk-luhmann-index-back)
              ("C-t" . zk-luhmann-index-unfold)
              ("t" . zk-luhmann-index-top)
              ("1" . zk-luhmann-index-level)
              ("2" . zk-luhmann-index-level)
              ("3" . zk-luhmann-index-level)
              ("4" . zk-luhmann-index-level)
              ("5" . zk-luhmann-index-level))
  :hook (completion-at-point-functions . zk-luhmann-completion-at-point)
  :custom
  (zk-luhmann-id-prefix "{")
  (zk-luhmann-id-postfix " }")
  :config
  (setq zk-luhmann-id-regexp (concat zk-luhmann-id-prefix
                                    "\\([0-9a-zA-Z,]*\\)"
                                    zk-luhmann-id-postfix)))

(use-package zk-consult
  :straight nil)

(use-package zk-extras
  :straight nil)

(use-package zk-link-hint
  :straight nil)

(with-eval-after-load 'link-hint-aw-select
  (link-hint-define-type 'zk-link
    :aw-select #'link-hint--aw-select-zk-link)
  (defun link-hint--aw-select-zk-link (id)
    (with-demoted-errors "%s"
      (if (> (length (aw-window-list)) 1)
          (let ((window (aw-select nil))
                (buffer (current-buffer))
                (new-buffer))
            (zk-follow-link-at-point id)
            (setq new-buffer
                  (current-buffer))
            (switch-to-buffer buffer)
            (aw-switch-to-window window)
            (switch-to-buffer new-buffer))
        (link-hint-open-link-at-point)))))

(embark-define-keymap embark-become-zk-file-map
  "Keymap for Embark zk-file actions."
  :parent embark-meta-map
  ("f" zk-find-file)
  ("g" consult-grep)
  ("s" zk-find-file-by-full-text-search))

(eval-and-compile
  (defhydra hydra-zk (:hint nil
                            :color blue)
    "
  _h h_: Inbox      _i_: Insert Link   _N_: New Note       _d_: dir ripgrep
  _h s_: Strct Nts  _c_: Insert Cite   _R_: Rename Note    _r_: zk grep
  _h i_: Index      _f_: Find File     _o_: Open Link      _e_: ebib
                  _b_: Backlinks     _C_: Current Notes  _B_: Biblio.biz "
    ("h h" (lambda () (interactive) (zk-find-file-by-id "201801190001")))
    ("h i" (lambda () (interactive) (zk-find-file-by-id "201801180001")))
    ("h s" (lambda () (interactive) (zk-find-file-by-id "201801180002")))
    ("N" zk-new-note)
    ("R" zk-rename-note)
    ("i" zk-insert-link)
    ("e" hydra-ebib/body)
    ("B" hydra-bib/body)
    ;;("B b" gr/append-bibliography)
    ;;("B r" citar-insert-reference)
    ;;("B p" pullbib-pull)
    ("I" zk-index)
    ("l" zk-luhmann-index)
    ("L" zk-lit-notes)
    ("c" gr/citar-mmd-insert-citation)
    ("C" zk-current-notes)
    ("o" link-hint-aw-select)
    ("b" zk-network)
    ("f" zk-find-file)
    ("F" zk-find-file-by-full-text-search)
    ("r" zk-consult-grep)
    ("s" zk-search)
    ("d" zk-index-send-to-desktop)
    ;; ("d" gr/consult-ripgrep-select-dir)
    ("p" devonthink-dir-find-file)
    ("q" nil)))

(bind-key* (kbd "C-z") 'hydra-zk/body)
(bind-key* (kbd "M-z") 'hydra-zk/body)

(eval-and-compile
  (defhydra hydra-bib (:hint nil
                            :color blue)
    "
       _r_: Insert Ref          _e_: ebib              _d_: DOI Lookup
       _b_: Insert Bib          _I_: Auto Import       _i_: ISBN Look up"

    ("b" gr/append-bibliography)
    ("r" citar-insert-reference)
    ("e" hydra-ebib/body)
    ("I" ebib-zotero-import-identifier)
    ("i" ebib-isbn-search)
    ("d" crossref-lookup)
    ("c" gr/citar-mmd-insert-citation)
    ("q" nil)))

(eval-and-compile
  (defhydra hydra-zk-index ()
    ("a" zk-index-refresh "all")
    ("l" zk-luhmann-index "luhmann")
    ("c" zk-core-index "core")
    ("n" zk-non-luhmann-index "non-Luhmann")
    ("L" zk-lit-notes-index "lit")
    ("e" zk-ed-index "ed")))

(defun zk-org-try-to-follow-link (fn &optional arg)
  "When 'org-open-at-point' FN fails, try 'zk-follow-link-at-point'.
Optional ARG."
  (let ((org-link-search-must-match-exact-headline t))
    (condition-case nil
	(apply fn arg)
      (error (zk-follow-link-at-point)))))

(advice-add 'org-open-at-point :around #'zk-org-try-to-follow-link)

;;;; sdcv-mode

(use-package sdcv-mode
  :straight (sdcv-mode :host github :repo "gucong/emacs-sdcv"))

;;;; ispell / flyspell

(use-package ispell
  :defer 3
  :config
  (setq ispell-program-name "/usr/local/bin/hunspell")
  (setq ispell-local-dictionary "en_US")
  )

(use-package flyspell
  :defer 3
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
  :defer t
  :diminish (yas-minor-mode)
  :init
  (yas-global-mode 1)
  (yas-reload-all)
  )

(use-package yasnippet-multiple-key
  :defer t
  :straight (yasnippet-multiple-key :host github :repo "ShuguangSun/yasnippet-multiple-key"))


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
    (if (equal "html" origext)
        (async-shell-command (format "inliner '%s' > '%s'" orig newfile))
      (error "Must select .html file"))))


;;;; pdf-tools

(use-package pdf-tools
  :straight (pdf-tools :host github :repo "vedang/pdf-tools" :fork "localauthor/pdf-tools")
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
  (pdf-view-mode-hook . pdf-keynav-minor-mode)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights")

  :config
  (pdf-tools-install 'no-query)
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



;;;; Org-Sidebar

(use-package org-sidebar
  :disabled
  :defer t
  :bind
  ("C-c o" . org-sidebar-tree-toggle)
  ;;("C-c k" . org-sidebar-toggle)
  (:map org-sidebar-tree-map
        ("q" . bury-buffer))
  :custom
  (setq org-sidebar-side 'right)
  )

;;;; Indirect Buffer -> Split Outline

;;Functions for Using a Split Outline in Org-Mode

;;Problems:
;;- doesn't truncate long lines, so tagged lines wrap
;;- isn't as good as org-sidebar

(defun split-and-indirect-orgtree ()
  "Split window to the right and open an org tree section in it."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (org-tree-to-indirect-buffer)
  (windmove-right))

(defun kill-and-unsplit-orgtree ()
  "Kill the cloned buffer and delete the window."
  (interactive)
  (kill-this-buffer)
  (delete-window))

;;Not for split outline view specifically, but useful generally
(defun close-and-kill-next-pane ()
  "When multiple windows, close other pane and kill its buffer."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(define-key org-mode-map (kbd "C-c o") #'split-and-indirect-orgtree)
(define-key org-mode-map (kbd "C-c b")  #'kill-and-unsplit-orgtree)

;;;; "Better Return" edited

;; a better return; inserts list item with RET instead of M-RET

(defun scimax/org-return (&optional ignore)
  "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
  (interactive "P")
  (if ignore
      (org-return)
    (cond

     ((eq 'line-break (car (org-element-context)))
      (org-return t))

     ;; Open links like usual, unless point is at the end of a line.
     ;; and if at beginning of line, just press enter.
     ((or (and (eq 'link (car (org-element-context))) (not (eolp)))
          (bolp))
      (org-return))

     ;; checkboxes - add new or delete empty
     ((org-at-item-checkbox-p)
      (cond
       ;; at the end of a line.
       ((and (eolp)
             (not (eq 'item (car (org-element-context)))))
        (org-insert-todo-heading nil))
       ;; no content, delete
       ((and (eolp) (eq 'item (car (org-element-context))))
        (setf (buffer-substring (line-beginning-position) (point)) ""))
       ((eq 'paragraph (car (org-element-context)))
        (goto-char (org-element-property :end (org-element-context)))
        (org-insert-todo-heading nil))
       (t
        (org-return))))

     ;; lists end with two blank lines, so we need to make sure we are also not
     ;; at the beginning of a line to avoid a loop where a new entry gets
     ;; created with only one blank line.
     ((org-in-item-p)
      (cond
       ;; empty definition list
       ((and (looking-at " ::")
             (looking-back "- " 3))
        (beginning-of-line)
        (delete-region (line-beginning-position) (line-end-position)))
       ;; empty item
       ((and (looking-at "$")
             (or
              (looking-back "- " 3)
              (looking-back "+ " 3)
              (looking-back " \\* " 3)))
        (beginning-of-line)
        (delete-region (line-beginning-position) (line-end-position)))
       ;; numbered list
       ((and (looking-at "$")
             (looking-back "[0-9]+. " (line-beginning-position)))
        (beginning-of-line)
        (delete-region (line-beginning-position) (line-end-position)))
       ;; insert new item
       (t
        (if (not (looking-at "$"))
            (org-return)
          (end-of-line)
          (org-insert-item)))))

     ;; org-heading
     ((org-at-heading-p)
      (if (not (string= "" (org-element-property :title (org-element-context))))
          (if (not (looking-at "$"))
              (org-return)
            (progn
              ;; Go to end of subtree suggested by Pablo GG on Disqus post.
              ;;(org-end-of-subtree)
              (org-meta-return)
              ;;(org-metaright)
              ;;(org-insert-heading-respect-content)
              (outline-show-entry)
              ))
        ;; The heading was empty, so we delete it
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")))

     ;; tables
     ((org-at-table-p)
      (if (-any?
           (lambda (x) (not (string= "" x)))
           (nth
            (- (org-table-current-dline) 1)
            (remove 'hline (org-table-to-lisp))))
          (org-return)
        ;; empty row
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")
        (org-return)))

     ;; fall-through case
     (t
      (org-return)))))

(define-key org-mode-map (kbd "RET") 'scimax/org-return)


;;;; ox-hugo

(use-package ox-hugo
  :defer 3
  :after org)

(defun gr/blog-deploy-localauthor ()
  (interactive)
  (shell-command "cd ~/Dropbox/Writings/localauthor & ./deploy.sh")
  )

(defun gr/blog-test-localauthor ()
  (interactive)
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (if
        (equal 1 (shell-command "pgrep 'hugo'"))
        (start-process-shell-command "hugo server" "*hugo server*" "cd ~/Dropbox/Writings/localauthor && hugo server")
      nil)
    (browse-url "http://localhost:1313/")))


;;;; capitalize, upcase, downcase dwim

(defun ct/word-boundary-at-point-or-region (&optional callback)
  "Return the boundary (beginning and end) of the word at point, or region, if any.
Forwards the points to CALLBACK as (CALLBACK p1 p2), if present.

URL: https://christiantietze.de/posts/2021/03/change-case-of-word-at-point/"
  (let ((deactivate-mark nil)
        $p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning)
              $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alpha:]")
        (setq $p1 (point))
        (skip-chars-forward "[:alpha:]")
        (setq $p2 (point))))
    (when callback
      (funcall callback $p1 $p2))
    (list $p1 $p2)))

;; (defun ct/capitalize-word-at-point ()
;;   (interactive)
;;   (ct/word-boundary-at-point-or-region #'upcase-initials-region))

(defun ct/downcase-word-at-point ()
  (interactive)
  (ct/word-boundary-at-point-or-region #'downcase-region))
(defun ct/upcase-word-at-point ()
  (interactive)
  (ct/word-boundary-at-point-or-region #'upcase-region))

(defun ct/capitalize-region (p1 p2)
  (downcase-region p1 p2)
  (upcase-initials-region p1 p2))
(defun ct/capitalize-word-at-point ()
  (interactive)
  (ct/word-boundary-at-point-or-region #'ct/capitalize-region))

;; Set global shortcuts
(global-set-key (kbd "M-c") #'ct/capitalize-word-at-point)
(global-set-key (kbd "M-u") #'ct/upcase-word-at-point)
(global-set-key (kbd "M-l") #'ct/downcase-word-at-point)

;;;; annotate.el

(use-package annotate
  ;; :hook (after-save-hook . annotate-save-annotations)
  :custom
  (annotate-summary-ask-query nil)
  (annotate-annotation-position-policy :by-length)
  )

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
    ))

;;;; convert docx to org

(defun gr/flush-properties-drawers ()
  (interactive)
  (goto-line 2)
  (flush-lines ":PROPERTIES:")
  (flush-lines ":CUSTOM_ID:")
  (flush-lines ":END:")
  )

(defun gr/convert-pandoc-docx-org ()
  "Use pandoc via shell command to convert a docx file to an org file.
Navigate to files in dired, mark files, and execute command."
  (interactive)
  (dired-do-async-shell-command
   "pandoc -f docx -t org --wrap=none" current-prefix-arg
   (dired-get-marked-files t current-prefix-arg))
  (switch-to-buffer-other-window "*Async Shell Command*")
  (run-with-idle-timer 1 nil
                       'gr/flush-properties-drawers)
  (goto-line 2)
  (run-with-idle-timer 1 nil
                       'gr/flush-properties-drawers)
  )

(defun gr/clear-empty-org-headers ()
  (interactive)
  (goto-line 2)
  (replace-string "
  ,* " " ")
  (goto-line 2)
  (replace-string "
  ,** " " ")
  (goto-line 2)
  (replace-string "
  ,*** " " ")
  )

;;;; org-wc

(use-package org-wc)

;; org-wc-display is useful


;;;; markdown mode

;; (use-package markdown-mode)


;;; my-lisp

(use-package elfeed-setup
  :straight nil
  :defer t)

(use-package misc-file-handling
  :straight nil
  :defer t)

(use-package text-to-speech
  :straight nil
  :defer t)

(use-package devonthink-dir
  :straight nil
  :defer t)

;; priv-lisp

(use-package dickinson
  :straight nil
  :defer t)

(use-package mu4e-setup
  :straight nil)

;;; variable resets

;;(setq gc-cons-threshold (* 2 1000 1000))

(setq debug-on-error nil)

;; (defun org-babel-tangle-config ()
;;   (interactive)
;;   (when (string-equal (buffer-file-name)
;;                       (expand-file-name "~/Dropbox/org/myinit.org"))
;;     (let ((org-config-babel-evaluate nil))
;;       (org-babel-tangle nil "~/Dropbox/org/myinit.el"))))

;; (add-hook 'org-mode-hook
;;        (lambda ()
;;          (add-hook 'after-save-hook #'org-babel-tangle-config)))
