;;; early-init.el -*- lexical-binding: t -*-

;; where path gets set for emacs:

;; 1) emacs called from zsh (eg, in iterm) path comes from ~/.zshenv

;; 2) when emacsclient is called from skhd through emacs-start, path comes from skhd, which is set in ~/Library/LaunchAgents/com.koekeishiya.skhd.plist

;; - (getenv "PATH") is set at start-up; this is the path in make-process calls
;; - eshell uses the var exec-path
;; - changing the exec-path does NOT effect (getenv "PATH")


(setq gc-cons-percentage 0.8
      gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq  gc-cons-percentage .2
                   gc-cons-threshold (* 50 1024 1024)))) ;; 50MB

(setopt auto-save-default nil)
(auto-save-mode -1)

(when (fboundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))

(dolist (dir '("lisp" "my-lisp" "my-lisp/priv-lisp"))
  (let ((default-directory (expand-file-name
                            (concat user-emacs-directory dir))))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

(defun gr/recompile-lisp-dirs ()
  (interactive)
  (dolist (dir '("lisp" "my-lisp" "/my-lisp/priv-lisp"))
    (let ((exp-dir (expand-file-name (concat user-emacs-directory dir))))
      (byte-recompile-directory exp-dir 0 nil t))))

(setopt load-prefer-newer t
        inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        inhibit-startup-buffer-menu t
        inhibit-splash-screen t
        inhibit-compacting-font-caches t
        frame-inhibit-implied-resize t
        auto-mode-case-fold nil
        bidi-inhibit-bpa t)

(setq warning-suppress-types '((emacs) (bytecomp comp))
      native-comp-async-report-warnings-errors 'silent
      warning-minimum-level ':error
      ring-bell-function 'ignore
      byte-compile-warnings '(not obsolete cl-functions))

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

;;; speedup

(setopt use-package-always-ensure t
        use-package-always-defer t
        use-package-vc-prefer-newest t
        use-package-enable-imenu-support t
        ;; use-package-expand-minimally t
        ;; use-package-compute-statistics t
        use-package-hook-name-suffix nil)

;;; package.el

(use-package package
  :custom
  (package-enable-at-startup t)
  (package-check-signature nil)
  (package-quickstart t)
  (package-install-upgrade-built-in nil)
  (package-native-compile t)
  (package-review-policy nil)
  (package-review-diff-command '("git" "diff" "--no-index" "--color=never" "--diff-filter=d"))
  :config
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  (add-to-list ' package-archives
                 '("org" . "https://orgmode.org/elpa/")))

(use-package package-vc
  :demand t
  :custom
  (package-vc-register-as-project nil)
  (package-vc-allow-build-commands t))

;;; frame

(setq frame-resize-pixelwise t
      frame-title-format "%b")

(setq default-frame-alist '((width . 90)
                            (height . 50)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (undecorated . t)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (left-fringe . 8)
                            (right-fringe . 8)))

(menu-bar-mode -1)

(setq tool-bar-mode nil
      scroll-bar-mode nil
      menu-bar-mode nil)
