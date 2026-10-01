;;; early-init.el -*- lexical-binding: t -*-

;; where path gets set for emacs:

;; 1) emacs called from zsh (eg, in iterm) path comes from ~/.zshenv

;; 2) when emacsclient is called from skhd through emacs-start, path comes from skhd, which is set in ~/Library/LaunchAgents/com.koekeishiya.skhd.plist

;; - (getenv "PATH") is set at start-up; this is the path in make-process calls
;; - eshell uses the var exec-path
;; - changing the exec-path does NOT effect (getenv "PATH")

;; 3) emacs.app opens from launcher, dock, etc. should get PATH from Info.plist, but doesn’t right now; no idea why
;; known issue: https://github.com/d12frosted/homebrew-emacs-plus/issues/720

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

;; (mapcar
;;  (lambda (x)
;;    (delq x load-path ))
;;  '("/Users/grantrosson/.emacs.d/my-lisp/zk"
;;    "/Users/grantrosson/.emacs.d/my-lisp/zk-luhmann"))

(defun gr/recompile-lisp-dirs (&optional force)
  (interactive "P")
  (dolist (dir '("lisp" "my-lisp" "/my-lisp/priv-lisp"))
    (let ((exp-dir (expand-file-name (concat user-emacs-directory dir))))
      (byte-recompile-directory exp-dir 0 force t))))

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
        use-package-expand-minimally t
        use-package-compute-statistics nil
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

(setq default-frame-alist '((width . 100)
                            (height . 40)
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

;;; startup time

;; Emacs startup proceeds through several stages: it loads the early init and
;; regular init files, processes command-line options, sets after-init-time,
;; runs emacs-startup-hook, performs additional initial-frame setup such as
;; applying frame parameters from the configuration, and then runs
;; window-setup-hook after the initial frame parameters have been configured.
;; This makes window-setup-hook a useful place to record a broader startup-time
;; measurement.
;;
;; URL: https://www.jamescherti.com/measuring-emacs-startup-time/
(defvar my-recorded-startup-time-message nil
  "Stores the formatted string of the Emacs startup metrics.")

(defun my-record-startup-time ()
  "Calculate and record the elapsed startup time.
This function records the time when `window-setup-hook' runs."
  (setq my-recorded-startup-time-message
        (format "Emacs loaded in %.2f seconds (Init time: %.2fs) with %d garbage collections."
                (float-time (time-since before-init-time))
                (float-time (time-subtract after-init-time before-init-time))
                gcs-done))
  ;; Output to the *Messages* buffer during the initial launch
  (message "%s" my-recorded-startup-time-message))

(defun my-display-startup-time ()
  "Display the previously recorded Emacs startup time in the echo area."
  (interactive)
  (if my-recorded-startup-time-message
      (message "%s" my-recorded-startup-time-message)
    (message "Startup time was not recorded.")))

;; Read startup summary:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html
(add-hook 'window-setup-hook #'my-record-startup-time 99)
