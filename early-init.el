;;; early-init.el -*- lexical-binding: t -*-

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'after-init-hook
          (lambda ()
            (setq debug-on-error nil)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

(auto-save-mode -1)

(setq auto-save-default nil
      load-prefer-newer t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      inhibit-splash-screen t
      inhibit-compacting-font-caches t
      frame-inhibit-implied-resize t
      auto-mode-case-fold nil
      bidi-inhibit-bpa t)

(setq-default
 bidi-display-reordering 'left-to-right
 bidi-paragraph-direction 'left-to-right)

(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

;;; speedup

(let ((default-gc-threshold gc-cons-threshold)
      (default-gc-percentage gc-cons-percentage))
  (setq gc-cons-threshold most-positive-fixnum
        default-gc-percentage 0.8)
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-percentage default-gc-percentage
                    gc-cons-threshold default-gc-threshold))))


(setq use-package-always-ensure t
      ;; use-package-expand-minimally t
      use-package-vc-prefer-newest t
      use-package-enable-imenu-support t
      use-package-hook-name-suffix nil)

;;; package.el

(use-package package
  :config
  (setq package-enable-at-startup t
        package-check-signature nil
        package-quickstart t
        package-install-upgrade-built-in nil
        package-native-compile t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  (add-to-list ' package-archives
                 '("org" . "https://orgmode.org/elpa/"))
  ;; (add-to-list 'package-archives
  ;;              '("elpa-devel" . "https://elpa.gnu.org/devel/") 'append)
  )

(use-package package-vc
  :config
  ;; check for new setting about pulling newest revision
  (setq package-vc-register-as-project nil))

;;; setenv

;; necessary for emacs to find gcc for native compilation
;; (setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/current:/usr/local/opt/libgccjit/lib/gcc/current:/usr/local/opt/glib/lib/glib-2.0/")

;; not necessary anymore, bc path is injected into build in my build script -> ~/Repos/emacs-build/emacs-build


(setq frame-resize-pixelwise t)
(setq frame-title-format "%b")

(setq default-frame-alist '((tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (undecorated . t)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (left-fringe . 8)
                            (right-fringe . 8)))

;; (tool-bar-mode 1)
;; (scroll-bar-mode -1)
(menu-bar-mode -1)

(setq
 tool-bar-mode nil
 scroll-bar-mode nil)
