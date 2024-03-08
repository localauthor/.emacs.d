;;; early-init.el -*- lexical-binding: t -*-

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'after-init-hook
          (lambda ()
            (setq debug-on-error nil)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))

;;; speedup

(let ((default-gc-threshold gc-cons-threshold)
      (default-gc-percentage gc-cons-percentage))
  (setq gc-cons-threshold most-positive-fixnum
        default-gc-percentage 0.8)
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-percentage default-gc-percentage
                    gc-cons-threshold default-gc-threshold))))

;;; package.el

(use-package package
  :config
  (setq package-enable-at-startup t
        package-check-signature nil
        package-quickstart t
        package-install-upgrade-built-in nil
        package-vc-allow-side-effects t
        package-native-compile t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))

;;; setenv

;; necessary for emacs to find gcc for native compilation
(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/current:/usr/local/opt/libgccjit/lib/gcc/current:/usr/local/Cellar/glib/2.78.0/lib/glib-2.0/")


;; (setq-default inhibit-redisplay t
;;               inhibit-message t)
;; (add-hook 'window-setup-hook
;;           (lambda ()
;;             (setq-default inhibit-redisplay nil
;;                           inhibit-message nil)
;;             (redisplay)))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq frame-resize-pixelwise t)
(setq frame-title-format "%b")

(setq default-frame-alist '((tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (undecorated . t)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (left-fringe . 8)
                            (right-fringe . 8)))
