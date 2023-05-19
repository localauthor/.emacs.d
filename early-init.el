;;; -*- lexical-binding: t -*-

(setq debug-on-error t)
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

(setq load-prefer-newer t)

;;; package setup

(setq package-enable-at-startup t)
(setq package-quickstart t)

(setq package-vc-allow-side-effects t)

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t)
  (setq use-package-hook-name-suffix nil))

;;; setenv

(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/12:/usr/local/opt/libgccjit/lib/gcc/12:/usr/local/opt/gcc/lib/gcc/12/gcc/x86_64-apple-darwin21/12")

;;; emacs config

(setq user-emacs-directory "~/.dotfiles/.emacs.d/")

(pixel-scroll-precision-mode)

(setq set-mark-command-repeat-pop t)

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

(setq dictionary-server "dict.org")

(setq auto-save-default nil) ;; stop creating #autosave# files
(setq create-lockfiles nil)  ;; stop creating .# files

(setq inhibit-splash-screen t)
(desktop-save-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq scroll-bar-mode nil)

(setq confirm-kill-emacs 'y-or-n-p)

(setq use-dialog-box nil)

;;(setq recenter-positions '(middle bottom top))

(add-hook 'after-make-frame-functions
          #'(lambda (frame)
              (modify-frame-parameters frame
                                       '((vertical-scroll-bars . nil)
                                         (horizontal-scroll-bars . nil)))))

(global-auto-revert-mode t)

(delete-selection-mode 1)
;;(global-visual-line-mode 1)
(global-hl-line-mode 0)

(winner-mode 1)

(transient-mark-mode 1)

(setq minibuffer-follows-selected-frame nil)

(setq find-library-include-other-files nil)

(setq-default indent-tabs-mode nil) ;; use spaces for tabs
(setq sentence-end-double-space nil)

(setq-default fill-column 77)

(setq search-default-mode t) ;; use literal strings in isearch, not regexps
(setq isearch-lazy-count t)

(setq ad-redefinition-action 'accept)
(setq warning-suppress-types (quote (bytecomp comp)))
;; outshine uses cl; so this suppresses deprecated warning
(setq byte-compile-warnings '((not cl-functions)))
(setq native-comp-async-report-warnings-errors nil)
(setq warning-minimum-level ':error)
(setq ring-bell-function 'ignore)

(setq vc-follow-symlinks t)

(add-to-list 'completion-ignored-extensions ".DS_Store")

(setq custom-file (concat user-emacs-directory "custom.el"))

;; backups
(setq make-backup-files t)
(setq vc-make-backup-files t)
(setq version-control t ;; Use version numbers for backups.
      kept-new-versions 10 ;; Number of newest versions to keep.
      kept-old-versions 0 ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t) ;; Copy all files, don't rename them.

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups/per-save"))))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist `(("." . ,(concat user-emacs-directory "backups/per-session"))))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(with-eval-after-load 'zk
  (add-to-list 'backup-directory-alist
               `(,zk-id-regexp . ,(concat user-emacs-directory "backups/per-save/ZK-backups")))

  (defun force-backup-of-buffer ()
    ;; Make a special "per session" backup at the first save of each
    ;; emacs session.
    (when (not buffer-backed-up)
      ;; Override the default parameters for per-session backups.
      (let ((backup-directory-alist `((,zk-id-regexp . ,(concat user-emacs-directory "backups/per-session/ZK-backups"))
                                      ("." . ,(concat user-emacs-directory "backups/per-session"))))
            (kept-new-versions 3))
        (backup-buffer)))
    ;; Make a "per save" backup on each save.  The first save results in
    ;; both a per-session and a per-save backup, to keep the numbering
    ;; of per-save backups consistent.
    (let ((buffer-backed-up nil))
      (backup-buffer))))

(add-hook 'before-save-hook 'force-backup-of-buffer)

(setq epg-gpg-program "/usr/local/bin/gpg")
(setq xref-search-program 'ripgrep)

(setq erc-server "irc.libera.chat"
      erc-nick "localauthor"
      erc-autojoin-channels-alist '(("#emacs" "#org-mode" "#systemcrafters")))

;; yes-or-no function

(setq y-or-n-p-use-read-key t) ;; needed for embark
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
    "Moves file at PATH to macOS Trash following `move-file-to-trash' convention.

      Relies on the command-line utility 'trash' to be installed.
      Get it from:  <http://hasseg.org/trash/>"
    (shell-command (concat "trash -vF \"" path "\""
                           "| sed -e 's/^/Trashed: /'")
                   nil ;; Name of output buffer
                   "*Trash Error Buffer*")))

(defun gr/make-frame ()
  "Make frame, centered, on current monitor."
  (interactive)
  (make-frame-on-current-monitor)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (modify-frame-parameters
     (selected-frame) '((user-position . t) (top . 0.5) (left . 0.5)))))

;; time and mode-line

(setq display-time-24hr-format t
      display-time-day-and-date nil
      display-time-default-load-average nil
      display-time-format "[%H:%M]") ;; put time in brackets

(display-time-mode 1)

(setq global-mode-string '("")) ;; remove display-time-string from right

;; truncate buffer name in mode-line to 29 characters
(setq-default mode-line-buffer-identification
              (append '(-29)
                      (propertized-buffer-identification "%b")))

(setq-default mode-line-format
              '(;;"%e"
                ;;mode-line-front-space
                ;;mode-line-mule-info
                ;;mode-line-client
                ;;mode-line-modified
                ;;mode-line-remote
                "  "
                display-time-string ;; left align
                mode-line-frame-identification
                mode-line-buffer-identification
                "  "
                mode-line-position
                "  "
                mode-line-modes
                "  "
                (vc-mode vc-mode)
                "  "
                mode-line-misc-info
                mode-line-end-spaces))
