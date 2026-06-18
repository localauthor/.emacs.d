;;; gr-theme-setup.el --- Custom theme setup         -*- lexical-binding: t; -*-

;;; Code:


(setq custom-theme-directory (concat user-emacs-directory "var/themes"))

;; if doric-theme not found, delete .elc files in doric-themes dir

;; (byte-recompile-directory
;;  (file-name-directory (find-library-name "doric-themes")))

(use-package doric-themes
  :demand t
  :config
  (require 'doric-obsidian-theme)
  (require 'doric-marble-theme)
  (doric-themes-select 'doric-marble))

(defun gr/custom-set-faces (&optional _)
  "My custom set faces for all themes.
Settinng these in use-package :custom-face doesn’t always transfer across themes."
  (custom-set-faces

   `(default ((t ( :family "DejaVu Sans Mono"
                   ;; "IBM Plex Mono" ;; "JetBrains Mono"
                   :width normal
                   :height 130 ;; 110
                   :weight normal
                   :slant normal))))

   `(fixed-pitch ((t (:family "DejaVu Sans Mono"))))

   `(variable-pitch ((t ( :family "SF Pro"
                          :height 130))))

;;; outline

   `(outline-1 ((t ( :weight bold
                     :underline (:style dashes)
                     :height 190
                     :extend t))))
   `(outline-2 ((t (:bold t :underline t))))
   `(outline-3 ((t (:underline nil))))
   `(outline-4 ((t (:underline nil))))
   `(outline-5 ((t (:underline nil))))

   `(org-level-1 ((t (:inherit outline-1 :extend t))))
   `(org-level-2 ((t (:inherit outline-2 :underline nil))))
   `(org-level-3 ((t (:inherit outline-3))))
   `(org-level-4 ((t (:inherit outline-4))))
   `(org-level-5 ((t (:inherit outline-5))))
   `(org-level-6 ((t (:inherit outline-6))))
   `(org-level-7 ((t (:inherit outline-7))))
   `(org-level-8 ((t (:inherit outline-9))))

;;; org

   `(org-hide ((t (:foreground
                   ,(face-attribute 'default :background)))))
   `(org-todo ((t (:family "Menlo" :weight bold :foreground "Red1"))))
   `(org-done ((t (:family "Menlo" :weight bold :foreground "Gray60"))))
   `(org-tag ((t (:family "Menlo" :height 110))))
   `(org-drawer ((t (:height .8))))
   `(org-special-keyword ((t (:height .8))))
   `(org-document-title ((t (:inherit default :box nil :height 1.2 :weight bold))))
   `(org-document-info-keyword ((t (:underline t))))
   ;; `(org-document-info ((t (:slant italic))))
   `(org-meta-line ((t (:inherit default :weight light))))
   `(org-ellipsis ((t (:inherit default :underline nil))))
   `(org-agenda-date ((t (:overline t :bold nil))))
   `(org-agenda-date-weekend ((t (:foreground "forestgreen" :overline t :bold nil))))
   `(org-agenda-date-today ((t (:foreground "black" :inverse-video nil :underline nil :italic nil :bold t :height 1 :overline t :box t))))

;;; minibuffer completion

   `(completions-common-part
     ((t (:inherit bold :underline nil))))
   `(completions-first-difference
     ((t (:inherit nil :underline nil))))
   `(orderless-match-face-0 ((t (:inherit bold :underline nil))))
   `(orderless-match-face-1 ((t (:inherit bold :underline nil))))
   `(orderless-match-face-2 ((t (:inherit bold :underline nil))))
   `(orderless-match-face-3 ((t (:inherit bold :underline nil))))

;;; shr text

   `(shr-text
     ((t ( :inherit nil
           :font "Baskerville"
           :height 170))))

;;; dired
   `(dired-directory
     ((t (:underline t))))
   `(dired-symlink
     ((t (:underline nil))))

;;; tab-bar

   `(tab-bar
     ((t ( :inherit nil
           :font "Menlo"
           :height 110))))
   `(tab-bar-tab
     ((t ( :inherit mode-line
           :font "Menlo"
           :height 110
           :foreground
           ,(face-attribute 'mode-line :foreground)
           :background
           ,(face-attribute 'mode-line :background)
           :box (:line-width 1 :style released-button)))))

   `(tab-bar-tab-inactive
     ((t ( :inherit nil
           :font "Menlo"
           :height 110
           :foreground
           ,(face-attribute 'mode-line-inactive :foreground)
           :background
           ,(face-attribute 'mode-line-inactive :background)
           :box ( :line-width 1
                  :style pressed-button)))))

;;; mode-line

   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line
     ((t ( :family "Menlo"
           :height 110
           :box (:line-width -1 :style released-button)))))
   `(mode-line-inactive
     ((t ( :family "Menlo"
           :height 110
           :weight light))))
   `(header-line ((t (:inherit (mode-line)))))
   `(header-line-inactive ((t (:family "Menlo" :height 110))))

;;; emacs

   `(region ((t (:extend t))))
   `(next-error ((t (:inherit (region)))))
   `(query-replace ((t (:inherit (isearch)))))

   `(fringe
     ((t ( :inherit mode-line-inactive
           :box (:line-width 1 :style released-button)))))

   `(font-lock-keyword-face
     ((t (:inherit nil))))

;;; packages

   `(org-side-tree-heading-face
     ((t (:inherit font-lock-builtin-face))))

   `(calfw-face-toolbar-button-off
     ((t (:inherit font-lock-builtin-face))))

   `(aw-leading-char-face
     ((t (:family "Menlo" :height 2.5))))

   `(golden-ratio-scroll-highlight-line-face
     ((t (:inherit highlight :weight normal ))))

   `(vertico-group-title
     ((t ( :height .9
           :weight bold
           :slant normal))))

   `(consult-file
     ((t ( :slant normal
           :inherit font-lock-variable-name-face))))
   `(consult-bookmark
     ((t ( :slant normal
           :inherit font-lock-function-name-face))))
   ))


(with-eval-after-load 'server
  (add-to-list 'server-after-make-frame-hook #'gr/custom-set-faces))

(add-to-list 'enable-theme-functions #'gr/custom-set-faces)

;; enable theme or update customizations
(if custom-enabled-themes
    (gr/custom-set-faces)
  (load-theme 'doric-obsidian t)
  (load-theme 'gr-light t t))

;;; theme functions

(defvar gr/light-themes '(doric-marble))

(defvar gr/dark-themes '(doric-obsidian))  ;; modus-vivendi-tinted

(defun gr/themes-rotate-or-random (themes current)
  "Rotate THEMES rightward such that the car is moved to the end."
  (if-let* ((index (seq-position themes current))
            (offset (1+ index)))
      (car (append (nthcdr offset themes) (take offset themes)))
    (or (nth (random (length themes)) themes)
        (car themes))))

(defun gr/toggle-theme-light-dark (&optional arg)
  "Toggle between light and dark themes."
  (interactive "P")
  (let* ((current (car custom-enabled-themes))
         (dark gr/dark-themes)
         (light gr/light-themes)
         (themes (if (member current gr/dark-themes)
                     (if arg dark light)
                   (if arg light dark)))
         (new-theme (gr/themes-rotate-or-random themes current)))
    (gr/select-theme new-theme)))

(defun gr/select-theme (theme)
  (interactive (list (intern (completing-read "Select: "
                                              (append
                                               gr/dark-themes
                                               gr/light-themes)))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (message "Enabled %s" theme))

;; (defvar gr/toggle-theme-pair '(gr-light . ef-owl))

;; (defun gr/toggle-theme ()
;;   "Toggle between the two `gr/toggle-theme-pair' themes."
;;   (interactive)
;;   (let* ((theme1 (car gr/toggle-theme-pair))
;;          (theme2 (cdr gr/toggle-theme-pair))
;;          (new-theme (if (member theme1 custom-enabled-themes)
;;                         theme2 theme1)))
;;     (gr/select-theme new-theme)))


(provide 'gr-theme-setup)
;;; gr-theme-setup.el ends here
