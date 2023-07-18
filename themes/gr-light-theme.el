;;; gr-light-theme.el --- A light theme  -*- lexical-binding: t; -*-

;;; Commentary:

;; To use it, put the following in your Emacs configuration file:
;;
;;   (load-theme 'gr-light t)

;;; Code:

(deftheme gr-light)

(let* ((c '((class color) (min-colors 256))))

  (custom-theme-set-faces
   'gr-light

   `(default ((,c (:family "JetBrains Mono" ;; "IBM Plex Mono"
                           ;;:foundry "IBM"
                           :width normal
                           :height 110
                           :weight normal
                           :slant normal
                           :underline nil
                           :overline nil
                           :extend nil
                           :strike-through nil
                           :box nil :inverse-video nil
                           :foreground "#212121"
                           :background "#FAFAFA"
                           :stipple nil
                           :inherit nil))))

   `(fixed-pitch ((,c (:family "Monospace"
                               :foreground "#212121"
                               :background "#FAFAFA"))))

   `(variable-pitch ((,c (:family "Times New Roman"
                                  ;; "Cardo" "Bembo Book MT std" "ET Bembo"
                                  :height 1.3
                                  :foreground "#212121"
                                  :background "#FAFAFA"))))

   `(cursor ((,c (:background "black"))))
   `(escape-glyph ((,c (:foreground "brown"))))
   `(homoglyph ((,c (:foreground "brown"))))
   `(minibuffer-prompt ((,c (:foreground "medium blue"))))
   `(highlight ((,c (:background "darkseagreen2"))))
   `(region ((,c (:extend t :background "#a9a9b8"))))
   `(shadow ((,c (:foreground "grey50"))))
   `(secondary-selection ((,c (:extend t :background "yellow1"))))
   `(trailing-whitespace ((,c (:background "red1"))))
   `(font-lock-builtin-face ((,c (:foreground "dark slate blue"))))
   `(font-lock-comment-delimiter-face ((,c (:inherit (font-lock-comment-face)))))
   `(font-lock-comment-face ((,c (:foreground "Firebrick"))))
   `(font-lock-constant-face ((,c (:foreground "dark cyan"))))
   `(font-lock-doc-face ((,c (:inherit (font-lock-string-face)))))
   `(font-lock-function-name-face ((,c (:foreground "Blue1"))))
   `(font-lock-keyword-face ((,c (:foreground "Purple"))))
   `(font-lock-negation-char-face ((,c nil)))
   `(font-lock-preprocessor-face ((,c (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((,c (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((,c (:inherit (bold)))))
   `(font-lock-string-face ((,c (:foreground "VioletRed4"))))
   `(font-lock-type-face ((,c (:foreground "ForestGreen"))))
   `(font-lock-variable-name-face ((,c (:foreground "sienna"))))
   `(font-lock-warning-face ((,c (:inherit (error)))))
   `(button ((,c (:inherit (link)))))
   `(link ((,c (:underline (:color foreground-color :style line) :foreground "RoyalBlue3"))))
   `(link-visited ((,c (:inherit (link)))))
   `(fringe ((,c (:background "grey95"))))
   `(header-line ((,c (:inherit (mode-line)))))
   `(tooltip ((,c (:foreground "black" :background "lightyellow"))))
   `(mode-line-buffer-id ((,c (:weight bold))))
   `(mode-line-emphasis ((,c (:weight bold))))
   `(isearch ((,c (:foreground "lightskyblue1" :background "magenta3"))))
   `(isearch-fail ((,c (:background "RosyBrown1"))))
   `(lazy-highlight ((,c (:background "paleturquoise"))))
   `(match ((,c (:background "khaki1"))))
   `(next-error ((,c (:inherit (region)))))
   `(query-replace ((,c (:inherit (isearch)))))

;;;; org-mode

   `(org-drawer ((,c (:foreground "gray60" :height .8))))
   `(org-special-keyword ((,c (:foreground "gray50" :height .8))))
   `(org-hide ((,c (:foreground "white"))))
   `(org-meta-line ((,c (:inherit fixed-pitch :foreground "Firebrick"))))
   `(org-block ((,c (:inherit fixed-pitch))))
   `(org-document-info-keyword ((,c (:inherit fixed-pitch :foreground "grey50"))))
   `(org-table ((,c (:inherit fixed-pitch))))
   `(org-tag ((,c (:foreground "grey50" :weight regular))))
   `(org-code ((,c (:inherit fixed-pitch))))
   `(org-block ((,c (:inherit fixed-pitch))))
   `(org-ellipsis ((,c (:inherit fixed-pitch :foreground "grey50" :underline nil))))
   `(org-todo ((,c (:family "Menlo" :weight bold :foreground "Red1"))))
   `(org-done ((,c (:family "Menlo" :weight bold :foreground "ForestGreen"))))

   `(outline-1 ((,c (:foreground "blue3" :weight bold :underline t))))
   `(outline-2 ((,c (:foreground "black" :weight bold :underline t))))
   `(outline-3 ((,c (:underline t))))
   `(outline-4 ((,c (:underline t))))
   `(outline-5 ((,c (:underline t))))

   `(org-level-1 ((,c (:inherit outline-1))))
   `(org-level-2 ((,c (:inherit outline-2))))
   `(org-level-3 ((,c (:inherit outline-3))))
   `(org-level-4 ((,c (:inherit outline-4))))
   `(org-level-5 ((,c (:inherit outline-5))))
   `(org-level-6 ((,c (:inherit outline-6))))
   `(org-level-7 ((,c (:inherit outline-7))))
   `(org-level-8 ((,c (:inherit outline-9))))

;;;; powerline
   `(powerline-active1 ((t (:background "Gray60"))))
   `(powerline-active2 ((t (:background "Gray80"))))

;;;; elfeed
   `(elfeed-search-title-face ((,c (:foreground "Black"))))
   `(elfeed-search-feed-face ((,c (:foreground "Sienna"))))
   `(elfeed-search-tag-face ((,c (:foreground "ForestGreen"))))

;;;; mail
   `(gnus-button ((,c :inherit button)))
   `(gnus-cite-1 ((,c :inherit mu4e-cited-1-face)))
   `(gnus-cite-10 ((,c :inherit mu4e-cited-10-face)))
   `(gnus-cite-11 ((,c :inherit mu4e-cited-10-face)))
   `(gnus-cite-2 ((,c :inherit mu4e-cited-2-face)))
   `(gnus-cite-3 ((,c :inherit mu4e-cited-3-face)))
   `(gnus-cite-4 ((,c :inherit mu4e-cited-4-face)))
   `(gnus-cite-5 ((,c :inherit mu4e-cited-5-face)))
   `(gnus-cite-6 ((,c :inherit mu4e-cited-6-face)))
   `(gnus-cite-7 ((,c :inherit mu4e-cited-7-face)))
   `(gnus-cite-8 ((,c :inherit mu4e-cited-8-face)))
   `(gnus-cite-9 ((,c :inherit mu4e-cited-9-face)))
   `(gnus-emphasis-bold ((,c :inherit bold)))
   `(gnus-emphasis-bold-italic ((,c :inherit bold-italic)))
   `(gnus-emphasis-highlight-words ((,c :inherit hi-yellow)))
   `(gnus-emphasis-italic ((,c :inherit italic)))
   `(gnus-emphasis-underline-bold ((,c :inherit gnus-emphasis-bold :underline t)))
   `(gnus-emphasis-underline-bold-italic ((,c :inherit gnus-emphasis-bold-italic :underline t)))
   `(gnus-emphasis-underline-italic ((,c :inherit gnus-emphasis-italic :underline t)))
   `(gnus-header-from ((,c :inherit mu4e-contact-face :underline t)))
   `(gnus-header-name ((,c :inherit mu4e-header-key-face)))
   `(gnus-header-content ((,c :inherit mu4e-header-value-face)))
   `(gnus-header-newsgroups ((,c :inherit message-header-newsgroups)))
   `(gnus-header-subject ((,c :inherit mu4e-header-value-face)))
   `(gnus-splash ((,c :inherit shadow)))
   
   ))

   ;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; add theme folder to `custom-theme-load-path' when installing over MELPA
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'gr-light)

(provide 'gr-light-theme)

;;; gr-light-theme.el ends here
