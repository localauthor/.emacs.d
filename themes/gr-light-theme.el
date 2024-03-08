;;; gr-light-theme.el --- A light theme  -*- lexical-binding: t; -*-

;;; Commentary:

;; To use it, put the following in your Emacs configuration file:
;;
;;   (load-theme 'gr-light t)

;;; Code:

(deftheme gr-light)

;;(let* ((c '((class color) (min-colors 256))))

(custom-theme-set-faces
 'gr-light

;;;; fonts

 `(default ((t (:family "DejaVu Sans Mono"
                        ;; "IBM Plex Mono" ;; "JetBrains Mono"
                        :width normal
                        :height 130 ;; 110
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

 `(fixed-pitch ((t (:family "Monospace"
                            :foreground "#212121"
                            :background "#FAFAFA"))))

 `(variable-pitch ((t (:family "Arial"
                               ;; "Times New Roman"
                               ;; "Cardo" "Bembo Book MT std"
                               ;; "ET Bembo"
                               :height 130
                               :foreground "#212121"
                               :background "#FAFAFA"))))

;;;; basics

 `(cursor ((t (:background "black"))))
 `(highlight ((t (:background "darkseagreen2"))))
 `(minibuffer-prompt ((t (:foreground "medium blue"))))
 `(fringe ((t (:background "gray90" :box (:line-width 1 :style   released-button)))))
 `(tooltip ((t (:foreground "black" :background "lightyellow"))))
 `(escape-glyph ((t (:foreground "brown"))))
 `(homoglyph ((t (:foreground "brown"))))

;;;; tab-bar

 `(tab-bar ((t (:background "grey90" :foreground "black" :font "Menlo" :height 110))))
 `(tab-bar-tab ((t (:background "gray40" :foreground "gray90" :box (:line-width 1 :style released-button)))))
 `(tab-bar-tab-inactive ((t (:background "gray60" :foreground "gray80":box (:line-width 1 :style pressed-button)))))

;;;; mode-line

 `(mode-line ((t (:family "Menlo" :height 110 :box (:line-width -1 :style released-button) :background "gray75" :foreground "black"))))
 `(mode-line-buffer-id ((t (:weight bold))))
 `(mode-line-emphasis ((t (:weight bold))))
 `(header-line ((t (:inherit (mode-line)))))

;;;; region

 `(region ((t (:extend t :background "#a9a9b8"))))
 `(shadow ((t (:foreground "gray50"))))
 `(secondary-selection ((t (:extend t :background "yellow1"))))
 `(trailing-whitespace ((t (:background "red1"))))

;;;; font-lock

 `(font-lock-builtin-face ((t (:foreground "dark slate blue"))))
 `(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
 `(font-lock-comment-face ((t (:foreground "Firebrick"))))
 `(font-lock-constant-face ((t (:foreground "dark cyan"))))
 `(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 `(font-lock-function-name-face ((t (:foreground "Blue1"))))
 `(font-lock-keyword-face ((t (:foreground "Purple"))))
 `(font-lock-negation-char-face ((t nil)))
 `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 `(font-lock-string-face ((t (:foreground "VioletRed4"))))
 `(font-lock-type-face ((t (:foreground "ForestGreen"))))
 `(font-lock-variable-name-face ((t (:foreground "sienna"))))
 `(font-lock-warning-face ((t (:inherit (error)))))

;;;; buttons / links

 `(button ((t (:inherit (link)))))
 `(link ((t (:underline (:color foreground-color :style line) :foreground "RoyalBlue3"))))
 `(link-visited ((t (:inherit (link)))))

;;;; isearch

 `(isearch ((t (:foreground "lightskyblue1" :background "magenta3"))))
 `(isearch-fail ((t (:background "RosyBrown1"))))
 `(lazy-highlight ((t (:background "paleturquoise"))))
 `(match ((t (:background "khaki1"))))
 `(next-error ((t (:inherit (region)))))
 `(query-replace ((t (:inherit (isearch)))))

;;;; org-mode

 `(org-drawer ((t (:foreground "gray60" :height .8))))
 `(org-special-keyword ((t (:foreground "gray50" :height .8))))
 `(org-hide ((t (:foreground "white"))))
 `(org-meta-line ((t (:inherit fixed-pitch :foreground "Firebrick"))))
 `(org-block ((t (:inherit fixed-pitch))))
 `(org-document-info-keyword ((t (:inherit fixed-pitch :foreground "gray50"))))
 `(org-table ((t (:inherit fixed-pitch))))
 `(org-tag ((t (:foreground "gray50" :weight regular))))
 `(org-code ((t (:inherit fixed-pitch))))
 `(org-block ((t (:inherit fixed-pitch))))
 `(org-ellipsis ((t (:inherit fixed-pitch :foreground "gray50" :underline nil))))
 `(org-todo ((t (:family "Menlo" :weight bold :foreground "Red1"))))
 `(org-done ((t (:family "Menlo" :weight bold :foreground "ForestGreen"))))

 ;; `(outline-1 ((t (:foreground "blue3" :weight bold :underline t))))
 ;; `(outline-2 ((t (:foreground "black" :weight bold :underline t))))
 ;; `(outline-1 ((t (:foreground "blue3" :weight bold :underline t))))
 ;; `(outline-2 ((t (:foreground "black" :weight bold :underline t))))
 `(outline-3 ((t (:underline t))))
 `(outline-4 ((t (:underline t))))
 `(outline-5 ((t (:underline t))))

 `(org-level-1 ((t (:inherit outline-1))))
 `(org-level-2 ((t (:inherit outline-2))))
 `(org-level-3 ((t (:inherit outline-3))))
 `(org-level-4 ((t (:inherit outline-4))))
 `(org-level-5 ((t (:inherit outline-5))))
 `(org-level-6 ((t (:inherit outline-6))))
 `(org-level-7 ((t (:inherit outline-7))))
 `(org-level-8 ((t (:inherit outline-9))))

;;;; powerline
 `(powerline-active1 ((t (:background "Gray60"))))
 `(powerline-active2 ((t (:background "Gray80"))))

;;;; elfeed
 `(elfeed-search-title-face ((t (:foreground "Black"))))
 `(elfeed-search-feed-face ((t (:foreground "Sienna"))))
 `(elfeed-search-tag-face ((t (:foreground "ForestGreen"))))

;;;; mail
 `(gnus-button ((t :inherit button)))
 `(gnus-cite-1 ((t :inherit mu4e-cited-1-face)))
 `(gnus-cite-10 ((t :inherit mu4e-cited-10-face)))
 `(gnus-cite-11 ((t :inherit mu4e-cited-10-face)))
 `(gnus-cite-2 ((t :inherit mu4e-cited-2-face)))
 `(gnus-cite-3 ((t :inherit mu4e-cited-3-face)))
 `(gnus-cite-4 ((t :inherit mu4e-cited-4-face)))
 `(gnus-cite-5 ((t :inherit mu4e-cited-5-face)))
 `(gnus-cite-6 ((t :inherit mu4e-cited-6-face)))
 `(gnus-cite-7 ((t :inherit mu4e-cited-7-face)))
 `(gnus-cite-8 ((t :inherit mu4e-cited-8-face)))
 `(gnus-cite-9 ((t :inherit mu4e-cited-9-face)))
 `(gnus-emphasis-bold ((t :inherit bold)))
 `(gnus-emphasis-bold-italic ((t :inherit bold-italic)))
 `(gnus-emphasis-highlight-words ((t :inherit hi-yellow)))
 `(gnus-emphasis-italic ((t :inherit italic)))
 `(gnus-emphasis-underline-bold ((t :inherit gnus-emphasis-bold :underline t)))
 `(gnus-emphasis-underline-bold-italic ((t :inherit gnus-emphasis-bold-italic :underline t)))
 `(gnus-emphasis-underline-italic ((t :inherit gnus-emphasis-italic :underline t)))
 `(gnus-header-from ((t :inherit mu4e-contact-face :underline t)))
 `(gnus-header-name ((t :inherit mu4e-header-key-face)))
 `(gnus-header-content ((t :inherit mu4e-header-value-face)))
 `(gnus-header-newsgroups ((t :inherit message-header-newsgroups)))
 `(gnus-header-subject ((t :inherit mu4e-header-value-face)))
 `(gnus-splash ((t :inherit shadow)))

 )


;;;; provide

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; add theme folder to `custom-theme-load-path' when installing over MELPA
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'gr-light)

(provide 'gr-light-theme)

;;; gr-light-theme.el ends here
