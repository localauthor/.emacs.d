;;; gr-light-theme.el --- A light theme  -*- lexical-binding: t; -*-

;;; Commentary:

;; To use it, put the following in your Emacs configuration file:
;;
;;   (load-theme 'gr-light t)

;;; Code:

(deftheme gr-light)

(display-color-cells (selected-frame))

(let* ((class '((class color) (min-colors 88)))
       (256color (eq (display-color-cells (selected-frame)) 256))
       (truecolor (eq (display-color-cells (selected-frame)) 16777216))
       (background (if (or window-system truecolor) "#FAFAFA" "#FAFAFA"))
       
       (custom-theme-set-faces
        'gr-light
        '(default ((t (:family "JetBrains Mono" ;; "IBM Plex Mono" ;; 
                               ;;:foundry "IBM"
                               :width normal
                               :height 1
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
        
        '(fixed-pitch ((t (:family "Monospace" :foreground "#212121" :background "#FAFAFA"))))

        '(variable-pitch ((t (:family "Times New Roman" ;;  "Cardo" ;; "Bembo Book MT std" ;; "ET Bembo" ;;
                                      :height 1.3 :foreground "#212121" :background "#FAFAFA"))))

        '(cursor
          ((((background light)) (:background "black"))
           (((background dark)) (:background "white"))))

        '(escape-glyph
          ((((background dark)) (:foreground "cyan"))
           (((type pc)) (:foreground "magenta"))
           (t (:foreground "brown"))))

        '(homoglyph
          ((((background dark)) (:foreground "cyan"))
           (((type pc)) (:foreground "magenta"))
           (t (:foreground "brown"))))

        '(minibuffer-prompt
          ((((background dark)) (:foreground "cyan"))
           (((type pc)) (:foreground "magenta"))
           (t (:foreground "medium blue"))))

        '(highlight
          ((((class color) (min-colors 88) (background light)) (:background "darkseagreen2"))
           (((class color) (min-colors 88) (background dark)) (:background "darkolivegreen"))
           (((class color) (min-colors 16) (background light)) (:background "darkseagreen2"))
           (((class color) (min-colors 16) (background dark)) (:background "darkolivegreen"))
           (((class color) (min-colors 8)) (:foreground "black" :background "green"))
           (t (:inverse-video t))))

        '(region ((t (:extend t :background "#a9a9b8"))))

        '(shadow
          ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50"))
           (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70"))
           (((class color) (min-colors 8) (background light)) (:foreground "green"))
           (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))

        '(secondary-selection 
          (((class color) (min-colors 88) (background light)) (:extend t :background "yellow1"))
          (((class color) (min-colors 88) (background dark)) (:extend t :background "SkyBlue4"))
          (((class color) (min-colors 16) (background light)) (:extend t :background "yellow"))
          (((class color) (min-colors 16) (background dark)) (:extend t :background "SkyBlue4"))
          (((class color) (min-colors 8)) (:extend t :foreground "black" :background "cyan"))
          (t (:inverse-video t))))

       '(trailing-whitespace 
         (((class color) (background light)) (:background "red1"))
         (((class color) (background dark)) (:background "red1"))
         (t (:inverse-video t)))

       '(font-lock-builtin-face
         ((((class grayscale) (background light)) (:weight bold :foreground "LightGray"))
          (((class grayscale) (background dark)) (:weight bold :foreground "DimGray"))
          (((class color) (min-colors 88) (background light)) (:foreground "dark slate blue"))
          (((class color) (min-colors 88) (background dark)) (:foreground "LightSteelBlue"))
          (((class color) (min-colors 16) (background light)) (:foreground "Orchid"))
          (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue"))
          (((class color) (min-colors 8)) (:weight bold :foreground "blue"))
          (t (:weight bold))))

       '(font-lock-comment-delimiter-face
         ((default (:inherit (font-lock-comment-face)))))

       '(font-lock-comment-face
         ((((class grayscale) (background light)) (:slant italic :weight bold :foreground "DimGray"))
          (((class grayscale) (background dark)) (:slant italic :weight bold :foreground "LightGray"))
          (((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
          (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1"))
          (((class color) (min-colors 16) (background light)) (:foreground "red"))
          (((class color) (min-colors 16) (background dark)) (:foreground "red1"))
          (((class color) (min-colors 8) (background light)) (:foreground "red"))
          (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))
          (t (:slant italic :weight bold))))

       '(font-lock-constant-face
         ((((class grayscale) (background light)) (:underline (:color foreground-color :style line) :weight bold :foreground "LightGray"))
          (((class grayscale) (background dark)) (:underline (:color foreground-color :style line) :weight bold :foreground "Gray50"))
          (((class color) (min-colors 88) (background light)) (:foreground "dark cyan"))
          (((class color) (min-colors 88) (background dark)) (:foreground "Aquamarine"))
          (((class color) (min-colors 16) (background light)) (:foreground "CadetBlue"))
          (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine"))
          (((class color) (min-colors 8)) (:foreground "magenta"))
          (t (:underline (:color foreground-color :style line) :weight bold))))

       '(font-lock-doc-face
         ((t (:inherit (font-lock-string-face)))))

       '(font-lock-function-name-face
         ((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
          (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
          (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
          (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
          (((class color) (min-colors 8)) (:weight bold :foreground "blue"))
          (t (:weight bold :inverse-video t))))

       '(font-lock-keyword-face
         ((((class grayscale) (background light)) (:weight bold :foreground "LightGray"))
          (((class grayscale) (background dark)) (:weight bold :foreground "DimGray"))
          (((class color) (min-colors 88) (background light)) (:foreground "Purple"))
          (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1"))
          (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
          (((class color) (min-colors 16) (background dark)) (:foreground "Cyan"))
          (((class color) (min-colors 8)) (:weight bold :foreground "cyan"))
          (t (:weight bold))))

       '(font-lock-negation-char-face ((t nil)))
       '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
       '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
       '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))

       '(font-lock-string-face
         ((((class grayscale) (background light)) (:slant italic :foreground "DimGray"))
          (((class grayscale) (background dark)) (:slant italic :foreground "LightGray"))
          (((class color) (min-colors 88) (background light)) (:foreground "VioletRed4"))
          (((class color) (min-colors 88) (background dark)) (:foreground "LightSalmon"))
          (((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
          (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
          (((class color) (min-colors 8)) (:foreground "green"))
          (t (:slant italic))))

       '(font-lock-type-face
         ((((class grayscale) (background light)) (:weight bold :foreground "Gray90"))
          (((class grayscale) (background dark)) (:weight bold :foreground "DimGray"))
          (((class color) (min-colors 88) (background light)) (:foreground "ForestGreen"))
          (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen"))
          (((class color) (min-colors 16) (background light)) (:foreground "ForestGreen"))
          (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen"))
          (((class color) (min-colors 8)) (:foreground "green"))
          (t (:underline (:color foreground-color :style line) :weight bold))))
       
       '(font-lock-variable-name-face
         ((((class grayscale) (background light)) (:slant italic :weight bold :foreground "Gray90"))
          (((class grayscale) (background dark)) (:slant italic :weight bold :foreground "DimGray"))
          (((class color) (min-colors 88) (background light)) (:foreground "sienna"))
          (((class color) (min-colors 88) (background dark)) (:foreground "LightGoldenrod"))
          (((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod"))
          (((class color) (min-colors 16) (background dark)) (:foreground "LightGoldenrod"))
          (((class color) (min-colors 8)) (:weight light :foreground "yellow"))
          (t (:slant italic :weight bold))))
       
       '(font-lock-warning-face
         ((t (:inherit (error)))))
       
       '(button
         ((t (:inherit (link)))))
       
       '(link
         ((((class color) (min-colors 88) (background light)) (:underline (:color foreground-color :style line) :foreground "RoyalBlue3"))
          (((class color) (background light)) (:underline (:color foreground-color :style line) :foreground "blue"))
          (((class color) (min-colors 88) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan1"))
          (((class color) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan"))
          (t (:inherit (underline)))))
       
       '(link-visited
         ((default (:inherit (link)))
          (((class color) (background light)) (:foreground "magenta4"))
          (((class color) (background dark)) (:foreground "violet"))))

       '(fringe
         ((((class color) (background light)) (:background "grey95"))
          (((class color) (background dark)) (:background "grey10"))
          (t (:background "gray"))))
       
       '(header-line
         ((default (:inherit (mode-line)))
          (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil))
          (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90"))
          (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20"))
          (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "black" :background "white"))
          (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "white" :background "black"))))
       
       '(tooltip
         ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow"))
          (t (:inherit (variable-pitch)))))

       '(mode-line-buffer-id ((t (:weight bold))))
       '(mode-line-emphasis ((t (:weight bold))))

       '(isearch
         ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3"))
          (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred2"))
          (((class color) (min-colors 16)) (:foreground "cyan1" :background "magenta4"))
          (((class color) (min-colors 8)) (:foreground "cyan1" :background "magenta4"))
          (t (:inverse-video t))))
       
       '(isearch-fail
         ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1"))
          (((class color) (min-colors 88) (background dark)) (:background "red4"))
          (((class color) (min-colors 16)) (:background "red"))
          (((class color) (min-colors 8)) (:background "red"))
          (((class color grayscale)) (:foreground "grey"))
          (t (:inverse-video t))))
       
       '(lazy-highlight
         ((((class color) (min-colors 88) (background light)) (:background "paleturquoise"))
          (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4"))
          (((class color) (min-colors 16)) (:background "turquoise3"))
          (((class color) (min-colors 8)) (:background "turquoise3"))
          (t (:underline (:color foreground-color :style line)))))
       
       '(match
         ((((class color) (min-colors 88) (background light)) (:background "khaki1"))
          (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3"))
          (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow"))
          (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue"))
          (((type tty) (class mono)) (:inverse-video t))
          (t (:background "gray"))))
       
       '(next-error ((t (:inherit (region)))))
       
       '(query-replace ((t (:inherit (isearch)))))


       ;;`(org-hide ((,class (:foreground "white"))))
       `(org-meta-line ((,class (:inherit fixed-pitch :foreground "Firebrick"))))
       `(org-block ((,class (:inherit fixed-pitch))))
       `(org-document-info-keyword ((,class (:inherit fixed-pitch :foreground "grey50"))))
       `(org-table ((,class (:inherit fixed-pitch))))
       `(org-tag ((,class (:foreground "grey50" :weight regular))))
       `(org-code ((,class (:inherit fixed-pitch))))
       `(org-block ((,class (:inherit fixed-pitch))))
       `(org-ellipsis ((,class (:inherit fixed-pitch :foreground "grey50" :underline nil))))
       `(org-todo ((,class (:family "Menlo" :weight bold :foreground "Red1"))))
       `(org-done ((,class (:family "Menlo" :weight bold :foreground "ForestGreen"))))

       `(outline-1 ((,class (:foreground "blue3" :weight bold :underline t))))
       `(outline-2 ((,class (:foreground "black" :weight bold :underline t))))
       `(outline-3 ((,class (:underline t))))
       `(outline-4 ((,class (:underline t))))
       `(outline-5 ((,class (:underline t))))

       `(org-level-1 ((,class (:inherit outline-1))))
       `(org-level-2 ((,class (:inherit outline-2))))
       `(org-level-3 ((,class (:inherit outline-3))))
       `(org-level-4 ((,class (:inherit outline-4))))
       `(org-level-5 ((,class (:inherit outline-5))))
       `(org-level-6 ((,class (:inherit outline-6))))
       `(org-level-7 ((,class (:inherit outline-7))))
       `(org-level-8 ((,class (:inherit outline-9))))

       ;; Powerline
       `(powerline-active1 ((t (:foreground ,foreground :background "Gray60"))))
       `(powerline-active2 ((t (:foreground ,foreground :background "Gray80"))))

       ;; elfeed
       `(elfeed-search-title-face ((,class (:foreground "Black"))))
       `(elfeed-search-feed-face ((,class (:foreground "Sienna"))))
       `(elfeed-search-tag-face ((,class (:foreground "ForestGreen"))))


       `(gnus-button ((,class :inherit button)))
       `(gnus-cite-1 ((,class :inherit mu4e-cited-1-face)))
       `(gnus-cite-10 ((,class :inherit mu4e-cited-10-face)))
       `(gnus-cite-11 ((,class :inherit mu4e-cited-10-face)))
       `(gnus-cite-2 ((,class :inherit mu4e-cited-2-face)))
       `(gnus-cite-3 ((,class :inherit mu4e-cited-3-face)))
       `(gnus-cite-4 ((,class :inherit mu4e-cited-4-face)))
       `(gnus-cite-5 ((,class :inherit mu4e-cited-5-face)))
       `(gnus-cite-6 ((,class :inherit mu4e-cited-6-face)))
       `(gnus-cite-7 ((,class :inherit mu4e-cited-7-face)))
       `(gnus-cite-8 ((,class :inherit mu4e-cited-8-face)))
       `(gnus-cite-9 ((,class :inherit mu4e-cited-9-face)))
       
       `(gnus-emphasis-bold ((,class :inherit bold)))
       `(gnus-emphasis-bold-italic ((,class :inherit bold-italic)))
       `(gnus-emphasis-highlight-words ((,class :inherit hi-yellow)))
       `(gnus-emphasis-italic ((,class :inherit italic)))
       `(gnus-emphasis-underline-bold ((,class :inherit gnus-emphasis-bold :underline t)))
       `(gnus-emphasis-underline-bold-italic ((,class :inherit gnus-emphasis-bold-italic :underline t)))
       `(gnus-emphasis-underline-italic ((,class :inherit gnus-emphasis-italic :underline t)))
       
       `(gnus-header-from ((,class :inherit mu4e-contact-face :underline t)))
       `(gnus-header-name ((,class :inherit mu4e-header-key-face)))
       `(gnus-header-content ((,class :inherit mu4e-header-value-face)))
       `(gnus-header-newsgroups ((,class :inherit message-header-newsgroups)))
       `(gnus-header-subject ((,class :inherit mu4e-header-value-face)))
       `(gnus-splash ((,class :inherit shadow)))

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
