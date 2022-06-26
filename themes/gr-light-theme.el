;;; gr-light-theme.el --- A light theme  -*- lexical-binding: t; -*-

;;; Commentary:

;; To use it, put the following in your Emacs configuration file:
;;
;;   (load-theme 'gr-light t)

;;; Code:

(deftheme gr-light)

(display-color-cells (selected-frame))

(let* ((class '((class color) (min-colors 89)))
       (256color (eq (display-color-cells (selected-frame)) 256))
       (truecolor (eq (display-color-cells (selected-frame)) 16777216))

       (background (if (or window-system truecolor) "#FAFAFA" "#FAFAFA")) ;; sidebar-container
       ;; (current-line (if (or window-system truecolor)  "#ECEFF1" "#dadada")) ;; tree-row
       ;; (far-background (if (or window-system truecolor)  "#e0f7fa" "#e0f7fa")) ;; panel-control
       ;; (inactive-gray (if (or window-system truecolor) "#cfd8dc" "#cfd8dc"))
       ;; (header-color (if (or window-system truecolor) "#C8E6C9" "#C8E6C9"))
       ;; (subtle "#a7adba") ;; tree-row-hover-disclosure-button-control
       ;; (selection "#90A4AE") ;; tab-control-dirty-tab-close-button
       ;; (secondary-selection "#bf616a") ;; tab-control-hover-tab-close-button
       (foreground "#212121"))
       ;; (comment "#607d8b") ;; table-row
       ;; (red "#B71C1C") ;; tab-control-hover-tab-close-button
       ;; (orange "#FF5722") ;; darker tab-control-dirty-tab-close-butto
       ;; (yellow "#FFA000") ;; tab-control-dirty-tab-close-button
       ;; (green "#558b2f") ;; complement tab-control-dirty-tab-close-button
       ;; (aqua "#00796b") ;; lighter complement tab-control-dirty-tab-close-button
       ;; (blue "#2196f3") ;; complement tab-control-dirty-tab-close-button
       ;; (purple "#4527A0") ;; complement tab-control-dirty-tab-close-button

(custom-theme-set-faces
 'gr-light
 '(default ((t (:family "JetBrains Mono" ;;"IBM Plex Mono"
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
 '(fixed-pitch ((t (:family "Monospace" :height .9 :foreground "#212121" :background "#FAFAFA"))))
 '(variable-pitch ((t (:family "Times New Roman" :height 1.2 :foreground "#212121" :background "#FAFAFA"))))

 '(cursor ((((background light)) (:background "black")) (((background dark)) (:background "white"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "medium blue"))))
 '(highlight ((((class color) (min-colors 88) (background light)) (:background "darkseagreen2")) (((class color) (min-colors 88) (background dark)) (:background "darkolivegreen")) (((class color) (min-colors 16) (background light)) (:background "darkseagreen2")) (((class color) (min-colors 16) (background dark)) (:background "darkolivegreen")) (((class color) (min-colors 8)) (:foreground "black" :background "green")) (t (:inverse-video t))))
 '(region ((t (:extend t :background "#a9a9b8"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:extend t :background "yellow1")) (((class color) (min-colors 88) (background dark)) (:extend t :background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:extend t :background "yellow")) (((class color) (min-colors 16) (background dark)) (:extend t :background "SkyBlue4")) (((class color) (min-colors 8)) (:extend t :foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))

 '(font-lock-builtin-face ((((class grayscale) (background light)) (:weight bold :foreground "LightGray")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "dark slate blue")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSteelBlue")) (((class color) (min-colors 16) (background light)) (:foreground "Orchid")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue")) (((class color) (min-colors 8)) (:weight bold :foreground "blue")) (t (:weight bold))))

 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))

 '(font-lock-comment-face ((((class grayscale) (background light)) (:slant italic :weight bold :foreground "DimGray")) (((class grayscale) (background dark)) (:slant italic :weight bold :foreground "LightGray")) (((class color) (min-colors 88) (background light)) (:foreground "Firebrick")) (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1")) (((class color) (min-colors 16) (background light)) (:foreground "red")) (((class color) (min-colors 16) (background dark)) (:foreground "red1")) (((class color) (min-colors 8) (background light)) (:foreground "red")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow")) (t (:slant italic :weight bold))))

 '(font-lock-constant-face ((((class grayscale) (background light)) (:underline (:color foreground-color :style line) :weight bold :foreground "LightGray")) (((class grayscale) (background dark)) (:underline (:color foreground-color :style line) :weight bold :foreground "Gray50")) (((class color) (min-colors 88) (background light)) (:foreground "dark cyan")) (((class color) (min-colors 88) (background dark)) (:foreground "Aquamarine")) (((class color) (min-colors 16) (background light)) (:foreground "CadetBlue")) (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine")) (((class color) (min-colors 8)) (:foreground "magenta")) (t (:underline (:color foreground-color :style line) :weight bold))))

 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))

 '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "Blue1")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue")) (((class color) (min-colors 16) (background light)) (:foreground "Blue")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue")) (((class color) (min-colors 8)) (:weight bold :foreground "blue")) (t (:weight bold :inverse-video t))))

 '(font-lock-keyword-face ((((class grayscale) (background light)) (:weight bold :foreground "LightGray")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "Purple")) (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1")) (((class color) (min-colors 16) (background light)) (:foreground "Purple")) (((class color) (min-colors 16) (background dark)) (:foreground "Cyan")) (((class color) (min-colors 8)) (:weight bold :foreground "cyan")) (t (:weight bold))))

 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((((class grayscale) (background light)) (:slant italic :foreground "DimGray")) (((class grayscale) (background dark)) (:slant italic :foreground "LightGray")) (((class color) (min-colors 88) (background light)) (:foreground "VioletRed4")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSalmon")) (((class color) (min-colors 16) (background light)) (:foreground "RosyBrown")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon")) (((class color) (min-colors 8)) (:foreground "green")) (t (:slant italic))))

 '(font-lock-type-face ((((class grayscale) (background light)) (:weight bold :foreground "Gray90")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 16) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 8)) (:foreground "green")) (t (:underline (:color foreground-color :style line) :weight bold))))
 '(font-lock-variable-name-face ((((class grayscale) (background light)) (:slant italic :weight bold :foreground "Gray90")) (((class grayscale) (background dark)) (:slant italic :weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "sienna")) (((class color) (min-colors 88) (background dark)) (:foreground "LightGoldenrod")) (((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod")) (((class color) (min-colors 16) (background dark)) (:foreground "LightGoldenrod")) (((class color) (min-colors 8)) (:weight light :foreground "yellow")) (t (:slant italic :weight bold))))
 '(font-lock-warning-face ((t (:inherit (error)))))
 '(button ((t (:inherit (link)))))
 '(link ((((class color) (min-colors 88) (background light)) (:underline (:color foreground-color :style line) :foreground "RoyalBlue3")) (((class color) (background light)) (:underline (:color foreground-color :style line) :foreground "blue")) (((class color) (min-colors 88) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan1")) (((class color) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan")) (t (:inherit (underline)))))
 '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
 '(fringe ((((class color) (background light)) (:background "grey95")) (((class color) (background dark)) (:background "grey10")) (t (:background "gray"))))
 '(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90")) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "black" :background "white")) (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "white" :background "black"))))
 '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))

 ;; comment out the following

 ;; '(mode-line ((((class color) (min-colors 88)) (:foreground "black"
 ;; :background "grey75" :box (:line-width (1 . -1) :color nil :style
 ;; released-button))) (t (:inverse-video t))))

 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))


 ;; comment out the following

 ;; '(mode-line-highlight ((((supports :box t) (class color) (min-colors 88)) (:box (:line-width (2 . 2) :color "grey40" :style released-button))) (t (:inherit (highlight)))))

 ;; comment out the following

 ;; '(mode-line-inactive
 ;;   ((default (:inherit (mode-line)))
 ;;    (((class color) (min-colors 88) (background light)) (:background "grey90" :foreground "grey20" :box (:line-width (1 . -1) :color "grey75" :style nil) :weight light))
 ;;    (((class color) (min-colors 88) (background dark)) (:background "grey30" :foreground "grey80" :box (:line-width (1 . -1) :color "grey40" :style nil) :weight light))))

 '(isearch ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3")) (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred2")) (((class color) (min-colors 16)) (:foreground "cyan1" :background "magenta4")) (((class color) (min-colors 8)) (:foreground "cyan1" :background "magenta4")) (t (:inverse-video t))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4")) (((class color) (min-colors 16)) (:background "turquoise3")) (((class color) (min-colors 8)) (:background "turquoise3")) (t (:underline (:color foreground-color :style line)))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "khaki1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))


   `(org-hide ((,class (:foreground "white"))))
   `(org-meta-line ((,class (:inherit fixed-pitch :height 1 :foreground "Firebrick"))))
   `(org-block ((,class (:inherit fixed-pitch))))
   `(org-document-info-keyword ((,class (:inherit fixed-pitch :foreground "grey50" :height 1))))
   `(org-table ((,class (:inherit fixed-pitch :height .8))))
   `(org-code ((,class (:inherit fixed-pitch))))
   `(org-block ((,class (:inherit fixed-pitch))))
   `(org-ellipsis ((,class (:inherit fixed-pitch :foreground "grey50" :underline nil :height 1.1))))
   `(org-todo ((,class (:family "Menlo" :weight bold :height 1 :foreground "Red1"))))
   `(org-done ((,class (:family "Menlo" :weight bold :height 1 :foreground "ForestGreen"))))

   `(outline-1 ((, class (:foreground "blue3" :weight bold :underline t))))
   `(outline-2 ((, class (:foreground "black" :weight bold :underline t))))
   `(outline-3 ((, class (:underline t))))

   `(org-level-1 ((,class (:inherit outline-1 :height 130))))
   `(org-level-2 ((,class (:inherit outline-2))))
   `(org-level-3 ((,class (:inherit outline-3))))
   `(org-level-4 ((,class (:inherit outline-4))))
   `(org-level-5 ((,class (:inherit outline-5))))
   `(org-level-6 ((,class (:inherit outline-6))))
   `(org-level-7 ((,class (:inherit outline-7))))
   `(org-level-8 ((,class (:inherit outline-9))))

   ;; `(org-level-1 ((,class (:inherit outline-1
   ;;                         :foreground "navy blue"
   ;;                         :weight bold
   ;;                         :family "Helvetica"
   ;;                         :underline t
   ;;                         :height 1.03))))
   ;; `(org-level-2 ((,class (:inherit outline-2
   ;;                         :family "Franklin Gothic Medium"
   ;;                         :height 1.01
   ;;                         :foreground "black"))))
   ;; `(org-level-3 ((,class (:inherit fixed-pitch
   ;;                         :family "Times New Roman"
   ;;                         :height 1.1
   ;;                         :foreground "black"))))
   ;; `(org-level-4 ((,class (:inherit fixed-pitch
   ;;                         :family "Times New Roman"
   ;;                         :height 1.1
   ;;                         :foreground "black"))))
   ;; `(org-level-5 ((,class (:inherit fixed-pitch
   ;;                         :family "Times New Roman"
   ;;                         :height 1.1
   ;;                         :foreground "black"))))
   ;; `(org-level-6 ((,class (:inherit fixed-pitch
   ;;                         :family "Times New Roman"
   ;;                         :height 1.1
   ;;                         :foreground "black"))))
   ;; `(org-level-7 ((,class (:inherit fixed-pitch
   ;;                         :family "Times New Roman"
   ;;                         :height 1.1
   ;;                         :foreground "black"))))
   ;; `(org-level-8 ((,class (:inherit fixed-pitch
   ;;                         :family "Times New Roman"
   ;;                         :height 1.1
   ;;                         :foreground "black"))))


  ;; `(org-level-2 ((,class (:inherit outline-2
  ;;                          :family "Franklin Gothic Medium"
  ;;                          :height 150
  ;;                          :foreground "black"))))
  ;;  `(org-level-3 ((,class (:inherit outline-3
  ;;                          :family "Helvetica"
  ;;                          :height 130
  ;;                          :foreground "black"))))
  ;;  `(org-level-4 ((,class (:inherit outline-4
  ;;                          :family "Helvetica"
  ;;                          :height 130
  ;;                          :foreground "black"))))
  ;;  `(org-level-5 ((,class (:inherit outline-5
  ;;                          :family "Helvetica"
  ;;                          :height 130
  ;;                          :foreground "black"))))
  ;;  `(org-level-6 ((,class (:inherit outline-6
  ;;                          :family "Helvetica"
  ;;                          :height 130
  ;;                          :foreground "black"))))
  ;;  `(org-level-7 ((,class (:inherit outline-7
  ;;                          :family "Helvetica"
  ;;                          :height 130
  ;;                          :foreground "black"))))
  ;;  `(org-level-8 ((,class (:inherit outline-8
  ;;                          :family "Helvetica"
  ;;                          :height 130
  ;;                          :foreground "black"))))
  ;;  `(org-level-9 ((,class (:inherit outline-9
  ;;                          :family "Helvetica"
  ;;                          :height 130
  ;;                          :foreground "black"))))

   ;; Powerline
   `(powerline-active1 ((t (:foreground ,foreground :background "Gray60"))))
   `(powerline-active2 ((t (:foreground ,foreground :background "Gray80"))))
;;   `(powerline-inactive1 ((t (:foreground ,comment :background ,selection))))
;;   `(powerline-inactive2 ((t (:foreground ,comment :background ,selection))))


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
    ;; `(gnus-cite-attribution ((,class :inherit italic :foreground ,fg-main)))
    `(gnus-emphasis-bold ((,class :inherit bold)))
    `(gnus-emphasis-bold-italic ((,class :inherit bold-italic)))
    `(gnus-emphasis-highlight-words ((,class :inherit hi-yellow)))
    `(gnus-emphasis-italic ((,class :inherit italic)))
    `(gnus-emphasis-underline-bold ((,class :inherit gnus-emphasis-bold :underline t)))
    `(gnus-emphasis-underline-bold-italic ((,class :inherit gnus-emphasis-bold-italic :underline t)))
    `(gnus-emphasis-underline-italic ((,class :inherit gnus-emphasis-italic :underline t)))
    ;; `(gnus-group-mail-1 ((,class :inherit bold :foreground ,magenta-alt)))
    ;; `(gnus-group-mail-1-empty ((,class :foreground ,magenta-alt)))
    ;; `(gnus-group-mail-2 ((,class :inherit bold :foreground ,magenta)))
    ;; `(gnus-group-mail-2-empty ((,class :foreground ,magenta)))
    ;; `(gnus-group-mail-3 ((,class :inherit bold :foreground ,magenta-alt-other)))
    ;; `(gnus-group-mail-3-empty ((,class :foreground ,magenta-alt-other)))
    ;; `(gnus-group-mail-low ((,class :inherit bold :foreground ,magenta-nuanced-fg)))
    ;; `(gnus-group-mail-low-empty ((,class :foreground ,magenta-nuanced-fg)))
    ;; `(gnus-group-news-1 ((,class :inherit bold :foreground ,green)))
    ;; `(gnus-group-news-1-empty ((,class :foreground ,green)))
    ;; `(gnus-group-news-2 ((,class :inherit bold :foreground ,cyan)))
    ;; `(gnus-group-news-2-empty ((,class :foreground ,cyan)))
    ;; `(gnus-group-news-3 ((,class :inherit bold :foreground ,yellow-nuanced-fg)))
    ;; `(gnus-group-news-3-empty ((,class :foreground ,yellow-nuanced-fg)))
    ;; `(gnus-group-news-4 ((,class :inherit bold :foreground ,cyan-nuanced-fg)))
    ;; `(gnus-group-news-4-empty ((,class :foreground ,cyan-nuanced-fg)))
    ;; `(gnus-group-news-5 ((,class :inherit bold :foreground ,red-nuanced-fg)))
    ;; `(gnus-group-news-5-empty ((,class :foreground ,red-nuanced-fg)))
    ;; `(gnus-group-news-6 ((,class :inherit bold :foreground ,fg-alt)))
    ;; `(gnus-group-news-6-empty ((,class :inherit shadow)))
    ;; `(gnus-group-news-low ((,class :inherit bold :foreground ,green-nuanced-fg)))
    ;; `(gnus-group-news-low-empty ((,class :foreground ,green-nuanced-fg)))
    `(gnus-header-from ((,class :inherit mu4e-contact-face :underline t)))
    `(gnus-header-name ((,class :inherit mu4e-header-key-face)))
    `(gnus-header-content ((,class :inherit mu4e-header-value-face)))
    `(gnus-header-newsgroups ((,class :inherit message-header-newsgroups)))
    `(gnus-header-subject ((,class :inherit mu4e-header-value-face)))
    ;; `(gnus-server-agent ((,class :inherit bold :foreground ,cyan)))
    ;; `(gnus-server-closed ((,class :inherit bold :foreground ,magenta)))
    ;; `(gnus-server-cloud ((,class :inherit bold :foreground ,cyan-alt)))
    ;; `(gnus-server-cloud-host ((,class :inherit modus-themes-refine-cyan)))
    ;; `(gnus-server-denied ((,class :inherit bold :foreground ,red)))
    ;; `(gnus-server-offline ((,class :inherit bold :foreground ,yellow)))
    ;; `(gnus-server-opened ((,class :inherit bold :foreground ,green)))
    ;; `(gnus-signature ((,class :inherit italic :foreground ,fg-special-cold)))
    `(gnus-splash ((,class :inherit shadow)))
    ;; `(gnus-summary-cancelled ((,class :inherit modus-themes-mark-alt :extend t)))
    ;; `(gnus-summary-high-ancient ((,class :inherit bold :foreground ,fg-alt)))
    ;; `(gnus-summary-high-read ((,class :inherit bold :foreground ,fg-special-cold)))
    ;; `(gnus-summary-high-ticked ((,class :inherit bold :foreground ,red-alt-other)))
    ;; `(gnus-summary-high-undownloaded ((,class :inherit bold :foreground ,yellow)))
    ;; `(gnus-summary-high-unread ((,class :inherit bold :foreground ,fg-main)))
    ;; `(gnus-summary-low-ancient ((,class :inherit italic :foreground ,fg-alt)))
    ;; `(gnus-summary-low-read ((,class :inherit italic :foreground ,fg-alt)))
    ;; `(gnus-summary-low-ticked ((,class :inherit italic :foreground ,red-refine-fg)))
    ;; `(gnus-summary-low-undownloaded ((,class :inherit italic :foreground ,yellow-refine-fg)))
    ;; `(gnus-summary-low-unread ((,class :inherit bold :foreground ,fg-special-cold)))
    ;; `(gnus-summary-normal-ancient ((,class :foreground ,fg-special-calm)))
    ;; `(gnus-summary-normal-read ((,class :inherit shadow)))
    ;; `(gnus-summary-normal-ticked ((,class :foreground ,red-alt-other)))
    ;; `(gnus-summary-normal-undownloaded ((,class :foreground ,yellow)))
    ;; `(gnus-summary-normal-unread ((,class :foreground ,fg-main)))
    ;; `(gnus-summary-selected ((,class :inherit modus-themes-subtle-blue :extend t)))

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
