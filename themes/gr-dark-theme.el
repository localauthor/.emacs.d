;;; gr-dark-theme.el --- Custom face theme for Emacs  -*- lexical-binding:t -*-

;;; Code:

(deftheme gr-dark)

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'gr-dark
   `(default ((,class (:family "JetBrains Mono" ;;"IBM Plex Mono" "Menlo"
                               :height 120 :background "#212121" :foreground "grey90"))))
   `(fixed-pitch ((,class (:family "Monospace" :height 130 :background "#212121" :foreground "#f6f3e8"))))
   `(variable-pitch ((,class (:family "Times New Roman" :height 1.2 :background "#212121" :foreground "#f6f3e8"))))
   `(cursor ((,class (:background "#656565"))))
   ;; Highlighting faces
   `(fringe ((,class (:background "#303030"))))
   `(highlight ((,class (:background "#454545" :foreground "#ffffff"
			 :underline t))))
   `(region ((,class (:background "#444444" :foreground "#f6f3e8"))))
   `(secondary-selection ((,class (:background "#333366" :foreground "#f6f3e8"))))
   `(isearch ((,class (:background "#343434" :foreground "#857b6f"))))
   `(lazy-highlight ((,class (:background "#384048" :foreground "#a0a8b0"))))
   ;; Mode line faces
   `(mode-line ((,class (:background "#444444" :foreground "#f6f3e8"))))
   `(mode-line-inactive ((,class (:background "#444444" :foreground "#857b6f"))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground "#e5786d"))))
   `(escape-glyph ((,class (:foreground "#ddaa6f" :weight bold))))
   `(homoglyph ((,class (:foreground "#ddaa6f" :weight bold))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground "#e5786d"))))
   `(font-lock-comment-face ((,class (:foreground "#99968b"))))
   `(font-lock-constant-face ((,class (:foreground "#e5786d"))))
   `(font-lock-function-name-face ((,class (:foreground "#cae682"))))
   `(font-lock-keyword-face ((,class (:foreground "#8ac6f2" :weight bold))))
   `(font-lock-string-face ((,class (:foreground "#95e454"))))
   `(font-lock-type-face ((,class (:foreground "#92a65e" :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground "#cae682"))))
   `(font-lock-warning-face ((,class (:foreground "#ccaa8f"))))
   ;; Help faces
   `(help-key-binding ((,class (:background "#333333" :foreground "#f6f3e8"))))
   ;; Button and link faces
   `(link ((,class (:foreground "#8ac6f2" :underline t))))
   `(link-visited ((,class (:foreground "#e5786d" :underline t))))
   `(button ((,class (:background "#333333" :foreground "#f6f3e8"))))
   `(header-line ((,class (:background "#303030" :foreground "#e7f6da"))))
   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-group-news-1-low ((,class (:foreground "#95e454"))))
   `(gnus-group-news-2 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-2-low ((,class (:foreground "#cae682"))))
   `(gnus-group-news-3 ((,class (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-news-3-low ((,class (:foreground "#ccaa8f"))))
   `(gnus-group-news-4 ((,class (:weight bold :foreground "#99968b"))))
   `(gnus-group-news-4-low ((,class (:foreground "#99968b"))))
   `(gnus-group-news-5 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-5-low ((,class (:foreground "#cae682"))))
   `(gnus-group-news-low ((,class (:foreground "#99968b"))))
   `(gnus-group-mail-1 ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-group-mail-1-low ((,class (:foreground "#95e454"))))
   `(gnus-group-mail-2 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-mail-2-low ((,class (:foreground "#cae682"))))
   `(gnus-group-mail-3 ((,class (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-mail-3-low ((,class (:foreground "#ccaa8f"))))
   `(gnus-group-mail-low ((,class (:foreground "#99968b"))))
   `(gnus-header-content ((,class (:foreground "#8ac6f2"))))
   `(gnus-header-from ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-header-subject ((,class (:foreground "#cae682"))))
   `(gnus-header-name ((,class (:foreground "#8ac6f2"))))
   `(gnus-header-newsgroups ((,class (:foreground "#cae682"))))
   ;; Message faces
   `(message-header-name ((,class (:foreground "#8ac6f2" :weight bold))))
   `(message-header-cc ((,class (:foreground "#95e454"))))
   `(message-header-other ((,class (:foreground "#95e454"))))
   `(message-header-subject ((,class (:foreground "#cae682"))))
   `(message-header-to ((,class (:foreground "#cae682"))))
   `(message-cited-text ((,class (:foreground "#99968b"))))
   `(message-separator ((,class (:foreground "#e5786d" :weight bold))))

   `(org-hide ((,class (:foreground "#212121"))))
   `(org-meta-line ((,class (:inherit fixed-pitch :height 1 :foreground "#e5786d"))))
   `(org-block ((,class (:inherit fixed-pitch))))
   `(org-document-info-keyword ((,class (:inherit fixed-pitch :foreground "grey50" :height 1))))
   `(org-table ((,class (:inherit fixed-pitch :height .8))))
   `(org-code ((,class (:inherit fixed-pitch))))
   `(org-block ((,class (:inherit fixed-pitch))))
   `(org-ellipsis ((,class (:inherit fixed-pitch :foreground "grey50" :underline nil :height 1.1))))
   `(org-todo ((,class (:family "Menlo" :weight bold :height 1 :foreground "#e5786d"))))
   `(org-done ((,class (:family "Menlo" :weight bold :height 1 :foreground "#95e454"))))

   `(outline-1 ((,class (:foreground "grey90"))))
   `(outline-2 ((,class (:foreground "grey90"))))
   `(outline-3 ((,class (:height 1 :foreground "grey90"))))
   `(outline-4 ((,class (:height 1 :foreground "grey90"))))
   `(outline-5 ((,class (:height 1 :foreground "grey90"))))
   `(outline-6 ((,class (:height 1 :foreground "grey90"))))
   `(outline-7 ((,class (:height 1 :foreground "grey90"))))
   `(outline-8 ((,class (:height 1 :foreground "grey90"))))

   `(outshine-level-1 ((,class (:height 1 :foreground "grey90"))))
   `(outshine-level-2 ((,class (:weight bold :underline t :foreground "grey90"))))
   `(outshine-level-3 ((,class (:height 1 :foreground "grey90"))))
   `(outshine-level-4 ((,class (:height 1 :foreground "grey90"))))
   `(outshine-level-5 ((,class (:height 1 :foreground "grey90"))))
   `(outshine-level-6 ((,class (:height 1 :foreground "grey90"))))
   `(outshine-level-7 ((,class (:height 1 :foreground "grey90"))))
   `(outshine-level-8 ((,class (:height 1 :foreground "grey90"))))

   `(org-level-1 ((,class (:inherit outline-1 :height 1 :foreground "grey90"))))
   `(org-level-2 ((,class (:inherit outline-2 :weight bold :underline t :foreground "grey90"))))
   `(org-level-3 ((,class (:inherit outline-3))))
   `(org-level-4 ((,class (:inherit outline-4))))
   `(org-level-5 ((,class (:inherit outline-5))))
   `(org-level-6 ((,class (:inherit outline-6))))
   `(org-level-7 ((,class (:inherit outline-7))))
   `(org-level-8 ((,class (:inherit outline-9))))

   ;; `(org-level-1 ((,class (:inherit outline-1
   ;;                       :foreground "#95e454"
   ;;                       :weight bold
   ;;                       :family "Helvetica"
   ;;                       :underline t
   ;;                       :height 150))))
   ;; `(org-level-2 ((,class (:inherit outline-2
   ;;                         ;; :family "Monospace" :height 130
   ;;                         :family "Franklin Gothic Medium" :height 150
   ;;                         :foreground "#95e454"))))
   ;; `(org-level-3 ((,class (:inherit fixed-pitch
   ;;                         :family "Times New Roman"
   ;;                         :height 1.1
   ;;                         :foreground "#f6f3e8"))))
   ;; `(org-level-4 ((,class (:inherit fixed-pitch
   ;;                         :family "Times New Roman"
   ;;                         :height 1.1
   ;;                         :foreground "#f6f3e8"))))
   ;; `(org-level-5 ((,class (:inherit fixed-pitch
   ;;                         :family "Times New Roman"
   ;;                         :height 1.1
   ;;                         :foreground "#f6f3e8"))))
   ;; `(org-level-6 ((,class (:inherit fixed-pitch
   ;;                         :family "Times New Roman"
   ;;                         :height 1.1
   ;;                         :foreground "#f6f3e8"))))
   ;; `(org-level-7 ((,class (:inherit fixed-pitch
   ;;                         :family "Times New Roman"
   ;;                         :height 1.1
   ;;                         :foreground "#f6f3e8"))))
   ;; `(org-level-8 ((,class (:inherit fixed-pitch
   ;;                         :family "Times New Roman"
   ;;                         :height 1.1
   ;;                         :foreground "#f6f3e8"))))
  ))


(custom-theme-set-variables
 'gr-dark
 '(ansi-color-names-vector ["#212121" "#e5786d" "#95e454" "#cae682"
			    "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]))

(provide-theme 'gr-dark)

;;; gr-dark-theme.el ends here
