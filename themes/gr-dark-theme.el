;;; gr-dark-theme.el --- Custom face theme for Emacs  -*- lexical-binding:t -*-


;;; Commentary:

;; To use it, put the following in your Emacs configuration file:
;;
;;   (load-theme 'gr-dark t)


;;; Code:

(deftheme gr-dark)

(put 'gr-dark 'theme-immediate t)

;;(let ((c '((class color) (min-colors 89))))

(custom-theme-set-faces
 'gr-dark

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
                        :background "#212121"
                        :foreground "gray75"
                        :stipple nil
                        :inherit nil))))

 `(fixed-pitch ((t (:family "Monospace"
                            :background "#212121"
                            :foreground "gray75"))))

 `(variable-pitch ((t (:family "Times New Roman"
                               ;; "Cardo" "Bembo Book MT std"
                               ;; "ET Bembo"
                               :height 1.3
                               :background "#212121"
                               :foreground "gray75"))))

;;;; basics

 `(cursor ((t (:background "gray75"))))
 `(fringe ((t (:background "gray50"))))
 `(highlight ((t (:background "#454545" :foreground "#ffffff" :underline t))))

 `(minibuffer-prompt ((t (:foreground "#e5786d"))))
 `(escape-glyph ((t (:foreground "#ddaa6f" :weight bold))))
 `(homoglyph ((t (:foreground "#ddaa6f" :weight bold))))

;;;; tab-bar
 `(tab-bar ((t (:font "Menlo" :height 110))))
 `(tab-bar-tab ((t (:background "gray30" :box (:line-width 1 :style released-button)))))
 `(tab-bar-tab-inactive ((t (:background "gray20" :box (:line-width 1 :style pressed-button)))))

;;;; region

 `(region ((t (:background "#444444" :foreground "#f6f3e8"))))
 `(secondary-selection ((t (:background "#333366" :foreground "#f6f3e8"))))


;;;; isearch

 `(isearch ((t (:background "#343434" :foreground "#857b6f"))))
 `(lazy-highlight ((t (:background "#384048" :foreground "#a0a8b0"))))


;;;; mode-line
 `(mode-line ((t (:background "#444444" :foreground "#f6f3e8"))))
 `(mode-line-inactive ((t (:background "#444444" :foreground "#857b6f"))))



;;;; font-lock
 `(font-lock-builtin-face ((t (:foreground "#e5786d"))))
 `(font-lock-comment-face ((t (:foreground "#99968b"))))
 `(font-lock-constant-face ((t (:foreground "#e5786d"))))
 `(font-lock-function-name-face ((t (:foreground "#cae682"))))
 `(font-lock-keyword-face ((t (:foreground "#8ac6f2" :weight bold))))
 `(font-lock-string-face ((t (:foreground "#95e454"))))
 `(font-lock-type-face ((t (:foreground "#92a65e" :weight bold))))
 `(font-lock-variable-name-face ((t (:foreground "#cae682"))))
 `(font-lock-warning-face ((t (:foreground "#ccaa8f"))))

 ;; Help faces
 `(help-key-binding ((t (:background "#333333" :foreground "#f6f3e8"))))

;;;; buttons / links
 `(link ((t (:foreground "#8ac6f2" :underline t))))
 `(link-visited ((t (:foreground "#e5786d" :underline t))))
 `(button ((t (:background "#333333" :foreground "#f6f3e8"))))
 `(header-line ((t (:background "#303030" :foreground "#e7f6da"))))

;;;; org-mode

 `(org-hide ((t (:foreground "#212121"))))
 `(org-meta-line ((t (:inherit fixed-pitch :height 1 :foreground "#e5786d"))))
 `(org-block ((t (:inherit fixed-pitch))))
 `(org-document-info-keyword ((t (:inherit fixed-pitch :foreground "gray50" :height 1))))
 `(org-table ((t (:inherit fixed-pitch :height .8))))
 `(org-code ((t (:inherit fixed-pitch))))
 `(org-block ((t (:inherit fixed-pitch))))
 `(org-ellipsis ((t (:inherit fixed-pitch :foreground "gray50" :underline nil :height 1.1))))
 `(org-todo ((t (:family "Menlo" :weight bold :height 1 :foreground "#e5786d"))))
 `(org-done ((t (:family "Menlo" :weight bold :height 1 :foreground "#95e454"))))

 `(outline-1 ((t (:foreground "#95e454" :weight bold :underline t))))
 `(outline-2 ((t (:foreground "#99968b" :weight bold :underline t))))
 `(outline-3 ((t (:foreground "gray90" :underline t))))
 `(outline-4 ((t (:foreground "gray90" :underline t))))
 `(outline-5 ((t (:foreground "gray90" :underline t))))
 `(outline-6 ((t (:foreground "gray90"))))
 `(outline-7 ((t (:foreground "gray90"))))
 `(outline-8 ((t (:foreground "gray90"))))

 `(org-level-1 ((t (:inherit outline-1 :height 130))))
 `(org-level-2 ((t (:inherit outline-2))))
 `(org-level-3 ((t (:inherit outline-3))))
 `(org-level-4 ((t (:inherit outline-4))))
 `(org-level-5 ((t (:inherit outline-5))))
 `(org-level-6 ((t (:inherit outline-6))))
 `(org-level-7 ((t (:inherit outline-7))))
 `(org-level-8 ((t (:inherit outline-9))))

;;;; mail

 `(gnus-group-news-1 ((t (:weight bold :foreground "#95e454"))))
 `(gnus-group-news-1-low ((t (:foreground "#95e454"))))
 `(gnus-group-news-2 ((t (:weight bold :foreground "#cae682"))))
 `(gnus-group-news-2-low ((t (:foreground "#cae682"))))
 `(gnus-group-news-3 ((t (:weight bold :foreground "#ccaa8f"))))
 `(gnus-group-news-3-low ((t (:foreground "#ccaa8f"))))
 `(gnus-group-news-4 ((t (:weight bold :foreground "#99968b"))))
 `(gnus-group-news-4-low ((t (:foreground "#99968b"))))
 `(gnus-group-news-5 ((t (:weight bold :foreground "#cae682"))))
 `(gnus-group-news-5-low ((t (:foreground "#cae682"))))
 `(gnus-group-news-low ((t (:foreground "#99968b"))))
 `(gnus-group-mail-1 ((t (:weight bold :foreground "#95e454"))))
 `(gnus-group-mail-1-low ((t (:foreground "#95e454"))))
 `(gnus-group-mail-2 ((t (:weight bold :foreground "#cae682"))))
 `(gnus-group-mail-2-low ((t (:foreground "#cae682"))))
 `(gnus-group-mail-3 ((t (:weight bold :foreground "#ccaa8f"))))
 `(gnus-group-mail-3-low ((t (:foreground "#ccaa8f"))))
 `(gnus-group-mail-low ((t (:foreground "#99968b"))))
 `(gnus-header-content ((t (:foreground "#8ac6f2"))))
 `(gnus-header-from ((t (:weight bold :foreground "#95e454"))))
 `(gnus-header-subject ((t (:foreground "#cae682"))))
 `(gnus-header-name ((t (:foreground "#8ac6f2"))))
 `(gnus-header-newsgroups ((t (:foreground "#cae682"))))
 ;; Message faces
 `(message-header-name ((t (:foreground "#8ac6f2" :weight bold))))
 `(message-header-cc ((t (:foreground "#95e454"))))
 `(message-header-other ((t (:foreground "#95e454"))))
 `(message-header-subject ((t (:foreground "#cae682"))))
 `(message-header-to ((t (:foreground "#cae682"))))
 `(message-cited-text ((t (:foreground "#99968b"))))
 `(message-separator ((t (:foreground "#e5786d" :weight bold))))

 )



;;;; provide

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; add theme folder to `custom-theme-load-path' when installing over MELPA
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(custom-theme-set-variables
 'gr-dark
 '(ansi-color-names-vector ["#212121" "#e5786d" "#95e454" "#cae682"
			    "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]))

(provide-theme 'gr-dark)

(provide 'gr-dark-theme)

;;; gr-dark-theme.el ends here
