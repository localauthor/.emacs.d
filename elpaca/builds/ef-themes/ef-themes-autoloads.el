;;; ef-themes-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:



;;; Generated autoloads from ef-autumn-theme.el

(put 'ef-autumn 'theme-properties '(:background-mode dark :kind color-scheme :family ef))


;;; Generated autoloads from ef-bio-theme.el

(put 'ef-bio 'theme-properties '(:background-mode dark :kind color-scheme :family ef))


;;; Generated autoloads from ef-cherie-theme.el

(put 'ef-cherie 'theme-properties '(:background-mode dark :kind color-scheme :family ef))


;;; Generated autoloads from ef-cyprus-theme.el

(put 'ef-cyprus 'theme-properties '(:background-mode light :kind color-scheme :family ef))


;;; Generated autoloads from ef-dark-theme.el

(put 'ef-dark 'theme-properties '(:background-mode dark :kind color-scheme :family ef))


;;; Generated autoloads from ef-day-theme.el

(put 'ef-day 'theme-properties '(:background-mode light :kind color-scheme :family ef))


;;; Generated autoloads from ef-deuteranopia-dark-theme.el

(put 'ef-deuteranopia-dark 'theme-properties '(:background-mode dark :kind color-scheme :family ef))


;;; Generated autoloads from ef-deuteranopia-light-theme.el

(put 'ef-deuteranopia-light 'theme-properties '(:background-mode light :kind color-scheme :family ef))


;;; Generated autoloads from ef-duo-dark-theme.el

(put 'ef-duo-dark 'theme-properties '(:background-mode dark :kind color-scheme :family ef))


;;; Generated autoloads from ef-duo-light-theme.el

(put 'ef-duo-light 'theme-properties '(:background-mode light :kind color-scheme :family ef))


;;; Generated autoloads from ef-frost-theme.el

(put 'ef-frost 'theme-properties '(:background-mode light :kind color-scheme :family ef))


;;; Generated autoloads from ef-light-theme.el

(put 'ef-light 'theme-properties '(:background-mode light :kind color-scheme :family ef))


;;; Generated autoloads from ef-night-theme.el

(put 'ef-night 'theme-properties '(:background-mode dark :kind color-scheme :family ef))


;;; Generated autoloads from ef-spring-theme.el

(put 'ef-spring 'theme-properties '(:background-mode light :kind color-scheme :family ef))


;;; Generated autoloads from ef-summer-theme.el

(put 'ef-summer 'theme-properties '(:background-mode light :kind color-scheme :family ef))


;;; Generated autoloads from ef-themes.el

(autoload 'ef-themes-select "ef-themes" "\
Load an Ef THEME using minibuffer completion.

With optional VARIANT as a prefix argument, prompt to limit the
set of themes to either dark or light variants.

Run `ef-themes-post-load-hook' after loading the theme.

When called from Lisp, THEME is the symbol of a theme.  VARIANT
is ignored in this scenario.

(fn THEME &optional VARIANT)" t)
(autoload 'ef-themes-toggle "ef-themes" "\
Toggle between the two `ef-themes-to-toggle'.
If `ef-themes-to-toggle' does not specify two Ef themes, inform
the user about it while prompting with completion for a theme
among our collection (this is practically the same as the
`ef-themes-select' command).

Run `ef-themes-post-load-hook' after loading the theme." t)
(autoload 'ef-themes-load-random "ef-themes" "\
Load an Ef theme at random, excluding the current one.

With optional VARIANT as a prefix argument, prompt to limit the
set of themes to either dark or light variants.

Run `ef-themes-post-load-hook' after loading the theme.

When called from Lisp, VARIANT is either the `dark' or `light'
symbol.

(fn &optional VARIANT)" t)
(autoload 'ef-themes-theme "ef-themes" "\
Bind NAME's color PALETTE around face specs and variables.
Face specifications are passed to `custom-theme-set-faces'.
While variables are handled by `custom-theme-set-variables'.
Those are stored in `ef-themes-faces' and
`ef-themes-custom-variables' respectively.

Optional OVERRIDES are appended to PALETTE, overriding
corresponding entries.

(fn NAME PALETTE &optional OVERRIDES)" nil t)
(function-put 'ef-themes-theme 'lisp-indent-function 0)
(when load-file-name (let ((dir (file-name-directory load-file-name))) (unless (file-equal-p dir (expand-file-name "themes/" data-directory)) (add-to-list 'custom-theme-load-path dir))))
(register-definition-prefixes "ef-themes" '("ef-themes-"))


;;; Generated autoloads from ef-trio-dark-theme.el

(put 'ef-trio-dark 'theme-properties '(:background-mode dark :kind color-scheme :family ef))


;;; Generated autoloads from ef-trio-light-theme.el

(put 'ef-trio-light 'theme-properties '(:background-mode light :kind color-scheme :family ef))


;;; Generated autoloads from ef-tritanopia-dark-theme.el

(put 'ef-tritanopia-dark 'theme-properties '(:background-mode dark :kind color-scheme :family ef))


;;; Generated autoloads from ef-tritanopia-light-theme.el

(put 'ef-tritanopia-light 'theme-properties '(:background-mode light :kind color-scheme :family ef))


;;; Generated autoloads from ef-winter-theme.el

(put 'ef-winter 'theme-properties '(:background-mode dark :kind color-scheme :family ef))

;;; End of scraped data

(provide 'ef-themes-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; ef-themes-autoloads.el ends here
