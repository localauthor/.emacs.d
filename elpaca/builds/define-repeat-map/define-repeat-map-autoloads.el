;;; define-repeat-map-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:



;;; Generated autoloads from define-repeat-map.el

(autoload 'define-repeat-map "define-repeat-map" "\
Define a `repeat-map', NAME -repeat-map, and bind KEYS to it.
Each ARG is a list of lists containing keybind definitions of
the form (KEY DEFINITION) KEY is anything `kbd' can recognize,
and DEFINITION is passed directly to `define-key'.

Optionally, the car of an arglist can contain the following
symbols, which changes the behavior of the key definitions in the
rest of the list:

:enter - Provided commands can enter the `repeat-map', but aren't
bound in the map.  They need to be bound elsewhere, however.

:exit - Keys are bound in the `repeat-map', but can't enter the
map.  Their invocation exits the `repeat-map'.

:continue - Keys are bound in the `repeat-map', but can't enter the
map.  However, their invocations keep the `repeat-map' active.

(fn NAME &rest KEYS)" nil t)
(function-put 'define-repeat-map 'lisp-indent-function 1)
(register-definition-prefixes "define-repeat-map" '("define-repeat-map--"))

;;; End of scraped data

(provide 'define-repeat-map-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; define-repeat-map-autoloads.el ends here
