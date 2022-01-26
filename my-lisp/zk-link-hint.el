;;; zk-link-hint.el --- Link-Hint integration for zk  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: January 4, 2022
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/localauthor/zk
;; Package-Requires: ((emacs "24.3") (link-hint "0.1"))


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides integration between link-hint.el and zk. To use,
;; arrange for it to be loaded once both of those are loaded:

;; (with-eval-after-load 'link-hint
;;   (with-eval-after-load 'zk
;;     (require 'zk-link-hint)))

;;; Code:

;; (require 'zk)
(require 'link-hint)

(defun zk-link-hint--zk-link-at-point-p ()
  "Return the ID for the zk-link at the point or nil."
  (thing-at-point-looking-at zk-link-regexp))

(defun zk-link-hint--next-zk-link (&optional bound)
  "Find the unext zk-link.
Only search the range between just after the point and BOUND."
  (link-hint--next-regexp zk-id-regexp bound))

(link-hint-define-type 'zk-link
  :next #'zk-link-hint--next-zk-link
  :at-point-p #'zk-link-hint--zk-link-at-point-p
  :open #'zk-follow-link-at-point
  :copy #'kill-new)

(push 'link-hint-zk-link link-hint-types)

(provide 'zk-link-hint)

;;; zk-link-hint.el ends here
