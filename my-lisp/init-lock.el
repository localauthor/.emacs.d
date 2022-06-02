;;; init-lock.el --- Lock init.el behind simple arithmetic  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Grant Rosson

;; Author: Grant Rosson <grantrosson@gmail.com>
;; Keywords: 

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

;; Inhibit the opening of your init.el file to prevent tinkering when you
;; should be working.

;; TODO Lock all files in a given directory with list-dirs-recursively

;;; Code:

(defgroup init-lock nil
  "Lock files behind simple arithmetic."
  :group 'convenience
  :prefix "init-lock")

(defcustom init-lock-files nil
  "Files to lock behind arithmetic."
  :type '(repeat file))

(defun init-lock-loop ()
  (let ((answer 1)
        (input)
        (a)
        (b))
    (while (not (eq answer input))
      (setq a (random 1000))
      (setq b (random 100))
      (setq answer (- a b))
      (setq input (read-number (format "%s - %s = " a b))))
    nil))

;;;###autoload
(defun init-lock (orig-fun &rest args)
  (when (member (car args) init-lock-files)
    (init-lock-loop))
  (apply orig-fun args))

;;;###autoload
(defun init-lock-enable ()
  (interactive)
  (advice-add 'find-file :around 'init-lock))

(defun init-lock-disable ()
  (interactive)
  (init-lock-loop)
  (advice-remove 'find-file 'file-lock))

(provide 'init-lock)
;;; init-lock.el ends here
