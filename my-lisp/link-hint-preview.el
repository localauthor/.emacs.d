;;; link-hint-preview.el --- Preview link contents in a pop-up frame with link-hint -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: May 31, 2022
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/localauthor/link-hint-preview
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

;; Preview link contents in a pop-up frame with link-hint.

;; Set frame parameters in the alist 'link-hint-preview-frame-parameters'.

;; Set additional configurations by adding to 'link-hint-preview-mode-hook'.
;; For example, to remove mode-line and tab-bar from the pop-up frame, evaluate:

;; (add-hook 'link-hint-preview-mode-hook 'link-hint-preview-toggle-frame-mode-line)
;; (add-hook 'link-hint-preview-mode-hook 'toggle-frame-tab-bar)

;; Pop-up frame opens in 'view-mode', which see for mode-specific keybindings.

;; Pressing 'q' closes the pop-up frame and returns point to previous window.


;;; Code:

(require 'link-hint)
(require 'zk-link-hint)

;;; Variables

(defgroup link-hint-preview nil
  "Preview link contents in pop-up frame with link-hint."
  :group 'convenience
  :prefix "link-hint-preview-")

(defcustom link-hint-preview-frame-parameters
  '((width . 80)
    (height . 30)
    (undecorated . t)
    (left-fringe . 0)
    (right-fringe . 0)
    (tool-bar-lines . 0)
    (line-spacing . 0)
    (no-special-glyphs . t)
    (inhibit-double-buffering . t)
    (tool-bar-lines . 0)
    (vertical-scroll-bars . nil)
    (menu-bar-lines . 0))
  "Parameters for pop-up frame called by 'link-hint-preview'."
  :type 'list)

(defvar link-hint-preview--kill-last nil)
(defvar-local link-hint-preview--last-frame nil)


;;; Minor Mode

(define-minor-mode link-hint-preview-mode
  "Minor mode to simulate buffer local keybindings."
  :init-value nil
  :keymap '(((kbd "q") . link-hint-preview-close-frame))
  (view-mode))

(defun link-hint-preview-close-frame ()
  "Close frame opened with 'link-hint-preview'."
  (interactive)
  (let ((frame link-hint-preview--last-frame))
    (link-hint-preview-mode -1)
    (view-mode -1)
    (when link-hint-preview--kill-last
      (kill-buffer))
    (delete-frame)
    (select-frame-set-input-focus frame)))


;;; General Command

;;;###autoload
(defun link-hint-preview ()
  "Use avy to view link contents in a pop-up frame.
Set frame parameters in 'link-hint-preview-frame-parameters'."
  (interactive)
  (avy-with link-hint-preview
    (link-hint--one :preview)))


;;; Helper Functions

(defun link-hint-preview-toggle-frame-mode-line ()
  "Remove mode-line from buffers during preview.
Intended to be added to 'link-hint-preview-mode-hook'."
  (if link-hint-preview-mode
      (setq mode-line-format nil)
    (setq mode-line-format (default-value 'mode-line-format))))

(defun link-hint-preview--params (param value)
  "Generate 'pop-up-frame-parameters' dynamically."
  (cons `(,param . ,value) link-hint-preview-frame-parameters))


;;; zk-link support

(link-hint-define-type 'zk-link
:preview #'link-hint-preview-zk-link)

(defun link-hint-preview-zk-link (&optional id)
  "Pop up a frame containing zk-file for ID at point.
Set pop-up frame parameters in 'link-hint-preview-frame-parameters'."
  (interactive)
  (let* ((id (zk--id-at-point))
         (file (zk--parse-id 'file-path id))
         (buffer (get-file-buffer file))
         (frame (selected-frame)))
    (if (get-file-buffer file)
        (setq link-hint-preview--kill-last nil)
      (setq buffer (find-file-noselect file))
      (setq link-hint-preview--kill-last t))
    (display-buffer-pop-up-frame
     buffer
     `((pop-up-frame-parameters . ,(link-hint-preview--params 'delete-before frame))
       (dedicated . t)))
    (with-current-buffer buffer
      (setq-local link-hint-preview--last-frame frame)
      (link-hint-preview-mode))))


;;; file-link support

(link-hint-define-type 'file-link
:preview #'link-hint-preview-file-link)

(defun link-hint-preview-file-link (link)
  "Popup a frame containing file at LINK.
Set popup frame parameters in 'link-hint-preview-frame-parameters'."
  (interactive)
  (let* ((buffer (get-file-buffer link))
         (frame (selected-frame)))
    (setq link-hint-preview--last-frame (selected-frame))
    (if (get-file-buffer link)
        (setq link-hint-preview--kill-last nil)
      (setq buffer (find-file-noselect link))
      (setq link-hint-preview--kill-last t))
    (display-buffer-pop-up-frame
     buffer
     `((pop-up-frame-parameters . ,(link-hint-preview--params 'delete-before frame))
       (dedicated . t)))
    (with-current-buffer buffer
      (setq-local link-hint-preview--last-frame frame)
      (link-hint-preview-mode))))


;;; org-link support

(link-hint-define-type 'org-link
:preview #'link-hint-preview-org-link)

(defun link-hint-preview-org-link ()
  "Popup a frame containing file of org-link.
Set popup frame parameters in 'link-hint-preview-frame-parameters'."
  (interactive)
  (let* ((file (org-element-property :path (org-element-context)))
         (buffer (get-file-buffer file))
         (frame (selected-frame)))
    (setq link-hint-preview--last-frame (selected-frame))
    (if (get-file-buffer file)
        (setq link-hint-preview--kill-last nil)
      (setq buffer (find-file-noselect file))
      (setq link-hint-preview--kill-last t))
    (display-buffer-pop-up-frame
     buffer
     `((pop-up-frame-parameters . ,(link-hint-preview--params 'delete-before frame))
       (dedicated . t)))
    (with-current-buffer buffer
      (setq-local link-hint-preview--last-frame frame)
      (link-hint-preview-mode))))


(provide 'link-hint-preview)

;;; link-hint-preview.el ends here
