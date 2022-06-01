;;; link-hint-preview.el --- Add popup preview function to link-hint -*- lexical-binding: t; -*-

;;; Commentary:

;; Incorporates pop-up preview function into 'link-hint'.

;;; Code:

(require 'link-hint)
(require 'zk-link-hint)

;;; Minor Mode

(define-minor-mode link-hint-preview-mode
  "Minor mode to simulate buffer local keybindings."
  :init-value nil
  :keymap '(((kbd "q") . link-hint-preview-close-frame))
  (read-only-mode)
  (tab-bar-mode -1))

(defun link-hint-preview-close-frame ()
  (interactive)
  (let ((frame link-hint-preview--last-frame))
    (link-hint-preview-mode -1)
    (read-only-mode -1)
    (when link-hint-preview--kill-last
      (kill-buffer))
    (delete-frame)
    (select-frame-set-input-focus frame)))

;;; General Command

;;;###autoload
(defun link-hint-preview ()
  "Use avy to preview link contents in a pop-up frame."
  (interactive)
  (avy-with link-hint-preview
    (link-hint--one :preview)))

(defvar link-hint-preview-frame-parameters
  '((width . 80)
    (height . 35)
    (undecorated . t)
    (dedicated . nil)
    (left-fringe . 0)
    (right-fringe . 0)
    (tool-bar-lines . 0)
    (line-spacing . 0)
    (no-special-glyphs . t)
    (inhibit-double-buffering . t)
    (tool-bar-lines . 0)
    (vertical-scroll-bars . nil)
    (menu-bar-lines . 0)
    (fullscreen . nil)
    (minibuffer . nil))
  "Parameters for pop-up frame called by 'link-hint-preview'")

(defvar link-hint-preview--kill-last nil)
(defvar-local link-hint-preview--last-frame nil)


;;; zk-link support

(link-hint-define-type 'zk-link
:preview #'link-hint--preview-zk-link)

(defun link-hint--preview-zk-link (id)
  "Pop up a frame containing zk-file for ID at point.
Set pop-up frame parameters in 'zk-preview-frame-parameters'."
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
     `((pop-up-frame-parameters . ,link-hint-preview-frame-parameters)))
    (with-current-buffer buffer
      (setq-local link-hint-preview--last-frame frame)
      (link-hint-preview-mode))))


;;; file-link support

(link-hint-define-type 'file-link
:preview #'link-hint--preview-file-link)

(defun link-hint--preview-file-link (link)
  "Popup a frame containing file at LINK.
Set popup frame parameters in 'link-hint-preview-frame-parameters'."
  (interactive)
  (let* ((buffer (get-file-buffer link)))
    (setq link-hint-preview--last-frame (selected-frame))
    (if (get-file-buffer link)
        (setq link-hint-preview--kill-last nil)
      (setq buffer (find-file-noselect link))
      (setq link-hint-preview--kill-last t))
    (display-buffer-pop-up-frame
     buffer
     `((pop-up-frame-parameters . ,link-hint-preview-frame-parameters)))
    (switch-to-buffer buffer)
    (link-hint-preview-mode)))


(provide 'link-hint-preview)

;;; link-hint-preview.el ends here
