;;; link-hint-preview.el --- Add popup preview function to link-hint -*- lexical-binding: t; -*-

;; or link-hint popup?

;;; Commentary:

;; Incorporates pop-up preview function into 'link-hint'.

;; how to get buffer-local keybinding, for "q" to quit?
;; (doing local-set-key applies to all buffers in the major-mode)
;; see:
;; https://emacs.stackexchange.com/questions/519/key-bindings-specific-to-a-buffer

;; or use "special-mode" instead of read-only-mode?
;; no that doesn't kill and quit buffer...

;; define preview minor-mode?

;;; Code:

(require 'link-hint)
(require 'org)
(require 'ace-window)
(require 'dired)

;;; Minor Mode

(define-minor-mode preview-mode
    "Minor mode to simulate buffer local keybindings."
    :init-value nil
    :keymap '(((kbd "q") . link-hint-preview-close-frame))
    (read-only-mode)
    (tab-bar-mode -1))

(defun link-hint-preview-close-frame ()
  (interactive)
  (preview-mode -1)
  (read-only-mode -1)
  (when link-hint-preview-kill-last
    (kill-buffer))
  (delete-frame)
  (select-frame-set-input-focus link-hint-preview-frame)
  (select-window link-hint-preview-window))

;; doesn't move point to parent frame after closing...?
;; problem with macos emacs build?
;; https://xenodium.com/no-emacs-frame-refocus-on-macos/


;; close buffer, if it wasn't open??

;;; General command

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
    (minibuffer . nil)
    )
  "Parameters for pop-up frame called by 'link-hint-preview'")

;;; zk

(link-hint-define-type 'zk-link
:preview #'link-hint--preview-zk-link)

(defvar link-hint-preview-kill-last nil)
(defvar link-hint-preview-window nil)
(defvar link-hint-preview-frame nil)

(defun link-hint--preview-zk-link (id)
  "Popup a frame containing zk-file for ID at point.
Set popup frame parameters in 'zk-preview-frame-parameters'."
  (interactive)
  (let* ((id (zk--id-at-point))
         (file (zk--parse-id 'file-path id))
         (buffer (get-file-buffer file)))
    (setq link-hint-preview-window (selected-window))
    (setq link-hint-preview-frame (selected-frame))
    (if (get-file-buffer file)
        (setq link-hint-preview-kill-last nil)
      (setq buffer (find-file-noselect file))
      (setq link-hint-preview-kill-last t))
    (display-buffer-pop-up-frame
     buffer
     `((pop-up-frame-parameters . ,link-hint-preview-frame-parameters)))
    (switch-to-buffer buffer)
    (preview-mode)))

;;; File-link support

(link-hint-define-type 'file-link
:preview #'link-hint--preview-file-link)


(defun link-hint--preview-file-link (link)
  "Popup a frame containing file at LINK.
Set popup frame parameters in 'link-hint-preview-frame-parameters'."
  (let* ((buffer (find-file-noselect link))
         (parent (selected-frame)))
    (special-display-popup-frame buffer link-hint-preview-frame-parameters)
    (switch-to-buffer buffer)
    ;;(local-set-key (kbd "q") 'kill-buffer-and-window)
    (tab-bar-mode -1)
    (read-only-mode)))


;; ;;; Shr-url

;; ;; (define-link-hint-aw-select shr-url (browse-url _link))

;; (defun link-hint--aw-select-shr-url (link)
;;   (with-demoted-errors "%s"
;;       (if (> (length (aw-window-list)) 1)
;;           (let ((window (aw-select nil))
;;                (buffer (current-buffer))
;;                (new-buffer))
;;             (browse-url link)
;;             (setq new-buffer
;;                   (current-buffer))
;;             (switch-to-buffer buffer)
;;             (aw-switch-to-window window)
;;             (switch-to-buffer new-buffer))
;;         (link-hint-open-link-at-point))))

;; ;;; Macro for similar link types

;; ;; This macro can be used for link types that open the target in the current
;; ;; window.

;; ;; NOTE: For this macro to work on org-links, it is necessary to change
;; ;; default behavior from 'find-file-other-window' to 'find-file'. To do this,
;; ;; evaluate the 'setf' function below.

;; ;; This function changes the value of the key 'file in the alist
;; ;; 'org-link-frame-setup' from the default, 'find-file-other-window', to
;; ;; 'find-file'. I prefer this globally, but it would also be possible to
;; ;; let-bind this variable in the function itself.

;; (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

;; (defmacro define-link-hint-aw-select (link-type fn)
;;   `(progn
;;      (link-hint-define-type ',link-type
;;        :aw-select #',(intern (concat "link-hint--aw-select-" (symbol-name link-type))))
;;      (defun ,(intern (concat "link-hint--aw-select-" (symbol-name link-type))) (_link)
;;        (with-demoted-errors "%s"
;;          (if (> (length (aw-window-list)) 1)
;;              (let ((window (aw-select nil))
;;                    (buffer (current-buffer))
;;                    (new-buffer))
;;                (,fn)
;;                (setq new-buffer (current-buffer))
;;                (switch-to-buffer buffer)
;;                (aw-switch-to-window window)
;;                (switch-to-buffer new-buffer))
;;            (link-hint-open-link-at-point))))))

;; (define-link-hint-aw-select button push-button)
;; (define-link-hint-aw-select org-link org-open-at-point)
;; (define-link-hint-aw-select dired-filename dired-find-file)

;; ;; add exception for http/s
;; (defun link-hint--aw-select-org-link (_link)
;;     (with-demoted-errors "%s"
;;       (if (and (> (length (aw-window-list)) 1)
;;                (not (member (org-element-property
;;                              :type (org-element-context))
;;                        '("http" "https"))))
;;           (let ((window (aw-select nil))
;;                 (buffer (current-buffer))
;;                 (new-buffer))
;;             (org-open-at-point)
;;             (setq new-buffer
;;                   (current-buffer))
;;             (switch-to-buffer buffer)
;;             (aw-switch-to-window window)
;;             (switch-to-buffer new-buffer))
;;         (link-hint-open-link-at-point))))


(provide 'link-hint-preview)

;;; link-hint-preview.el ends here
