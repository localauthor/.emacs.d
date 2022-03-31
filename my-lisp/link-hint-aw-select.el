;;; link-hint-aw-select-el --- Add aw-select function to link-hint -*- lexical-binding: t; -*-

;;; Commentary:

;; Incorporates 'aw-select' into 'link-hint', allowing for the selection of a
;; window in which to open the target link.

;;; Code:

(require 'link-hint)
(require 'org)
(require 'ace-window)
(require 'dired)

;; NOTE: To for this function to work on org-links, it is necessary to change
;; default behavior for opening files in org-mode by evaluating the
;; following, which changes the value of the key 'file in the alist
;; 'org-link-frame-setup' from the default, 'find-file-other-window', to
;; 'find-file'. I prefer this globally, but it would also be possible to
;; let-bind this variable in the function itself.

(setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

;;; General command

;;;###autoload
(defun link-hint-aw-select ()
  "Use avy to open a link in a window selected with ace-window."
  (interactive)
  (unless
      (avy-with link-hint-aw-select
        (link-hint--one :aw-select))
    (message "No visible links")))

;;; File-link support

(link-hint-define-type 'file-link
:aw-select #'link-hint--aw-select-file-link)

;; (defun link-hint--aw-select-file-link (link)
;;   (let ((aw-dispatch-function #'ignore))
;;     (aw-switch-to-window (aw-select nil))
;;     (find-file link)))

(defun link-hint--aw-select-file-link (link)
  (with-demoted-errors "%s"
    (aw-switch-to-window (aw-select nil))
    (find-file link)))

;;; Macro for similar types

(defmacro define-link-hint-aw-select (link-type fn)
  `(progn
     (link-hint-define-type ',link-type
       :aw-select #',(intern (concat "link-hint--aw-select-" (symbol-name link-type))))
     (defun ,(intern (concat "link-hint--aw-select-" (symbol-name link-type))) (_link)
       (with-demoted-errors "%s"
         (if (> (length (aw-window-list)) 1)
             (let ((window (aw-select nil))
                   (buffer (current-buffer))
                   (new-buffer))
               (,fn)
               (setq new-buffer (current-buffer))
               (switch-to-buffer buffer)
               (aw-switch-to-window window)
               (switch-to-buffer new-buffer))
           (link-hint-open-link-at-point))))))

(define-link-hint-aw-select button push-button)
(define-link-hint-aw-select org-link org-open-at-point)
(define-link-hint-aw-select dired-filename dired-find-file)

(provide 'link-hint-aw-select)
