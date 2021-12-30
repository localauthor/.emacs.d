;;; link-hint-aw-select-el --- Add aw-select function to link-hint    -*- lexical-binding: t; -*-

;; Incorporates 'aw-select' into 'link-hint', allowing for the selection of a
;; window in whichto open the target link.

(require 'link-hint)

;; NOTE: To for this function to work on org-links, it is necessary to change
;; default behavior for opening files in org-mode by evaluating the
;; following, which changes the value of the key 'file in the alist
;; 'org-link-frame-setup' from the default, 'find-file-other-window', to
;; 'find-file'. I prefer this globally, but it would also be possible to
;; let-bind this variablbe in the function itself.

(setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)


;;; general command

(defun link-hint-aw-select ()
  "Use avy to open a link in a window selected with ace-window."
  (interactive)
  (avy-with link-hint-aw-select
    (link-hint--one :aw-select)))


;;; file-link support

(link-hint-define-type 'file-link
:aw-select #'link-hint--aw-select-file-link)

(defun link-hint--aw-select-file-link (link)
(aw-switch-to-window (aw-select nil))
(find-file link))


;;; macro for similar types

(eval-when-compile
  (defmacro define-link-hint-aw-select (link-type fn)
    (link-hint-define-type ',link-type
      :aw-select #',(intern (concat "link-hint--aw-select-" (symbol-name link-type))))
    `(defun ,(intern (concat "link-hint--aw-select-" (symbol-name link-type))) (link)
       (let ((window (aw-select nil))
             (buffer (current-buffer))
             (new-buffer))
         (,fn)
         (setq new-buffer (current-buffer))
         (switch-to-buffer buffer)
         (aw-switch-to-window window)
         (switch-to-buffer new-buffer)))))

(define-link-hint-aw-select button push-button)
(define-link-hint-aw-select org-link org-open-at-point)
(define-link-hint-aw-select dired-filename dired-find-file)


;;; avy action

(defun gr/avy-action-aw-select (pt)
  (let ((window (aw-select nil))
        (buffer (current-buffer))
        (new-buffer))
    (goto-char pt)
    (link-hint-open-link-at-point)
    (setq new-buffer (current-buffer))
    (switch-to-buffer buffer)
    (aw-switch-to-window window)
    (switch-to-buffer new-buffer)))


(provide 'link-hint-aw-select)
