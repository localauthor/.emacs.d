;;; gr-functions.el --- Miscellaneous helpful functions     -*- lexical-binding: t; -*-

(defun gr/daily-notes (p)
  "Pop up dailynotes.org."
  (interactive "P")
  (let ((buffer (find-file-noselect
                 (concat org-directory "/dailynotes.org"))))
  (cond ((equal p '(4))
         (select-frame (make-frame-command))
            (find-file (concat org-directory "/dailynotes.org"))
            (set-frame-position (selected-frame) 845 20)
            (delete-other-windows))
        (t (pop-to-buffer buffer
                          '(display-buffer-at-bottom))))))

(defun gr/calfw-open-org-calendar ()
  (interactive)
  (select-frame (make-frame-command))
  (set-frame-position (selected-frame) 150 20)
  (set-frame-size (selected-frame) 160 60)
  (delete-other-windows)
  (calfw-open-org-calendar))

(defun gr/word-count-subtree ()
  "Count words in org subtree at point."
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)
    (let ((wc (org-word-count-aux (point-min) (point-max))))
      (kill-new (format "%d" wc))
      (message (format "%d words in subtree." wc)))))

;; (defun gr/org-journal-new-entry ()
;;   (interactive)
;;   (select-frame (make-frame-command))
;;   (delete-other-windows)
;;   (org-journal-new-entry nil nil))

(defun gr/org-journal-new-entry ()
  (interactive)
  (split-window-below)
  (windmove-down)
  (org-journal-new-entry nil nil)
  (goto-char (point-max)))

(defun gr/lookup-word-at-point ()
  "Lookup word at point in OSX Dictionary."
  (interactive)
  (call-process-shell-command (format "open dict:///%s/" (word-at-point))))

(defun gr/open-mu4e ()
  (interactive)
  (select-frame (make-frame))
  (set-frame-size (selected-frame) 125 45)
  (mu4e)
  ;; (switch-to-buffer " *mu4e-main*")
  (pop-to-buffer-same-window " *mu4e-main*")
  (delete-other-windows))

(defun gr/open-fragments-file ()
  (interactive)
  (find-file "~/Dropbox/org/fragments.org"))

(defun gr/open-fragments-file-other-frame ()
  (interactive)
  (find-file-other-frame "~/Dropbox/org/fragments.org")
  (delete-other-windows))

(defun switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

;; load ~/.emacs.d/init.el
;; (defun refresh-emacs ()
;;   (interactive)
;;   (load "~/.emacs.d/init.el"))

;; (global-set-key (kbd "C-c I") 'refresh-emacs)

(defun gr/open-tasks-file ()
  (interactive)
  (find-file "~/Dropbox/org/tasks.org"))

(defun gr/open-tasks-upcoming-agenda ()
  (interactive)
  (find-file "~/Dropbox/org/tasks.org")
  (delete-other-windows)
  (set-frame-size (selected-frame) 80 43)
  (split-window-below)
  (org-agenda nil "y")
  (other-window 1)
  (enlarge-window 5))

(defun gr/open-tasks-below ()
  (interactive)
  (let ((buffer (find-file-noselect "~/Dropbox/org/tasks.org")))
    (pop-to-buffer buffer
                   '(display-buffer-at-bottom))))

(defun gr/open-tasks-upcoming-agenda-other-frame ()
  (interactive)
  (find-file-other-frame "~/Dropbox/org/tasks.org")
  (set-frame-size (selected-frame) 80 43)
  (delete-other-windows)
  (split-window-below)
  (org-agenda nil "y")
  (other-window 1)
  (enlarge-window 5)
  )

(defun gr/open-init-file (p)
  "Open myinit.org in new frame. With universal argument, open in current window."
  (interactive "P")
  (cond ((equal p '(4)) (find-file (concat user-emacs-directory "init.el")))
        (t  (select-frame (make-frame-command))
            (find-file (concat user-emacs-directory "init.el"))
            (set-frame-position (selected-frame) 845 20)
            (delete-other-windows))))

(global-set-key (kbd "C-c i") 'gr/open-init-file)

;;OPEN init.el
(defun open-user-init-file ()
  "Open ~/.emacs.d/init.el in new frame."
  (interactive)
  (find-file-other-frame user-init-file)
  (set-frame-position (selected-frame) 845 20))

;;OPEN New Window
(defun gr/new-window ()
  (interactive)
  ;;  (when (get-buffer " *company-posframe-buffer*")
  ;;    (kill-buffer " *company-posframe-buffer*"))
  (select-frame (make-frame-command))
  (set-frame-position (selected-frame) 200 100)
  (delete-other-windows))

(defun gr/insert-line (p)
  (interactive "P")
  (cond ((equal p '(4)) (save-excursion
                          (end-of-line 0)
                          (open-line 1)))
        (t (save-excursion
             (end-of-line)
             (open-line 1)))))

(global-set-key (kbd "C-o") 'gr/insert-line)

(defun gr/comment-and-copy ()
  (interactive)
  (let ((beg (region-beginning))
        (end (region-end)))
  (kill-ring-save beg end t)
  (comment-region beg end)
  (goto-char end)
  (forward-line 2)
  (save-excursion
    (yank)
    (newline 2))))

(bind-key* (kbd "C-M-;") 'gr/comment-and-copy)

(provide 'gr-functions)
