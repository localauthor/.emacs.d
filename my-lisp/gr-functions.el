;;; gr-functions.el --- Miscellaneous helpful functions     -*- lexical-binding: t; -*-


;; (defhydra gr/symbol-menu (:hint nil :color blue)
;;   "
;; Symbols and Diacritics
;;     _a_: ą   _e_: ė   _u_: ū   _U_: ų  _E_: €
;;     _s_: š   _c_: č   _z_: ž   _i_: į
;;     "
;;   ("q" nil)
;;   ("a" (insert "ą"))
;;   ("e" (insert "ė"))
;;   ("u" (insert "ū"))
;;   ("U" (insert "ų"))
;;   ("s" (insert "š"))
;;   ("c" (insert "č"))
;;   ("z" (insert "ž"))
;;   ("i" (insert "į"))
;;   ("E" (insert "€"))
;;   )

(defun gr/select-theme ()
  (interactive)
  (let ((theme (intern (completing-read "Select: " '(gr-light ef-bio)))))
    (mapc #'disable-theme custom-known-themes)
    (load-theme theme :no-confirm)))

(defun gr/toggle-theme ()
  (interactive)
  (if (equal custom-enabled-themes '(gr-light))
      (progn
        (mapc #'disable-theme '(gr-light))
        (load-theme 'ef-bio :no-confirm))
    (progn
      (mapc #'disable-theme '(ef-bio))
      (load-theme 'gr-light :no-confirm))))

;;;###autoload
(defun gr/daily-notes ()
  "Pop up dailynotes.org."
  (interactive)
  (let ((buffer (find-file-noselect
                 (concat org-directory "/dailynotes.org"))))
    (cond ((equal current-prefix-arg '(4))
           (select-frame (make-frame-command))
           (find-file (concat org-directory "/dailynotes.org"))
           (set-frame-position (selected-frame) 845 20)
           (delete-other-windows))
          (t (pop-to-buffer buffer
                            '((display-buffer-at-bottom)
                              ;;(window-height . 0.5)
                              ))))
    (gr/daily-notes-new-headline)))

(defun gr/daily-notes-new-headline ()
  (interactive)
  (org-set-startup-visibility)
  (let ((date (concat "** " (format-time-string "%Y-%m-%d %A")))
        (month (concat "* " (format-time-string "%B %Y"))))
    (goto-char (point-min))
    (unless (re-search-forward month nil t)
      (forward-line 2)
      (insert month "\n\n")
      (forward-line -2)
      (org-set-property "VISIBILITY" "all"))
    (unless (re-search-forward date nil t)
      (forward-line 4)
      (insert "\n" date "\n- |\n")
      (search-backward "|")
      (delete-char 1))))

(defun gr/calfw-open-org-calendar ()
  (interactive)
  (select-frame (make-frame-command))
  (set-frame-position (selected-frame) 150 20)
  (set-frame-size (selected-frame) 160 60)
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
  (delete-other-windows))
;;   (display-buffer-full-frame " *mu4e-main*" '(nil)))
;;   ;; (switch-to-buffer " *mu4e-main*")
;;   (pop-to-buffer-same-window " *mu4e-main*")
;;   (delete-other-windows))
;; ;; use display-buffer-full-frame ?

(defun gr/open-fragments-file ()
  (interactive)
  (find-file "~/Dropbox/org/fragments.org"))

(defun gr/open-fragments-file-other-frame ()
  (interactive)
  (find-file-other-frame "~/Dropbox/org/fragments.org"))
  ;;(delete-other-windows))

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
  (cond ((equal p '(4))
         (gr/make-frame)
         (find-file (concat user-emacs-directory "init.el")))
        (t (find-file (concat user-emacs-directory "init.el")))))

;;(set-frame-position (selected-frame) 845 20)
;;(delete-other-windows))))

;;OPEN init.el
(defun open-user-init-file ()
  "Open ~/.emacs.d/init.el in new frame."
  (interactive)
  (find-file-other-frame user-init-file)
  (set-frame-position (selected-frame) 845 20))

;;OPEN New Frame
;; (defun gr/new-window ()
;;   (interactive)
;;   (select-frame (make-frame-command))
;;   (set-frame-position (selected-frame) 200 100)
;;   (delete-other-windows))


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

(defun gr/copy-file-path ()
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (let ((filepath (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filepath
      (kill-new filepath)
      (message "Copied buffer file path '%s' to the clipboard." filepath))))


(defun gr/copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))


;;;; capitalize, upcase, downcase dwim

(defun title-case-region ()
  "Render string in region in title case."
  (interactive)
  (let* ((input (buffer-substring (region-beginning)
                                  (region-end)))
         (words (split-string input))
         (first (pop words))
         (last (car(last words)))
         (do-not-capitalize '("a" "an" "and" "as" "at" "but" "by" "en" "for" "if" "in" "of" "on" "or" "the" "to" "via"))
         (output (concat (capitalize first)
                         " "
                         (mapconcat (lambda (w)
                                      (if (not(member (downcase w) do-not-capitalize))
                                          (capitalize w)(downcase w)))
                                    (butlast words) " ")
                         " " (capitalize last))))
    (replace-string input output nil (region-beginning)(region-end))))


(defun ct/word-boundary-at-point-or-region (&optional callback)
  "Return the boundary (beginning and end) of the word at point, or region, if any.
Forwards the points to CALLBACK as (CALLBACK p1 p2), if present.

URL: https://christiantietze.de/posts/2021/03/change-case-of-word-at-point/"
  (let ((deactivate-mark nil)
        $p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning)
              $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alpha:]")
        (setq $p1 (point))
        (skip-chars-forward "[:alpha:]")
        (setq $p2 (point))))
    (when callback
      (funcall callback $p1 $p2))
    (list $p1 $p2)))

;; (defun ct/capitalize-word-at-point ()
;;   (interactive)
;;   (ct/word-boundary-at-point-or-region #'upcase-initials-region))

(defun ct/downcase-word-at-point ()
  (interactive)
  (ct/word-boundary-at-point-or-region #'downcase-region))

(defun ct/upcase-word-at-point ()
  (interactive)
  (ct/word-boundary-at-point-or-region #'upcase-region))

(defun ct/capitalize-region (p1 p2)
  (downcase-region p1 p2)
  (upcase-initials-region p1 p2))

(defun ct/capitalize-word-at-point ()
  (interactive)
  (ct/word-boundary-at-point-or-region #'ct/capitalize-region))

;; Set global shortcuts
(global-set-key (kbd "M-c") #'ct/capitalize-word-at-point)
(global-set-key (kbd "M-u") #'ct/upcase-word-at-point)
(global-set-key (kbd "M-l") #'ct/downcase-word-at-point)
(global-set-key (kbd "M-U") #'title-case-region)



;;;; convert docx to org

(defun gr/flush-properties-drawers ()
  (interactive)
  (goto-line 2)
  (flush-lines ":PROPERTIES:")
  (flush-lines ":CUSTOM_ID:")
  (flush-lines ":END:")
  )

(defun gr/convert-pandoc-docx-org ()
  "Use pandoc via shell command to convert a docx file to an org file.
Navigate to files in dired, mark files, and execute command."
  (interactive)
  (dired-do-async-shell-command
   "pandoc -f docx -t org --wrap=none" current-prefix-arg
   (dired-get-marked-files t current-prefix-arg))
  (switch-to-buffer-other-window "*Async Shell Command*")
  (run-with-idle-timer 1 nil
                       'gr/flush-properties-drawers)
  (goto-line 2)
  (run-with-idle-timer 1 nil
                       'gr/flush-properties-drawers)
  )

(defun gr/clear-empty-org-headers ()
  (interactive)
  (goto-line 2)
  (replace-string "
  ,* " " ")
  (goto-line 2)
  (replace-string "
  ,** " " ")
  (goto-line 2)
  (replace-string "
  ,*** " " ")
  )


;;;; "Better Return" edited

;; a better return; inserts list item with RET instead of M-RET

(defun scimax/org-return (&optional ignore)
  "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
  (interactive "P")
  (if ignore
      (org-return)
    (cond

     ((eq 'line-break (car (org-element-context)))
      (org-return t))

     ;; Open links like usual, unless point is at the end of a line.
     ;; and if at beginning of line, just press enter.
     ((or (and (eq 'link (car (org-element-context))) (not (eolp)))
          (bolp))
      (org-return))

     ;; checkboxes - add new or delete empty
     ((org-at-item-checkbox-p)
      (cond
       ;; at the end of a line.
       ((and (eolp)
             (not (eq 'item (car (org-element-context)))))
        (org-insert-todo-heading nil))
       ;; no content, delete
       ((and (eolp) (eq 'item (car (org-element-context))))
        (delete-region (line-beginning-position) (line-end-position)))
       ((eq 'paragraph (car (org-element-context)))
        (goto-char (org-element-property :end (org-element-context)))
        (org-insert-todo-heading nil))
       (t
        (org-return))))

     ;; lists end with two blank lines, so we need to make sure we are also not
     ;; at the beginning of a line to avoid a loop where a new entry gets
     ;; created with only one blank line.
     ((org-in-item-p)
      (cond
       ;; empty definition list
       ((and (looking-at " ::")
             (looking-back "- " 3))
        (beginning-of-line)
        (delete-region (line-beginning-position) (line-end-position)))
       ;; empty item
       ((and (looking-at "$")
             (or
              (looking-back "- " 3)
              (looking-back "+ " 3)
              (looking-back " \\* " 3)))
        (beginning-of-line)
        (delete-region (line-beginning-position) (line-end-position)))
       ;; numbered list
       ((and (looking-at "$")
             (looking-back "^[0-9]+. " (line-beginning-position)))
        (beginning-of-line)
        (delete-region (line-beginning-position) (line-end-position)))
       ;; insert new item
       (t
        (if (not (looking-at "$"))
            (org-return)
          (end-of-line)
          (org-insert-item)))))

     ;; org-heading
     ((org-at-heading-p)
      (if (not (string= "" (org-element-property :title (org-element-context))))
          (if (not (looking-at "$"))
              (org-return)
            (progn
              ;; Go to end of subtree suggested by Pablo GG on Disqus post.
              ;;(org-end-of-subtree)
              (org-meta-return)
              ;;(org-metaright)
              ;;(org-insert-heading-respect-content)
              (outline-show-entry)
              ))
        ;; The heading was empty, so we delete it
        (beginning-of-line)
        (delete-region (line-beginning-position) (line-end-position))))

     ;; tables
     ((org-at-table-p)
      (if (-any?
           (lambda (x) (not (string= "" x)))
           (nth
            (- (org-table-current-dline) 1)
            (remove 'hline (org-table-to-lisp))))
          (org-return)
        ;; empty row
        (beginning-of-line)
        (delete-region (line-beginning-position) (line-end-position))
        (org-return)))

     ;; footnotes

     ;; fall-through case
     (t
      (org-return)))))


(provide 'gr-functions)
