;;;; gr-functions.el --- Miscellaneous helpful functions     -*- lexical-binding: t; -*-

;;; daily-notes

;;;###autoload
(defun gr/daily-notes (p)
  "Pop up dailynotes.org."
  (interactive "P")
  (let ((buffer (find-file-noselect
                 (concat org-directory "/dailynotes.org"))))
    (cond ((equal p '(4))
           (select-frame (make-frame-command))
           (find-file (concat org-directory "/dailynotes.org"))
           (delete-other-windows))
          ((equal p '(16))
           (pop-to-buffer-same-window buffer)
           (goto-char (point-min))
           (org-next-visible-heading 1))
          ((or (eq (current-buffer) buffer)
               t)
           (pop-to-buffer-same-window buffer)
           (gr/daily-notes-new-dateline)))))

(defun gr/daily-notes-new-dateline ()
  "Create new date headline for daily note.
When called interactively, select date."
  (interactive "")
  (if (interactive-p)
      (let* ((org-display-custom-times t)
             (org-time-stamp-formats
              '("%Y-%m-%d %A" . "%Y-%m-%d %A %H:%M"))
             (date (with-temp-buffer
                     (insert "** ")
                     (org-time-stamp nil
                                     'no-brackets)
                     (buffer-string))))
        (insert date))
    (org-cycle-set-startup-visibility)
    (let ((date (concat "** "
                        (format-time-string "%Y-%m-%d %A")))
          (month (concat "* " (format-time-string "%B %Y")))
          (last-month (format-time-string "%B"
                                          (time-subtract
                                           (current-time)
                                           (days-to-time 30)))))
      (goto-char (point-min))
      (unless (re-search-forward month nil t)
        (re-search-forward (concat "* " last-month))
        (forward-line 1)
        (kill-whole-line)
        (forward-line -1)
        (org-cycle)
        (insert month "\n\n")
        (forward-line -2)
        (org-set-property "VISIBILITY" "all"))
      (unless (re-search-forward date nil t)
        (forward-line 4)
        (insert "\n" date "\n- |\n")
        (search-backward "|")
        (delete-char 1)))))


;;; frame functions

(defun gr/delete-frame-or-tab ()
  "Delete frame or tab."
  (interactive)
  (if  (< 1 (length (tab-bar-tabs (window-frame))))
      (tab-close)
    (delete-frame)))

(defun gr/make-frame ()
  "Make frame, centered, on current monitor."
  (interactive)
  (make-frame-on-current-monitor))
;; (unless (eq 'maximised (frame-parameter nil 'fullscreen))
;;   (modify-frame-parameters
;;    (selected-frame) '((user-position . t) (top . 0.5) (left . 0.5)))))

;;; open functions

(defun switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun gr/open-init-file (p)
  "Open myinit.org in new frame. With universal argument, open in current window."
  (interactive "P")
  (if (equal p '(4))
      (find-file (concat user-emacs-directory "init.el"))
    (find-file-other-tab (concat user-emacs-directory "init.el"))))

;;; bluetooth

(defun gr/process-output (program &rest args)
  "Run PROGRAM with ARGS and return output."
  (with-temp-buffer
    (apply 'call-process program nil (current-buffer) nil args)
    (buffer-string)))

(defun gr/toggle-bluetooth (&optional arg)
  (interactive)
  (if (equal "0\n" ;; bluetooth off
             (gr/process-output "blueutil"
                                "-p"))
      (shell-command "blueutil -p 1")
    (unless arg
      (shell-command "blueutil -p 0"))))

(defun gr/toggle-headphones ()
  "Toggle bluetooth headphones connection
Uses command-line program blueutil."
  (interactive)
  (let ((inhibit-message t))
    (gr/toggle-bluetooth 1)
    (if (equal "0\n"
               (gr/process-output "blueutil"
                                  "--is-connected"
                                  "ac-12-2f-5c-30-35"))
        (shell-command "blueutil --connect  ac-12-2f-5c-30-35")
      (shell-command "blueutil --disconnect  ac-12-2f-5c-30-35"))))

;;; insert dummy header line

(defun gr/dummy-header-line ()
  (interactive)
  (insert "* ")
  (insert-char ?* 50)
  (insert " :noheadline:"))

;;; insert line

(defun gr/insert-line (p)
  (interactive "P")
  (let ((fill-prefix nil))
    (cond ((equal p '(4)) (save-excursion
                            (end-of-line 0)
                            (open-line 1)))
          (t (save-excursion
               (end-of-line)
               (open-line 1))))))

;;; capslock

(defun gr/toggle-capslock ()
  "Toggle capslock.
See bin in ~/Repos/capslock and source
https://discussions.apple.com/thread/7094207"
  (interactive)
  (shell-command "capslock -1")
  (message "Capslock toggled"))


;;; select from alist

(defun gr/select-from-alist (alist &optional input)
  "Use 'completing-read' to return a value from a list of keys ALIST.
Optional initial INPUT."
  (alist-get
   (completing-read "Choose: " alist nil t input)
   alist nil nil 'equal))

(defun gr/open-file-externally (alist &optional input)
  (embark-open-externally (gr/select-from-alist alist input)))

;;; comment and copy

(defun gr/comment-and-copy ()
  (interactive)
  (unless (region-active-p)
    (mark-defun))
  (let ((beg (region-beginning))
        (end (region-end)))
    (kill-ring-save beg end t)
    (comment-region beg end)
    (goto-char beg)
    (newline 2)
    (forward-line -2)
    (yank)))

;;; backward-delete-word

(defun gr/backward-delete-word ()
  "Like `backward-kill-word', but doesn't add to kill ring."
  (interactive "*")
  (push-mark)
  (backward-word)
  (delete-region (point) (mark)))

(keymap-global-set "C-<backspace>" 'gr/backward-delete-word)


;;; word-count functions

(defun gr/word-count-subtree ()
  "Count words in org subtree at point."
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)
    (let ((wc (org-word-count-aux (point-min) (point-max))))
      (kill-new (format "%d" wc))
      (message (format "%d words in subtree." wc)))))

(defun gr/lookup-word-at-point ()
  "Lookup word at point in OSX Dictionary."
  (interactive)
  (call-process-shell-command (format "open dict:///%s/" (word-at-point))))

;;; capitalize, upcase, downcase dwim

(defun title-case-region ()
  "Render string in region in title case."
  (interactive)
  (let* ((input (buffer-substring (region-beginning)
                                  (region-end)))
         (words (split-string input))
         (first (capitalize (pop words)))
         (last (car (last words)))
         (do-not-capitalize '("a" "an" "and" "as" "at" "but" "by" "en" "for" "if" "in" "of" "on" "or" "the" "to" "via"))
         (mid (mapconcat (lambda (w)
                           (if (not (member (downcase w) do-not-capitalize))
                               (capitalize w)(downcase w)))
                         (butlast words) " "))
         (output (concat first
                         (unless (string-empty-p mid)
                           (concat " " mid))
                         (when last
                           (concat " " (capitalize last))))))
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


;;; convert docx to org

(defun gr/flush-properties-drawers ()
  (interactive)
  (goto-line 2)
  (flush-lines ":PROPERTIES:")
  (flush-lines ":CUSTOM_ID:")
  (flush-lines ":END:"))

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
                       'gr/flush-properties-drawers))

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
  ,*** " " "))

(provide 'gr-functions)
