;;;; gr-functions.el --- Miscellaneous helpful functions     -*- lexical-binding: t; -*-

;;; train tickets

(defun gr/train-tickets (dest)
  "Open train schedule for chosen DESTination."
  (interactive (list (gr/select-from-alist
                      '(("Vilnius" . 17)
                        ("Vievis" . 8))
                      nil "To: ")))
  (browse-url
   (format
    "https://bilietas.ltglink.lt/journeys?oStop=%s&dStop=%s&fareClasses=BONUS_SCHEME_GROUP.ADULT,1"
    (if (eq dest 17) 8 17) dest)))

(defun gr/train-to-vievis ()
  (interactive)
  (gr/train-tickets 8))

(defun gr/train-to-vilnius ()
  (interactive)
  (gr/train-tickets 17))

;;; daily-notes

;;;###autoload
(defun gr/daily-notes (p)
  "Pop up dailynotes.org."
  (interactive "P")
  (let ((buffer (find-file-noselect
                 (concat org-directory "/dailynotes.org"))))
    (if (equal p '(4))
        (progn
          (select-frame (make-frame-command))
          (find-file (concat org-directory "/dailynotes.org"))
          (delete-other-windows))
      (pop-to-buffer-same-window buffer)
      (goto-char (point-min))
      (org-next-visible-heading 1))))

(defun gr/insert-date ()
  (interactive)
  (let* ((org-display-custom-times t)
         (org-time-stamp-formats
          '("%F %A" . "%F %A %R")))
    (org-timestamp nil 'no-brackets)))

(defun gr/daily-notes-new-dateline ()
  "Create new date headline for daily note.
When called interactively, select date."
  (interactive)
  (org-cycle-set-startup-visibility)
  (let ((date (format-time-string "** %F %A"))
        (month (format-time-string "* %B %Y"))
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
    (if (re-search-forward date nil t)
        (gr/daily-notes-new-item-maybe)
      (forward-line 4)
      (insert "\n" date "\n- |\n")
      (search-backward "|")
      (delete-char 1))))

(defun gr/org-empty-item-p ()
  (end-of-line)
  (and (looking-at "$")
       (or
        (looking-back "- " 3)
        (looking-back "+ " 3)
        (looking-back " \\* " 3))))

(defun gr/daily-notes-new-item-maybe ()
  (forward-line 1)
  (if (org-at-item-p)
      (while
          (not (gr/org-empty-item-p))
        (or (ignore-errors
              (org-next-item))
            (end-of-line)
            (insert "\n- ")))
    (insert "- ")))


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
  (let* ((init-file (concat user-emacs-directory "init.el"))
         (init-buf (get-file-buffer init-file))
         (init-tab (when init-buf (tab-bar-get-buffer-tab init-buf))))
    (cond (p
           (find-file init-file))
          (init-tab
           (tab-switch (alist-get 'name init-tab)))
          (t
           (find-file-other-tab init-file)))))

(defun gr/find-file-open-externally (file)
  "Find FILENAME and open with external application."
  (interactive "fFind file:")
  (call-process "open" nil 0 nil (expand-file-name file)))

;;; bluetooth

(defun gr/process-output (program &rest args)
  "Run PROGRAM with ARGS and return output."
  (with-temp-buffer
    (when (zerop (apply #'call-process program nil t nil args))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun gr/toggle-bluetooth (&optional arg)
  (interactive)
  (if (equal "0\n" ;; bluetooth off
             (gr/process-output "blueutil"
                                "-p"))
      (progn
        (shell-command "blueutil -p 1")
        (message "Bluetooth on"))
    (unless arg
      (shell-command "blueutil -p 0")
      (message "Bluetooth off"))))

(defun gr/toggle-headphones ()
  "Toggle bluetooth headphones connection
Uses command-line program blueutil."
  (interactive)
  (let ((inhibit-message t))
    (gr/toggle-bluetooth 1)
    (if (equal "0\n"
               (gr/process-output "blueutil"
                                  "--is-connected"
                                  "1 Soundcore A1"))
        (shell-command "blueutil --connect '1 Soundcore A1'")
      (shell-command "blueutil --disconnect '1 Soundcore A1'"))))

;;; insert dummy heading line

(defun gr/dummy-heading-line ()
  (interactive)
  (insert "* ")
  (insert-char ?* 50)
  (insert " :noheadline:"))

;;; insert line

(defun gr/insert-line (p)
  (interactive "P")
  (let ((fill-prefix nil))
    (cond (p
           (save-excursion
             (end-of-line 0)
             (open-line 1)))
          (t
           (save-excursion
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

(defun gr/select-from-alist (alist &optional input prompt)
  "Use 'completing-read' to return a value from a list of keys ALIST.
Optional initial INPUT."
  (assoc-default
   (completing-read (or prompt "Choose: ") alist nil t input)
   alist))

;;; comment and copy

(defun gr/comment-and-copy (beg end)
  (interactive "r")
  (unless (region-active-p)
    (mark-defun))
  (kill-ring-save beg end t)
  (comment-region beg end)
  (goto-char beg)
  (newline 2)
  (forward-line -2)
  (yank))

;;; backward-delete-word

(defun gr/backward-delete-word ()
  "Like `backward-kill-word', but doesn't add to kill ring."
  (interactive "*")
  (let ((p (point)))
    (forward-word -1)
    (delete-region (point) p)))

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
  (when-let* ((word (or (word-at-point) "")))
    (call-process-shell-command (format "open dict:///%s/" word))))


;;; replace straight quotes with curly

(defun replace-straight-quotes-with-curly (&optional beg end)
  "Replace straight quotes in region or buffer with curly quotes.
Handles both single (') and double (\") quotes.
If region is active, operate on region. Else, operate on entire buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  ;; Handle double quotes (alternates left/right)
  (let ((quote t))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\"" end t)
        (replace-match (if (prog1 quote (setq quote (not quote)))
                           "“" "”")
                       t t))))
  ;; Handle single quotes (apostrophes are preserved)
  ;; This simplistic algorithm considers only quotes around words/spaces. For better handling, use more sophisticated rules.
  (let ((quote t))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\\(\\s-\\|^\\)'" end t)
        (replace-match (concat (match-string 1) (if (prog1 quote (setq quote (not quote))) "‘" "’"))
                       t t))))
  ;; Also replace closing single quote after word character or punctuation, e.g. don't, ain't
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "\\([[:alnum:]]\\)'" end t)
      (replace-match (concat (match-string 1) "’") t t))))

;;; capitalize, upcase, downcase dwim

(defun title-case-word-or-region (beg end)
  "Render string in region in title case."
  (interactive "r")
  (if (use-region-p)
      (save-excursion
        (let* ((input (buffer-substring beg
                                        end))
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
          (replace-string input output nil
                          beg end)))
    (ct/capitalize-word-at-point)))

(defun ct/word-boundary-at-point-or-region (&optional callback)
  "Return the boundary (beginning and end) of the word at point, or region, if any.
Forwards the points to CALLBACK as (CALLBACK p1 p2), if present.

URL: https://christiantietze.de/posts/2021/03/change-case-of-word-at-point/"
  (let ((deactivate-mark nil)
        p1 p2)
    (if (use-region-p)
        (setq p1 (region-beginning)
              p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alpha:]")
        (setq p1 (point))
        (skip-chars-forward "[:alpha:]")
        (setq p2 (point))))
    (when callback
      (funcall callback p1 p2))
    (list p1 p2)))

(defun ct/capitalize-word-at-point ()
  (interactive)
  (ct/word-boundary-at-point-or-region #'upcase-initials-region))

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

;;; convert to ereader

(defun gr/convert-pdf-for-ereader (file &optional args)
  "Convert pdf FILE to ereader format with k2pdfopt."
  (interactive "fFile: \nsArgs: ")
  (let ((path (shell-quote-argument (expand-file-name file))))
    (async-shell-command (concat "convert-for-ereader " path " "
                                 (shell-quote-argument args))
                         "*k2pdfopt-output*" "*k2pdfopt-error*")))

;;; convert docx to org

(defun gr/flush-properties-drawers ()
  "Remove property drawers and custom IDs from current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (flush-lines "^:\\(PROPERTIES\\|CUSTOM_ID.*\\|END\\):$")))

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

(defun gr/clear-empty-org-headings ()
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
