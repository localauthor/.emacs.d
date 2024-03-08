;;; org-gr-extras.el --- Extra functions for org-mode   -*- lexical-binding: t; -*-

(defun gr/org-next-heading ()
  (interactive)
  (let (org-side-tree-narrow-on-jump)
    (if (org-buffer-narrowed-p)
        (progn
          (setq org-side-tree-narrow-on-jump t)
          (org-side-tree-next-heading))
      (org-speed-move-safe 'org-next-visible-heading)
      (org-side-tree-update))))

(defun gr/org-previous-heading ()
  (interactive)
  (let (org-side-tree-narrow-on-jump)
    (if (org-buffer-narrowed-p)
        (progn
          (setq org-side-tree-narrow-on-jump t)
          (org-side-tree-previous-heading))
      (org-speed-move-safe 'org-previous-visible-heading)
      (org-side-tree-update))))

;;; gr/org-return

;; derived from scimax/org-return
;; a better return; inserts list item with RET instead of M-RET

(defun gr/org-return (&optional ignore)
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
     ((or (org-footnote-at-reference-p)
          (org-footnote-at-definition-p))
      (org-footnote-action))

     ;; fall-through case
     (t
      (org-return)))))


;;; org-export

;;   (defun gr/org-export-spacing (backend)
;;     "Single newline is not a paragraph break.
;; Only double newline is a paragraph break."
;;     (cond
;;      ((eq 'latex backend)
;;       (goto-char (point-min))
;;       (while (progn
;;                (forward-paragraph)
;;                (not (eobp)))
;;         (when (looking-at "^$\\|# ")
;;           (kill-line))))))

;;  (defun gr/org-export-spacing (backend)
;;    "Single newline is not a paragraph break.
;; Only double newline is a paragraph break."
;;    (cond
;;     ((eq 'latex backend)
;;      (goto-char (point-min))
;;      (while (progn
;;               (forward-paragraph)
;;               (not (eobp)))
;;        (while (and (looking-at "^$\\|# ")
;;                    (save-excursion
;;                      (forward-line)
;;                      (looking-at ".")))
;;          (kill-line))))))

(defun gr/org-export-spacing (backend)
  "Single newline is not a paragraph break.
  Only double newline is a paragraph break."
  (goto-char (point-min))
  (while (re-search-forward ":noheadline:" nil t)
    (forward-line)
    (insert "\\newline \\indent"))
  (goto-char (point-min))
  (while (progn
           (forward-paragraph)
           (not (eobp)))
    (while (cond ((and (looking-at "^$")
                       (save-excursion
                         (forward-line)
                         (looking-at ".")))
                  (kill-line)
                  t)
                 ((and (looking-at "# ")
                       (save-excursion
                         (forward-line)
                         (looking-at "^$")))
                  (kill-line 2)
                  t)
                 ((and (looking-at "# ")
                       (save-excursion
                         (forward-line)
                         (looking-at ".")))
                  (kill-line)
                  t)))))

(add-hook 'org-export-before-processing-functions #'gr/org-export-spacing)

(with-eval-after-load 'ox
  (add-to-list
   'org-export-smart-quotes-alist
   '("en-us" (primary-opening :utf-8 "“" :html "&ldquo;" :latex "``" :texinfo "``")
     (primary-closing :utf-8 "”" :html "&rdquo;" :latex "''" :texinfo "''")
     (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex "`" :texinfo "`")
     (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "'" :texinfo "'")
     (apostrophe :utf-8 "’" :html "&rsquo;"))))


;;; org narrow/widen

;; Simplified/smart narrow-widen, bound to `C-x n`; call prefix argument `C-u` to narrow further

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
  Dwim means: region, org-src-block, org-subtree, or
  defun, whichever applies first. Narrowing to
  org-src-block actually calls `org-edit-src-code'.

  With prefix P, don't widen, just narrow even if buffer
  is already narrowed."
  (interactive "P")
  ;;(declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;;(define-key endless/toggle-map "n" #'narrow-or-widen-dwim)
;; This line actually replaces Emacs' entire narrowing
;; keymap, that's how much I like this command. Only
;; copy it if that's what you want.

(define-key ctl-x-map "n" #'narrow-or-widen-dwim)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (define-key LaTeX-mode-map "\C-xn"
              nil)))
;;(global-set-key (kbd "C-x n") 'narrow-or-widen-dwim)
(global-set-key (kbd "C-c n") 'narrow-or-widen-dwim)

;;; org-archive

(defun gr/org-mark-done-and-archive-datetree ()
  (interactive)
  (let ((org-archive-location "%s_archive::datetree/"))
    (org-todo 'done)
    (org-archive-subtree)))

(defun gr/org-archive (arg)
  "Without C-u: mark heading DONE and archive to datetree.
With C-u: archive subtree to same hierarchy as in original file."
  (interactive "P")
  (cond ((equal arg '(4)) (gr/org-archive-subtree-hierarchy))
        (t (gr/org-mark-done-and-archive-datetree))))

;; Archive subtrees under the same hierarchy as the original org file.
;; Link: https://gist.github.com/Fuco1/e86fb5e0a5bb71ceafccedb5ca22fcfb
;; Link: https://github.com/daviderestivo/galactic-emacs/blob/master/lisp/org-archive-subtree.el

(defun gr/org-archive-subtree-hierarchy (&optional find-done)
  "Move the current subtree to the archive.
The archive can be a certain top-level heading in the current
file, or in a different file. The tree will be moved to that
location, the subtree heading be marked DONE, and the current
time will be added.

When called with a single prefix argument FIND-DONE, find whole
trees without any open TODO items and archive them (after getting
confirmation from the user). When called with a double prefix
argument, find whole trees with timestamps before today and
archive them (after getting confirmation from the user). If the
cursor is not at a headline when these commands are called, try
all level 1 trees. If the cursor is on a headline, only try the
direct children of this heading."
  (interactive "P")
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
                    'region-start-level 'region))
            org-loop-over-headlines-in-active-region)
        (org-map-entries
         `(progn (setq org-map-continue-from (progn (org-back-to-heading) (point)))
                 (org-archive-subtree ,find-done))
         org-loop-over-headlines-in-active-region
         cl (if (org-invisible-p) (org-end-of-subtree nil t))))
    (cond
     ((equal find-done '(4))  (org-archive-all-done))
     ((equal find-done '(16)) (org-archive-all-old))
     (t
      ;; Save all relevant TODO keyword-related variables.
      (let* ((tr-org-todo-keywords-1 org-todo-keywords-1)
             (tr-org-todo-kwd-alist org-todo-kwd-alist)
             (tr-org-done-keywords org-done-keywords)
             (tr-org-todo-regexp org-todo-regexp)
             (tr-org-todo-line-regexp org-todo-line-regexp)
             (tr-org-odd-levels-only org-odd-levels-only)
             (this-buffer (current-buffer))
             (time (format-time-string
                    (substring (cdr org-time-stamp-formats) 1 -1)))
             (file (abbreviate-file-name
                    (or (buffer-file-name (buffer-base-buffer))
                        (error "No file associated to buffer"))))
             (location (org-archive--compute-location
                        (or (org-entry-get nil "ARCHIVE" 'inherit)
                            org-archive-location)))
             (afile (car location))
             (heading (cdr location))
             (infile-p (equal file (abbreviate-file-name (or afile ""))))
             (newfile-p (and (org-string-nw-p afile)
                             (not (file-exists-p afile))))
             (buffer (cond ((not (org-string-nw-p afile)) this-buffer)
                           ((find-buffer-visiting afile))
                           ((find-file-noselect afile))
                           (t (error "Cannot access file \"%s\"" afile))))
             (org-odd-levels-only
              (if (local-variable-p 'org-odd-levels-only (current-buffer))
                  org-odd-levels-only
                tr-org-odd-levels-only))
             level datetree-date datetree-subheading-p)
        (when (string-match "\\`datetree/\\(\\**\\)" heading)
          ;; "datetree/" corresponds to 3 levels of headings.
          (let ((nsub (length (match-string 1 heading))))
            (setq heading (concat (make-string
                                   (+ (if org-odd-levels-only 5 3)
                                      (* (org-level-increment) nsub))
                                   ?*)
                                  (substring heading (match-end 0))))
            (setq datetree-subheading-p (> nsub 0)))
          (setq datetree-date (org-date-to-gregorian
                               (or (org-entry-get nil "CLOSED" t) time))))
        (if (and (> (length heading) 0)
                 (string-match "^\\*+" heading))
            (setq level (match-end 0))
          (setq heading nil level 0))
        (save-excursion
          (org-back-to-heading t)
          ;; Get context information that will be lost by moving the
          ;; tree.  See `org-archive-save-context-info'.
          (let* ((all-tags (org-get-tags))
                 (local-tags
                  (cl-remove-if (lambda (tag)
                                  (get-text-property 0 'inherited tag))
                                all-tags))
                 (inherited-tags
                  (cl-remove-if-not (lambda (tag)
                                      (get-text-property 0 'inherited tag))
                                    all-tags))
                 (context
                  `((category . ,(org-get-category nil 'force-refresh))
                    (file . ,file)
                    (itags . ,(mapconcat #'identity inherited-tags " "))
                    (ltags . ,(mapconcat #'identity local-tags " "))
                    (olpath . ,(mapconcat #'identity
                                          (org-get-outline-path)
                                          "/"))
                    (time . ,time)
                    (todo . ,(org-entry-get (point) "TODO")))))
            ;; We first only copy, in case something goes wrong
            ;; we need to protect `this-command', to avoid kill-region sets it,
            ;; which would lead to duplication of subtrees
            (let (this-command) (org-copy-subtree 1 nil t))
            (set-buffer buffer)
            ;; Enforce Org mode for the archive buffer
            (if (not (derived-mode-p 'org-mode))
                ;; Force the mode for future visits.
                (let ((org-insert-mode-line-in-empty-file t)
                      (org-inhibit-startup t))
                  (call-interactively 'org-mode)))
            (when (and newfile-p org-archive-file-header-format)
              (goto-char (point-max))
              (insert (format org-archive-file-header-format
                              (buffer-file-name this-buffer))))
            (when datetree-date
              (require 'org-datetree)
              (org-datetree-find-date-create datetree-date)
              (org-narrow-to-subtree))
            ;; Force the TODO keywords of the original buffer
            (let ((org-todo-line-regexp tr-org-todo-line-regexp)
                  (org-todo-keywords-1 tr-org-todo-keywords-1)
                  (org-todo-kwd-alist tr-org-todo-kwd-alist)
                  (org-done-keywords tr-org-done-keywords)
                  (org-todo-regexp tr-org-todo-regexp)
                  (org-todo-line-regexp tr-org-todo-line-regexp))
              (goto-char (point-min))
              (org-show-all '(headings blocks))
              (if (and heading (not (and datetree-date (not datetree-subheading-p))))
                  (progn
                    (if (re-search-forward
                         (concat "^" (regexp-quote heading)
                                 "\\([ \t]+:\\(" org-tag-re ":\\)+\\)?[ \t]*$")
                         nil t)
                        (goto-char (match-end 0))
                      ;; Heading not found, just insert it at the end
                      (goto-char (point-max))
                      (or (bolp) (insert "\n"))
                      ;; datetrees don't need too much spacing
                      (insert (if datetree-date "" "\n") heading "\n")
                      (end-of-line 0))
                    ;; Make the subtree visible
                    (outline-show-subtree)
                    (if org-archive-reversed-order
                        (progn
                          (org-back-to-heading t)
                          (outline-next-heading))
                      (org-end-of-subtree t))
                    (skip-chars-backward " \t\r\n")
                    (and (looking-at "[ \t\r\n]*")
                         ;; datetree archives don't need so much spacing.
                         (replace-match (if datetree-date "\n" "\n\n"))))
                ;; No specific heading, just go to end of file, or to the
                ;; beginning, depending on `org-archive-reversed-order'.
                (if org-archive-reversed-order
                    (progn
                      (goto-char (point-min))
                      (unless (org-at-heading-p) (outline-next-heading)))
                  (goto-char (point-max))
                  ;; Subtree narrowing can let the buffer end on
                  ;; a headline.  `org-paste-subtree' then deletes it.
                  ;; To prevent this, make sure visible part of buffer
                  ;; always terminates on a new line, while limiting
                  ;; number of blank lines in a date tree.
                  (unless (and datetree-date (bolp)) (insert "\n"))))
              ;; Paste
              (org-paste-subtree (org-get-valid-level level (and heading 1)))
              ;; Shall we append inherited tags?
              (and inherited-tags
                   (or (and (eq org-archive-subtree-add-inherited-tags 'infile)
                            infile-p)
                       (eq org-archive-subtree-add-inherited-tags t))
                   (org-set-tags all-tags))
              ;; Mark the entry as done
              (when (and org-archive-mark-done
                         (let ((case-fold-search nil))
                           (looking-at org-todo-line-regexp))
                         (or (not (match-end 2))
                             (not (member (match-string 2) org-done-keywords))))
                (let (org-log-done org-todo-log-states)
                  (org-todo
                   (car (or (member org-archive-mark-done org-done-keywords)
                            org-done-keywords)))))

              ;; Add the context info.
              (dolist (item org-archive-save-context-info)
                (let ((value (cdr (assq item context))))
                  (when (org-string-nw-p value)
                    (org-entry-put
                     (point)
                     (concat "ARCHIVE_" (upcase (symbol-name item)))
                     value))))
              ;; Save the buffer, if it is not the same buffer and
              ;; depending on `org-archive-subtree-save-file-p'.
              (unless (eq this-buffer buffer)
                (when (or (eq org-archive-subtree-save-file-p t)
                          (eq org-archive-subtree-save-file-p
                              (if (boundp 'org-archive-from-agenda)
                                  'from-agenda
                                'from-org)))
                  (save-buffer)))
              (widen))))
        ;; Here we are back in the original buffer.  Everything seems
        ;; to have worked.  So now run hooks, cut the tree and finish
        ;; up.
        (run-hooks 'org-archive-hook)
        (let (this-command) (org-cut-subtree))
        (when (featurep 'org-inlinetask)
          (org-inlinetask-remove-END-maybe))
        (setq org-markers-to-move nil)
        (when org-provide-todo-statistics
          (save-excursion
            ;; Go to parent, even if no children exist.
            (org-up-heading-safe)
            ;; Update cookie of parent.
            (org-update-statistics-cookies nil)))
        (message "Subtree archived %s"
                 (if (eq this-buffer buffer)
                     (concat "under heading: " heading)
                   (concat "in file: " (abbreviate-file-name afile)))))))
    (org-reveal)
    (if (looking-at "^[ \t]*$")
        (outline-next-visible-heading 1))))

;; (use-package dash :defer t)

(defadvice gr/org-archive-subtree-hierarchy (around fix-hierarchy activate)
  (let* ((fix-archive-p (and (not current-prefix-arg)
                             (not (use-region-p))))
         (afile  (car (org-archive--compute-location
                       (or (org-entry-get nil "ARCHIVE" 'inherit) org-archive-location))))
         (buffer (or (find-buffer-visiting afile) (find-file-noselect afile))))
    ad-do-it
    (when fix-archive-p
      (with-current-buffer buffer
        (goto-char (point-max))
        (while (org-up-heading-safe))
        (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
               (path (and olpath (split-string olpath "/")))
               (level 1)
               tree-text)
          (when olpath
            (org-mark-subtree)
            (setq tree-text (buffer-substring (region-beginning) (region-end)))
            (let (this-command) (org-cut-subtree))
            (goto-char (point-min))
            (save-restriction
              (widen)
              (-each path
                (lambda (heading)
                  (if (re-search-forward
                       (rx-to-string
                        `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
                      (org-narrow-to-subtree)
                    (goto-char (point-max))
                    (unless (looking-at "^")
                      (insert "\n"))
                    (insert (make-string level ?*)
                            " "
                            heading
                            "\n"))
                  (cl-incf level)))
              (widen)
              (org-end-of-subtree t t)
              (org-paste-subtree level tree-text))))))))


(defun gr/org-src-block-auto-indent ()
  (interactive)
  (when (org-in-src-block-p)
    (org-edit-special)
    (indent-region (point-min) (point-max))
    (org-edit-src-exit)))

;;(define-key org-mode-map (kbd "C-i") #'gr/org-src-block-auto-indent)

;; moves the pointer off the item marker after converting a heading to an
;; item, because I found it annoying having to move the pointer over in order
;; to begin typing; also just learned how to advise functions, instead of
;; trying to rewrite the function, which I tried before in this case and
;; found too difficult to pursue

;; (advice-add #'org-toggle-item :after #'org-end-of-line)

;;(require 'org-habit)
;;(add-to-list 'org-modules 'ol-habit)



;;; Indirect Buffer -> Split Outline

;;Functions for Using a Split Outline in Org-Mode

;;Problems:
;;- doesn't truncate long lines, so tagged lines wrap
;;- isn't as good as org-sidebar

(defun split-and-indirect-orgtree ()
  "Split window to the right and open an org tree section in it."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (org-tree-to-indirect-buffer)
  (windmove-right))

(defun kill-and-unsplit-orgtree ()
  "Kill the cloned buffer and delete the window."
  (interactive)
  (kill-this-buffer)
  (delete-window))

;;Not for split outline view specifically, but useful generally
(defun close-and-kill-next-pane ()
  "When multiple windows, close other pane and kill its buffer."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

;; (define-key org-mode-map (kbd "C-c o") #'split-and-indirect-orgtree)
;; (define-key org-mode-map (kbd "C-c b")  #'kill-and-unsplit-orgtree)

(provide 'gr-org-extras)
