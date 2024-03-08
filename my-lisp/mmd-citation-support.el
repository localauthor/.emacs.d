;;; mmd-citation-support.el --- Add support for multi-markdown style citations    -*- lexical-binding: t; -*-

;;; Commentary:

;;
;; Adds support for multi-markdown citations to citar, link-hint, and embark
;;
;; mmd-citations are in the style [#AuthorYEAR] or [23-25][#AuthorYEAR]

;; (require 'devonthink-dir)
;; (require 'ebib-extras)

;;; Code:

(require 'citar)
(require 'citar-file)
(require 'citar-citeproc)

(require 'ebib-extras)

(require 'oc-csl)
(require 'thingatpt)

(require 'embark)
(require 'link-hint)


;;; variables

(defvar zk-desktop-directory "~/Dropbox/ZK/ZK-Desktops")

(defvar gr/mmd-citation-regexp "\\[#.[[:alpha:]-']+[[:digit:]]\\{4\\}.?]")
(defvar gr/full-mmd-citation-regexp "\\(?1:\\[\\(?3:[^#][^]]*\\)]\\)?\\(?2:\\[#\\(?4:[[:alpha:]-']*?[[:digit:]]\\{4\\}.?\\)]\\)")
(defvar citar-citeproc-csl-style)

;;; add highlighting and tooltips to mmd-citekeys

(defun gr/mmd-citation-activate (limit)
  "Activate font-lock on mmd-citations up to LIMIT."
  (when (re-search-forward gr/full-mmd-citation-regexp limit t)
    (let ((beg (- (match-beginning 4) 1)) ;; -1 to match the #
          (end (match-end 4))
          (key (match-string 4)))
      (funcall 'gr/mmd-citation-fontify key beg end)
      t)))

(defun gr/mmd-citation-fontify (key beg end)
  "Fontify mmd-citation KEY, from BEG to END."
  (if (member key (hash-table-keys (citar-get-entries)))
      (add-text-properties beg end
                           '(font-lock-face font-lock-keyword-face
                                            help-echo mmd-tooltip))
    (add-text-properties beg end
                         '(font-lock-face font-lock-warning-face
                                          help-echo "No record"))))


(font-lock-add-keywords 'org-mode
                        '((gr/mmd-citation-activate)))

(font-lock-add-keywords 'outline-mode
                        '((gr/mmd-citation-activate)))


;; different ways to get list of all citekeys
;; or: (hash-table-keys (parsebib-parse gr/bibliography)) ;; slow
;; or: (org-cite-basic--all-keys) ;; (require 'oc)
;; (hash-table-keys (citar-get-entries)) FAST

(defvar mmd-tooltip-enable t)

(setq tooltip-delay 0.1)

;; auto show tooltip in echo area
;; (progn
;;   (setq help-at-pt-display-when-idle nil)
;;   (setq help-at-pt-timer-delay 0.5)
;;   (help-at-pt-set-timer))

;;;###autoload
(defun mmd-tooltip (_win _obj pos)
  "Generate tooltip for mmd-citation at POS."
  (save-excursion
    (goto-char pos)
    (let* ((key (progn
                  (when (thing-at-point-looking-at "[#|\\[]")
                    (forward-char 2))
                  (thing-at-point 'symbol t)))
           (author (or (citar-get-value "author" key)
                       (citar-get-value "editor" key)))
           (title (or (citar-get-value "title" key)
                      (citar-get-value "booktitle" key)))
           (publisher (or (citar-get-value "publisher" key)
                          (citar-get-value "journal" key)))
           (notep (if (funcall (citar-has-notes) key) "✓" "✗"))
           (filep (if (funcall (citar-has-files) key) "✓" "✗")))
      (format "%s\n%s\n%s\nfile: %s note: %s" author title publisher filep notep))))

;;;###autoload
(defun mmd-tooltip-toggle ()
  "Toggle `mmd-tooltip'."
  (interactive)
  (if (bound-and-true-p mmd-tooltip-enable)
      (progn
        (setq mmd-tooltip-enable nil)
        (message "mmd-tooltips off"))
    (progn
      (setq mmd-tooltip-enable t)
      (message "mmd-tooltips on"))))

;;; citar integration

(defvar gr/last-mmd-citation-key nil)
(defvar-local gr/mmd-citation-use nil)

;;;###autoload
(defun gr/format-mmd-citation (key &optional pages)
  "Return BibTeX KEY in mmd format, with option to include pages."
  (let* ((mmd (format "[#%s]" key))
         (cite))
    (if (or (not pages)
            (string= "" pages))
        (setq cite mmd)
      (setq cite (concat (format "[%s]" pages) mmd)))
    (kill-new mmd)
    cite))

;;;###autoload
(defun gr/citar-insert-citation (&optional key pages)
  "Insert cite-key, format depending on context.
When in zk file, mmd format; when `org-mode', org-cite."
  (interactive)
  (unless (derived-mode-p 'text-mode)
    (error "Not a text mode"))
  (let ((key (or key
                 (if (and gr/last-mmd-citation-key
                          current-prefix-arg)
                     gr/last-mmd-citation-key
                   (citar-select-ref))))
        (pages (or pages
                   (unless (looking-back "]" (- (point) 1))
                     (read-from-minibuffer "Pages: ")))))
    (if (or (zk-file-p)
            (string= "*scratch*" (buffer-name))
            (file-in-directory-p (or buffer-file-name
                                     ;;for indirect clones
                                     (buffer-file-name
                                      (buffer-base-buffer)))
                                 zk-desktop-directory)
            gr/mmd-citation-use)
        (insert (gr/format-mmd-citation key pages))
      (citar-insert-citation (list key))
      (unless (or (not pages)
                  (string= "" pages))
        (if (citar-citation-at-point)
            (save-excursion
              (forward-char -1)
              (insert " " pages))
          (insert (gr/format-mmd-citation key pages)))))
    (setq gr/last-mmd-citation-key key)))

(defun gr/mmd-citation-at-point ()
  "When mmd-citation is at point, return citekey."
  (interactive)
  (when
      (save-excursion
        (re-search-backward "\\[#")
        (thing-at-point-looking-at gr/mmd-citation-regexp))
    (thing-at-point 'symbol t)))

(defun gr/mmd-citation-convert (key)
  "Convert citation at point to/from org and mmd."
  (interactive (list (or (citar-key-at-point)
                         (gr/mmd-citation-at-point))))
  (let* ((beg (save-excursion
                (re-search-backward "[^]\\]\\(\\[\\)\\|\\(\\[\\)c")
                (or (match-beginning 1)
                    (match-beginning 2))))
         (end (save-excursion
                (re-search-forward "]")
                (point)))
         (post (save-excursion
                 (if (citar-key-at-point)
                     (progn
                       (goto-char beg)
                       (if (re-search-forward " \\(.*\\)]" end t)
                           (match-string 1) ""))
                   (progn
                     (goto-char beg)
                     (re-search-forward "\\[\\(.*\\)]\\["
                                        end t)
                     (match-string 1))))))
    (if (not (citar-key-at-point))
        (progn
          (delete-region beg end)
          (citar-org-insert-citation (list key))
          (when post
            (forward-char -1)
            (insert " " post)
            (forward-char 1)))
      (let ((gr/mmd-citation-use t)) ;; org to mmd
        (delete-region beg end)
        (gr/citar-insert-citation key post)))))

(defun gr/mmd-citation-convert-buffer ()
  (interactive)
  (while
      (re-search-forward "\\[cite:")
    (gr/mmd-citation-convert (citar-key-at-point))))

;;; link-hint integration

(defun link-hint--mmd-citation-at-point-p ()
  "Return t if mmd-citation at point."
  (thing-at-point-looking-at gr/mmd-citation-regexp))

(defun link-hint--next-mmd-citation (bound)
  "Find next mmd-citation in buffer, up to BOUND."
  (link-hint--next-regexp gr/mmd-citation-regexp bound))

(defun link-hint--open-mmd-citation ()
  "Call `citar-open' on mmd-citation key at point."
  (let* ((key (when (thing-at-point-looking-at "[#|\\[]")
                (progn
                  (forward-char 2)
                  (substring-no-properties (thing-at-point 'symbol))))))
    (citar-open (list key))))

(link-hint-define-type 'mmd-citation
  :next #'link-hint--next-mmd-citation
  :at-point-p #'link-hint--mmd-citation-at-point-p
  :open #'link-hint--open-mmd-citation
  :copy #'kill-new
  :aw-select #'link-hint--open-mmd-citation)

(push 'link-hint-mmd-citation link-hint-types)


;;; embark integration

(with-eval-after-load 'embark
  (defun embark-target-mmd-citation-at-point ()
    "Target a multimarkdown style citation at point."
    ;; Includes a hack, a result of limitation of (thing-at-point 'symbol), to
    ;; allow accurate target identification when point is on "[" or "#" at
    ;; beginning of mmd citation maybe better to do thing-at-point thing
    ;; manually, like citar does for markdown?
    (when (thing-at-point-looking-at gr/mmd-citation-regexp)
      (when (thing-at-point-looking-at "[#|\\[]")
        (forward-char 2))
      (let ((mmd-citation (thing-at-point 'symbol t)))
        `(mmd-citation ,mmd-citation . ,(bounds-of-thing-at-point 'symbol)))))

  (defvar-keymap embark-mmd-citation-map
    :doc "Keymap for Embark comment actions."
    :parent embark-general-map
    "RET" #'citar-open
    "z" #'zk-search
    "k" #'citar-copy-reference
    "e" #'citar-ebib-jump-to-entry
    "f" #'citar-open-files
    "o" #'citar-open
    "n" #'citar-open-notes)

  (add-to-list 'embark-keymap-alist '(mmd-citation . embark-mmd-citation-map))
  (add-to-list 'embark-target-finders 'embark-target-mmd-citation-at-point)

  )

;;; append-bibliography

(defun gr/list-buffer-mmd-citations ()
  "Return a list of all keys from mmd-citations in buffer."
  (interactive)
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match gr/mmd-citation-regexp (buffer-string) pos)
        (setq pos (match-end 0))
        (push (car (split-string (match-string-no-properties 0 (buffer-string)) nil t "\\[#\\|\\]")) matches))
      (delete-dups matches))))

;; append bibliography, compiled from citekeys in current file
(defun gr/append-bibliography ()
  "Append formatted bibliography to end of current file.
Collects mmd-citation keys from current buffer."
  (interactive)
  (unless citar-citeproc-csl-style
    (citar-citeproc-select-csl-style))
  (if-let ((keys (gr/list-buffer-mmd-citations)))
      (let* ((heading (ignore-errors (org-find-olp '("Bibliography") 'this-buffer)))
             (proc (citeproc-create (concat citar-citeproc-csl-styles-dir "/" citar-citeproc-csl-style)
                                    (citeproc-itemgetter-from-bibtex citar-bibliography)
                                    (citeproc-locale-getter-from-dir org-cite-csl-locales-dir)
                                    "en-US"))
             (rendered-citations (progn
                                   (citeproc-add-uncited keys proc)
                                   (citeproc-render-bib proc 'plain))))
        (if heading
            (progn
              (org-goto-marker-or-bmk heading)
              (org-cut-subtree))
          (goto-char (point-max)))
        (insert (concat "\n* Bibliography\n\n" (car rendered-citations) "\n\n"))
        (org-previous-visible-heading 1))
    (error "No citations")))

;;; convert mmd-citations to pandoc or org-mode

(defun gr/convert-citations-mmd-to-pandoc (&optional file)
  "Convert citations in buffer from mmd to pandoc style.
Optional FILE."
  (interactive)
  (when file
    (find-file file))
  (goto-char (point-min))
  (save-match-data
    (while (re-search-forward gr/full-mmd-citation-regexp nil t)
      (let ((pos)
            (prefix (match-string-no-properties 1))
            (new-cite (string-replace "#" "@" (match-string-no-properties 2)))
            (internals (match-string-no-properties 3)))
        (setq pos (match-end 2))
        (if prefix
            (delete-region (match-beginning 1) (match-end 2))
          (delete-region (match-beginning 2) (match-end 2)))
        (insert new-cite)
        (backward-char 1)
        (cond
         ((not internals))
         ((string-match "^[0-9]" internals)
          (insert (concat ", p. " internals)))
         ((and internals)
          (insert (concat ", " internals))))
        (goto-char pos)))
    (save-buffer)
    (if file
        (kill-buffer))
    ))

(defun gr/convert-citations-mmd-to-org-cite (&optional file)
  "Convert citations in buffer from mmd to pandoc style.
Optional FILE."
  (interactive)
  (when file
    (find-file file))
  (goto-char (point-min))
  (save-match-data
    (while (re-search-forward gr/full-mmd-citation-regexp nil t)
      (let ((pos)
            (prefix (match-string-no-properties 1))
            (new-cite (string-replace "#" "cite:@" (match-string-no-properties 2)))
            (internals (match-string-no-properties 3)))
        (setq pos (match-end 2))
        (if prefix
            (delete-region (match-beginning 1) (match-end 2))
          (delete-region (match-beginning 2) (match-end 2)))
        (insert new-cite)
        (backward-char 1)
        (cond
         ((not internals))
         ((string-match "^[0-9]" internals)
          (insert (concat " p. " internals)))
         ((and internals)
          (insert (concat " " internals))))
        (goto-char pos)))
    (save-buffer)
    (if file
        (kill-buffer))
    ))

;; ;;; mmd completion-at-point

(defun gr/mmd-citation-completion-at-point ()
  "Complete mmd-citations at point."
  (save-excursion
    (let ((origin (point)))
      (when (and (re-search-backward
                  "\\[\\#"
                  (line-beginning-position)
                  t)
                 (save-excursion
                   (not (search-forward
                         "]" origin t))))
        (let ((begin (match-end 0))
              (end origin))
          (list begin end
                (completion-table-dynamic
                 (lambda (_)
                   (citar--format-candidates)))
                :exit-function
                ;; take completion str and replace with key
                (lambda (str _status)
                  (delete-char (- (length str)))
                  (insert (citar--extract-candidate-citekey str) "]"))))))))

;; (add-to-list 'completion-at-point-functions 'gr/mmd-citation-completion-at-point)

;; (defun citation-key-completion-at-point ()
;;   "Completion-at-point function for citation-keys."
;;   (let ((keys (hash-table-keys (parsebib-parse citar-bibliography)))
;;         (candidates)
;;         (case-fold-search t)
;;         (pt (point)))
;;     (dolist (key keys)
;;       (push (format "@%s" key) candidates))
;;     (save-excursion
;;       (when (re-search-backward "@.?" nil t)
;;         (list (match-beginning 0)
;;               pt
;;               candidates
;;               :exclusive 'no)))))

(provide 'mmd-citation-support)
;;; mmd-citation-support.el ends here
