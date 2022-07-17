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
;; (require 'citar-file)
;; (require 'citar-citeproc)

;; (require 'oc-csl)
;; (require 'thingatpt)

;; (require 'embark)
;; (require 'link-hint)


;;; variables

(defvar gr/mmd-citation-regexp "\\[#.[[:alpha:]-']+[[:digit:]]\\{4\\}.?]")
(defvar gr/full-mmd-citation-regexp "\\(?1:\\[\\(?3:[^#][^]]*\\)]\\)?\\(?2:\\[#\\(?4:[[:alpha:]-']*?[[:digit:]]\\{4\\}.?\\)]\\)")

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
  (if (member key (hash-table-keys (citar-get-entries)))
      (add-text-properties beg end
                           '(font-lock-face font-lock-keyword-face
                                            help-echo mmd-tooltip))
    (add-text-properties beg end
                         '(font-lock-face font-lock-warning-face))))


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
                          (citar-get-value "journal" key))))
      (if key
          (format "%s\n%s\n%s" author title publisher)
        (message "No record")))))

;;;###autoload
(defun mmd-tooltip-toggle ()
  "Toggle 'mmd-tooltip'."
  (interactive)
  (if (bound-and-true-p mmd-tooltip-enable)
      (progn
        (setq mmd-tooltip-enable nil)
        (message "mmd-tooltips off"))
    (progn
      (setq mmd-tooltip-enable t)
      (message "mmd-tooltips on"))))

;;; citar integration

(defvar gr/last-mmd-citation nil)

;;;###autoload
(defun gr/citar-mmd-insert-citation ()
  "Insert BibTeX KEY-ENTRY in mmd format, with option to include PAGES."
  (interactive)
  (if current-prefix-arg
      (insert gr/last-mmd-citation)
    (let* ((key (citar-select-ref))
           (pages (read-from-minibuffer "Pages: "))
           (mmd (format "[#%s]"  key)))
      (if (string= "" pages) (insert mmd)
        (insert (format "[%s]" pages) mmd))
      (setq gr/last-mmd-citation mmd)
      (kill-new mmd))))

;;; link-hint integration

(defun link-hint--mmd-citation-at-point-p ()
  "Return t if mmd-citation at point."
  (thing-at-point-looking-at gr/mmd-citation-regexp))

(defun link-hint--next-mmd-citation (bound)
  "Find next mmd-citation in buffer, up to BOUND."
  (link-hint--next-regexp gr/mmd-citation-regexp bound))

(defun link-hint--open-mmd-citation ()
  "Call 'citar-open' on mmd-citation key at point."
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

  (embark-define-keymap embark-mmd-citation-map
    "Keymap for Embark comment actions."
    ("RET" citar-open)
    ("z" zk-search)
    ("f" devonthink-dir-find-file)
    ("k" citar-copy-reference)
    ("e" citar-ebib-jump-to-entry)
    ("F" citar-open-files)
    ("o" citar-open)
    ("n" citar-open-notes))

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
      (let* ((proc (citeproc-create (concat citar-citeproc-csl-styles-dir "/" citar-citeproc-csl-style)
                                    (citeproc-itemgetter-from-bibtex citar-bibliography)
                                    (citeproc-locale-getter-from-dir org-cite-csl-locales-dir)
                                    "en-US"))
             (rendered-citations (progn
                                   (citeproc-add-uncited keys proc)
                                   (citeproc-render-bib proc 'plain))))
        (goto-char (point-max))
        (insert (concat "\n* Bibliography\n\n" (car rendered-citations)))
        (when
            (derived-mode-p 'org-mode)
          (org-find-olp '("Bibliography") 'this-buffer)))
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
