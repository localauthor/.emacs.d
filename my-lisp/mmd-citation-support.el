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

(defvar gr/all-cite-keys (mapcar (lambda (x) (nth 1 x)) (citar--get-candidates)))

;; Getting all-keys by putting (citar--get-candidates) in the activate
;; function 'gr/mmd-citation-activate' makes things realllly slow when typing
;; in the buffer, since font-lock calls that function repeatedly, so we
;; instead put all-keys in a variable 'gr/all-cite-keys', above.

;; However, the variable will be out of sync unless we refresh it, like so:

(defun gr/refresh-cite-keys (&rest _)
  "Refresh 'gr/all-cite-keys'."
  (interactive)
  (setq gr/all-cite-keys (mapcar (lambda (x) (nth 1 x)) (citar--get-candidates))))

(advice-add #'citar-refresh :after #'gr/refresh-cite-keys)

;;;###autoload
(defun gr/mmd-citation-activate (limit)
  "Activate font-lock on mmd-citations up to LIMIT."
  (when (re-search-forward gr/full-mmd-citation-regexp limit t)
    (if (member (match-string 4) gr/all-cite-keys)
        (progn
          (add-text-properties (- (match-beginning 4) 1) ;; -1 to match the #
                               (match-end 4)
                               '(font-lock-face font-lock-keyword-face
                                                help-echo mmd-tooltip))
          t)
      (progn
        (add-text-properties (- (match-beginning 4) 1)
                             (match-end 4)
                             '(font-lock-face font-lock-warning-face))
        t))))

(font-lock-add-keywords 'org-mode
                        '((gr/mmd-citation-activate)))

(font-lock-add-keywords 'outline-mode
                        '((gr/mmd-citation-activate)))


;; different ways to get list of all citekeys
;; (gr/bibtex-all-field-values gr/bibliography "=key=")
;; or: (hash-table-keys (parsebib-parse gr/bibliography)) ;; slow
;; or: (progn (ebib) (ebib--list-keys ebib--cur-db))
;; or: (ebib-db-list-keys ebib--cur-db)
;; or: (org-cite-basic--all-keys) ;; (require 'oc)
;; or: (mapcar (lambda (x) (nth 1 x)) citar--candidates-cache) ;; faaast, but need cache first
;; or: (mapcar (lambda (x) (elt x 1)) citar--candidates-cache)
;; or: (mapcar (lambda (x) (nth 1 x)) (citar--get-candidates)) ;; (require 'citar); and slow to run each tme


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
    (let* ((citar-templates
            '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
              (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
              (preview . "${author editor} (${year issued date})\n${title}\n${journal publisher}")
              (note . "Notes on ${author editor}, ${title}")))
           (mmd-citation (progn
                           (when (thing-at-point-looking-at "[#|\\[]")
                             (forward-char 2))
                           (list (thing-at-point 'symbol t))))
           (entry (citar--ensure-entries mmd-citation)))
      (if entry
          (citar-format-reference entry)
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

;;;###autoload
(defun gr/citar-mmd-insert-citation (key-entry)
  "Insert BibTeX KEY-ENTRY in mmd format, with option to include PAGES."
  (interactive (list (citar-select-ref)))
  (let* ((pages (read-from-minibuffer "Pages: "))
         (mmd (format "[#%s]" (car key-entry))))
    (if (string= "" pages) (insert mmd)
      (insert (format "[%s]" pages) mmd))
    (kill-new mmd)))

;;; link-hint integration

(defun link-hint--mmd-citation-at-point-p ()
  "Return t if mmd-citation at point."
  (thing-at-point-looking-at gr/mmd-citation-regexp))

(defun link-hint--next-mmd-citation (bound)
  "Find next mmd-citation in buffer, up to BOUND."
  (link-hint--next-regexp gr/mmd-citation-regexp bound))

(defun link-hint--open-mmd-citation ()
  "Call 'citar-open' on mmd-citation key at point."
  (let* ((cite (when (thing-at-point-looking-at "[#|\\[]")
                (progn
                  (forward-char 2)
                  (substring-no-properties (thing-at-point 'symbol))))))
    (citar-open (list (append (list cite) (citar--get-entry cite))))))

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
    ("r" citar-copy-reference)
    ("e" citar-ebib-jump-to-entry)
    ("F" citar-open-library-file)
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
      (when (and (re-search-backward "\\[\\#"
                                     (line-beginning-position)
                                     t)
                 (save-excursion
                   (not (search-forward "]" origin t))))
        (let ((begin (match-end 0))
              (end origin))
          (list begin end
                (completion-table-dynamic
                 (lambda (_)
                   (citar--get-candidates)))
                :exit-function
                (lambda (str _status)
                  ;; take completion str and replace with key
                  (delete-char (- (length str)))
                  (insert (car (last (split-string str))) "]"))))))))

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
