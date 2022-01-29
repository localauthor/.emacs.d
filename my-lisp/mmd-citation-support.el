;;; mmd-citation-support.el --- Add support for multi-markdown style citations    -*- lexical-binding: t; -*-

;; NOTES:
;;
;; Adds support for multi-markdown style citations to citar, link-hint, and embark support
;;
;; mmd-citations are in the style [#AuthorYEAR] or [23-25][#AuthorYEAR]

(require 'citar)
(require 'citar-file)
(require 'citar-citeproc)
(require 'devonthink-dir)
(require 'oc-csl)
(require 'thingatpt)
(require 'embark)
(require 'link-hint)
(require 'ebib-extras)

;;; variables

(defvar gr/mmd-citation-regexp "\\[#.[[:alpha:]-']+[[:digit:]]\\{4\\}.?]")
(defvar gr/full-mmd-citation-regex "\\(?1:\\[\\(?3:[^#][^]]*\\)]\\)?\\(?2:\\[#.[[:alpha:]-']+[[:digit:]]\\{4\\}.?]\\)")


;;; add highlighting and tooltips to mmd-citekeys

;; (font-lock-add-keywords 'org-mode '(("\\[@.*?\\]" . font-lock-keyword-face)
;;                                     ("\\[-@.*?\\]" . font-lock-keyword-face)
;;                                     ("\\[#.*?\\]" . font-lock-keyword-face)
;;                                     ("\\[-#.*?\\]" . font-lock-keyword-face)))

(font-lock-add-keywords 'org-mode '(("\\[#.*?\\]" 0 '(face font-lock-keyword-face
                                                           help-echo mmd-tooltip))
                                    ("\\[-#.*?\\]" 0 '(face font-lock-keyword-face
                                                            help-echo mmd-tooltip))))

;; (font-lock-add-keywords 'outline-mode '(("\\[@.*?\\]" . font-lock-keyword-face)
;;                                         ("\\[-@.*?\\]" . font-lock-keyword-face)
;;                                         ("\\[#.*?\\]" . font-lock-keyword-face)
;;                                         ("\\[-#.*?\\]" . font-lock-keyword-face)))

(font-lock-add-keywords 'outline-mode '(("\\[#.*?\\]" 0 '(face font-lock-keyword-face
                                                               help-echo mmd-tooltip))
                                        ("\\[-#.*?\\]" 0 '(face font-lock-keyword-face
                                                                help-echo mmd-tooltip))))

(defun mmd-tooltip (_win _obj pos)
  (save-excursion
    (goto-char pos)
    (let* ((mmd-citation (list (thing-at-point 'symbol t)))
           (citar-citeproc-csl-style "chicago-fullnote-bibliography-short-title-subsequent.csl"))
      (citar-citeproc-format-reference mmd-citation))))

;;; citar integration

(defun gr/citar-mmd-insert-citation (key-entry)
  "Insert BibTeX KEYS in mmd format, with option to include PAGES."
  (interactive (list (citar-select-ref)))
  (let* ((pages (read-from-minibuffer "Pages: "))
         (pages  (if (string= "" pages) "" (concat pages ""))))
    (if (string= "" pages)
        (insert (format "[#%s]" (car key-entry)))
      (insert (format "[%s][#%s]" pages (car key-entry)))
      )))


;;; link-hint integration

(defun link-hint--mmd-citation-at-point-p ()
  (thing-at-point-looking-at gr/mmd-citation-regexp))

(defun link-hint--next-mmd-citation (&optional _bound)
  (link-hint--next-regexp gr/mmd-citation-regexp))

(defun link-hint--open-mmd-citation ()
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

(defun embark-target-mmd-citation-at-point ()
  "Target a multimarkdown style citation at point."
  ;; Includes a hack, a result of limitation of (thing-at-point 'symbol), to allow accurate target identification when point is on "[" or "#" at beginning of mmd citation
  ;; maybe better to do thing-at-point thing manually, like citar does for markdown?
  (when (thing-at-point-looking-at "\\[#.[[:alpha:]-']+[[:digit:]]\\{4\\}]")
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


;;; append-bibliography

(defun gr/list-buffer-mmd-citations ()
  "Returns a list of all keys from mmd-citations in buffer."
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
  (let* ((keys (gr/list-buffer-mmd-citations))
         (proc (citeproc-create (concat citar-citeproc-csl-styles-dir "/" citar-citeproc-csl-style)
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
      (org-find-olp '("Bibliography") 'this-buffer))))


;;; convert mmd-citations to pandoc or org-mode

(defun gr/convert-citations-mmd-to-pandoc (&optional file)
  "Convert citations in buffer from mmd to pandoc style."
  (interactive)
  (when file
    (find-file file))
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward gr/full-mmd-citation-regex nil t)
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
  "Convert citations in buffer from mmd to pandoc style."
  (interactive)
  (when file
    (find-file file))
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward gr/full-mmd-citation-regex nil t)
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

(provide 'mmd-citation-support)
