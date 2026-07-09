;;; zk-setup.el --- Setup for zk, zk-index, zk-luhmann, etc.  -*- lexical-binding: t; -*-

(require 'gr-functions)
(require 'embark)
(require 'embark-org)
;; (require 'hydra)

;;; zk

(use-package zk
  :ensure nil
  :demand t
  ;;:commands (zk-org-try-to-follow-link)
  :bind
  (:map zk-file-map
        ("G" . zk-luhmann-index-goto)
        ("f" . gr/zk-find-file-other-frame)
        ("t" . gr/zk-find-file-other-tab))
  (:map embark-region-map
        ("N" . zk-new-note)
        ("z" . zk-search))
  (:map embark-org-heading-map
        ("w" . gr/zk-org-copy-subtree))
  (:map embark-identifier-map
        ("z" . zk-search))
  (:map zk-id-map
        ("f" . gr/zk-find-file-other-frame)
        ("t" . gr/zk-find-file-other-tab)
        ("r" . gr/zk-id-title-echo)
        ("s" . zk-search)
        ("z" . zk-grep) ;; zk-consult-grep does not work as embark action
        ("G" . zk-luhmann-index-goto)
        ("o" . link-hint--aw-select-zk-link))
  :hook
  (completion-at-point-functions . zk-completion-at-point)
  (completion-at-point-functions . gr/mmd-citation-completion-at-point)
  :custom
  (zk-directory "~/Documents/ZK")
  (zk-file-extension "org")
  (zk-tag-regexp "\\s#[a-zA-Z0-9]\\+")
  (zk-new-note-header-function #'gr/zk-new-note-header)
  (zk-tag-insert-function 'gr/zk-insert-tag)
  (zk-link-and-title 'ask)
  (zk-new-note-link-insert 'ask)
  (zk-link-format "[[%s]]")
  (zk-link-and-title-format "%t: [[%i]]")
  (zk-completion-at-point-format "%t: [[%i]]")
  (zk-search-function #'zk-consult-grep) ;; #'zk-grep ;; #'zk-xref)
  (zk-current-notes-function nil)
  :config
  (zk-setup-auto-link-buttons)
  (zk-setup-embark)

  (defun gr/zk-id-title-echo ()
    (interactive)
    (message "%s"
             (zk--parse-id 'title (zk--id-at-point))))

  (defun gr/zk-org-copy-subtree ()
    (interactive)
    (if-let* ((id (ignore-errors (zk--current-id)))
              (heading (org-get-heading))
              (subtree (org-get-entry))
              (source (concat "SOURCE: [[" id "]]")))
        (progn
          (kill-new (concat heading "\n\n" source "\n" subtree))
          (message "Subtree copied as kill"))
      (org-copy-subtree)))

  (defun zk-index-embark-clear-selection (&rest _)
    "Clear buffer-local candidates selected with `embark-select'."
    (mapc
     (lambda (x) (delete-overlay (cdr x)))
     embark--selection)
    (setq-local embark--selection nil))

  (defun gr/zk-insert-tag (tag)
    (goto-char (point-min))
    (when (re-search-forward "tags:" nil t)
      (goto-char (match-beginning 0))
      (end-of-line)
      (insert tag)))

  (with-eval-after-load 'embark
    (add-to-list 'embark-become-keymaps
                 'embark-become-zk-file-map)
    (add-to-list 'embark-post-action-hooks
                 '(zk-index-narrow zk-index-embark-clear-selection))
    (add-to-list 'embark-post-action-hooks
                 '(zk-index-insert-link zk-index-embark-clear-selection))
    (add-to-list 'embark-post-action-hooks
                 '(zk-copy-link-and-title
                   zk-index-embark-clear-selection))))

(defun tab-bar-tab-name-format-zk-file (name _tab _i)
  "Remove zk-id from tab-name of zk files."
  (if (string-match (zk-file-name-regexp) name)
      (concat "zk:" (match-string 2 name))
    name))

(add-to-list 'tab-bar-tab-name-format-functions
             'tab-bar-tab-name-format-zk-file)

(defun gr/zk-find-file-other-tab (arg)
  (interactive (list (funcall zk-select-file-function "Find file: ")))
  (let* ((filename (car (zk--processor arg)))
         (file (find-file-noselect filename)))
    ;; instead of find-file-other-tab, which obeys my display-buffer-alist
    ;; and therefore opens zk-buffers in same window, not new tab
    (tab-bar-new-tab)
    (switch-to-buffer file)))

(defun gr/zk-find-file-other-frame (arg)
  (interactive (list (funcall zk-select-file-function "Find file: ")))
  (let* ((filename (car (zk--processor arg)))
         (file (find-file-noselect filename)))
    ;; instead of find-file-other-frame, which obeys my
    ;; display-buffer-alist
    ;; and therefore opens zk-buffers in same frame
    (make-frame)
    (switch-to-buffer file)))

(defun gr/zk-new-note-header (title new-id &optional orig-id)
  "Insert header in new notes with args TITLE and NEW-ID.
  Optionally use ORIG-ID for backlink."
  (insert (format "#+TITLE: %s %s\n\n#+tags: \n" new-id title))
  (when (ignore-errors (zk--parse-id 'title orig-id)) ;; check for file
    (progn
      (insert "\n<- ")
      (zk--insert-link-and-title orig-id)
      (newline)))
  (insert "===\n\n"))


;;; better org-link handling

(defun zk-follow-org-link-at-point (&optional id)
  "Open note that corresponds with the zk ID at point."
  (interactive)
  (when-let* ((id (zk--id-at-point)))
    (find-file (zk--parse-id 'file-path id))))

(with-eval-after-load 'org
  (add-to-list 'org-open-at-point-functions #'zk-follow-org-link-at-point))


;;; org style links - EXPERIMENTAL

;;;; add zk completion in org-insert-link

(with-eval-after-load 'ol
  (setq org-link-make-description-function #'gr/zk-org-link--description)

  (defun gr/zk-org-link--description (id desc)
    (when (string-match zk-id-regexp id)
      (if desc desc
        (zk--parse-id 'title (match-string 1 id)))))

  (org-link-set-parameters "zk"
                           :complete
                           (lambda ()
                             (zk--parse-file
                              'id
                              (zk--select-file))))

  (advice-add 'org-insert-link :after #'zk-make-org-link-buttons)

  (define-advice zk--backlinks-list
      (:around (orig-func &rest args) zk-org-backlinks-list)
    "find zk-ids in org-link format."
    (let ((zk-link-format "[[%s]]?"))
      (apply orig-func args)))

  (define-advice zk-link-regexp
      (:override nil zk-org-link-regexp)
    "adjust zk-link-regexp for org-link format."
    (or zk--link-regexp-cache
        (setq zk--link-regexp-cache
              (format "\\[\\[%s\\(?:\\]\\]\\|\\]\\[.*?\\]\\]\\)" zk-id-regexp))))

  ;; maybe simpler/sufficient to just use `[[%s]]?’

  ;; NOTE: org-style links break zk--grep-link-id-list; here’s a fix:

  (define-advice zk--grep-link-id-list
      (:around (orig-func &rest args) zk-org-grep-link-id-list)
    "find zk-ids in org-link format."
    (let ((zk--link-regexp-cache (format (regexp-quote "[[%s]") zk-id-regexp)))
      (apply orig-func args)))

  (defun gr/zk-insert-org-link (beg end)
    "If active region, use region as description, otherwise zk-insert-link."
    (interactive "R")
    (if beg
        (let* ((file (funcall zk-select-file-function "Insert link: "))
               (id (zk--parse-file 'id file))
               (desc (buffer-substring beg end)))
          (delete-region beg end)
          (insert (format "[[%s][%s]]" id desc))
          (zk-make-org-link-buttons))
      (call-interactively #'zk-insert-link)))

  ;; maybe defined out of order?
  ;; (define-key zk-keymap "i" #'gr/zk-insert-org-link)

  )

;;;; alternative to zk-make-link-buttons, for help-echos

(with-eval-after-load 'zk
  (defun zk-make-org-link-buttons ()
    "Make org-style zk links in current buffer into zk-link buttons."
    (interactive)
    (when (and (zk-file-p)
               zk-enable-link-buttons)
      (let* ((zk--no-gc t)
             (ids (zk--id-list)))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward (zk-link-regexp) nil t)
            (let ((beg (match-beginning 0)) ;; match whole link
                  (end (match-end 0))
                  (id (match-string-no-properties 1)))
              (when (member id ids)
                (make-button beg end 'type 'zk-link))))))))

  (advice-add 'zk-make-link-buttons :override #'zk-make-org-link-buttons)

  ;; get zk-id from org-link button label

  (button-type-put
   'zk-link
   'help-echo
   (lambda (_win _obj pos)
     (let ((label
            (button-label
             (button-at pos))))
       (string-match zk-id-regexp label)
       (format "%s" (zk--parse-id
                     'title (match-string 1 label)))))))

;;;; get zk-id from org-links

(defun zk-embark-target-zk-org-link-at-point ()
  "target zk-id at point."
  (pcase (org-in-regexp (zk-link-regexp))
    (`(,start . ,end)
     (let ((zk-id (match-string-no-properties 1)))
       `(zk-id ,zk-id ,start . ,end)))))

(advice-add #'zk-embark-target-zk-id-at-point :override #'zk-embark-target-zk-org-link-at-point)




;;; zk-index

(use-package zk-index
  :ensure nil
  :after zk
  :demand t
  :bind
  (:map gr-map
        ("." . zk-index-switch-to-index))
  (:map zk-index-mode-map
        ("C-<up>" . nil) ;; unbind move-text
        ("C-<down>" . nil) ;; unbind move-text
        ("N" . zk-new-note)
        ("n". zk-index-next-line)
        ("p" . zk-index-previous-line)
        ("C-n" . next-line)
        ("C-p" . previous-line)
        ("G" . gr/zk-index-luhmann-goto)
        ("/" . zk-index-focus)
        ("RET" . zk-index-view-note)
        ("v" . zk-index-view-note)
        ("P" . link-hint-preview-button)
        ("r" . gr/zk-index-title-echo)
        ("j" . consult-line) ;; "jump"
        ("?" . hydra-zk-index/body)
        ("R" . rotate-frame-clockwise)
        ([wheel-right] . (lambda () (interactive) (scroll-left 2)))
        ([wheel-left] . (lambda () (interactive) (scroll-right 2))))
  (:map zk-index-view-mode-map
        ("RET" . zk-index-view-mode))
  :hook
  (zk-index-mode-hook . (lambda () (setq-local line-spacing 1)))
  :custom
  (zk-index-view-debounce-delay 0)
  (zk-index-format "%i%t")
  (zk-index-invisible-ids t)
  (zk-index-button-display-function #'gr/zk-index-display-action)
  (zk-index-cursor 'bar)
  (zk-index-help-echo-function nil)
  (zk-index-prefix " ")
  (zk-index-view-mode-lighter (list
                               " "
                               (propertize "ZK-View"
                                           'face
                                           '( :background "yellow"
                                              :foreground "black"))))
  :config
  (zk-index-setup-embark)

  (defun gr/zk-index-display-action (file buffer)
    "Function to display FILE or BUFFER on button press in ZK-Index."
    (if (one-window-p)
        (if (zk-index--wide-window-p zk-index-buffer-name nil)
            (pop-to-buffer buffer
                           (display-buffer-in-direction
                            buffer
                            '((direction . right)
                              (window-width . 0.5))))
          (pop-to-buffer buffer
                         (display-buffer-in-direction
                          buffer
                          '((direction . up)
                            (window-height . 0.6)))))
      (if (window-in-direction 'above)
          (windmove-up)
        (windmove-right))
      (find-file file)))

  (defun gr/zk-index-title-echo ()
    (interactive)
    (message "%s"
             (zk--parse-id 'title (zk-index--button-at-point-p))))
  )

;;; zk-luhmann

(use-package zk-luhmann
  :ensure nil
  :after zk-index
  :demand t
  ;; :commands zk-luhmann-index
  :bind
  (:map zk-index-mode-map
        ("l" . gr/zk-luhmann-index-top)
        ("C-f" . zk-luhmann-index-forward)
        ("C-b" . zk-luhmann-index-back)
        ("f" . zk-luhmann-index-forward)
        ("b" . zk-luhmann-index-back)
        ("t" . zk-luhmann-index-top))
  :custom
  (zk-luhmann-id-prefix "{")
  (zk-luhmann-id-postfix "}")
  (zk-luhmann-indent-index t)
  (zk-luhmann-link-formatting t)
  (zk-luhmann-link-format "[[%i][%l]]")
  (zk-luhmann-link-and-title-format "%t: [[%i][%l]]")
  (zk-luhmann-count-format " [%s]")
  :config
  (add-hook 'completion-at-point-functions
            'zk-luhmann-completion-at-point)

  (defun gr/zk-luhmann-index-top ()
    (interactive)
    (zk-luhmann-index)
    (outline-cycle-buffer 1)
    (beginning-of-buffer))

  (defun gr/zk-index-luhmann-goto ()
    (interactive)
    (let ((arg (zk-index--button-at-point-p)))
      (zk-luhmann-index-goto arg)))

  (defun gr/zk-luhmann-index-goto ()
    "Designed to be called from a zk note itself."
    (interactive)
    (if-let* ((id (when (zk-file-p)
                    (zk--current-id))))
        (zk-luhmann-index-goto id)
      (call-interactively #'zk-luhmann-index-goto)))

  (defun zk-luhmann-outline-level ()
    (1+ (/ (length (match-string 1)) 2)))

  (defun zk-luhmann-setup-outline ()
    "Add to zk-index-mode-hook to setup outline-mode."
    (outline-minor-mode 1)
    (setq-local outline-regexp
                "{\\|\\(?1:[[:space:]]*\\).*?{")
    (setq-local outline-level #'zk-luhmann-outline-level)
    (define-advice forward-button (:around (oldfun n &rest extra) cond-for-zk-outline)
      "Move to next visible heading in `zk-index’ buffer."
      (if (and (derived-mode-p 'zk-index-mode)
               (eq zk-index-last-sort-function 'zk-luhmann-sort)
               outline-minor-mode)
          (outline-next-visible-heading n)
        (apply oldfun n extra))))

  (add-hook 'zk-index-mode-hook #'zk-luhmann-setup-outline)

  )

;;; zk-desktop

(use-package zk-desktop
  :ensure nil
  :disabled
  :commands zk-desktop
  :bind
  (:map zk-desktop-button-map
        ("v" . link-hint-preview-button))
  ;; ("o" . zk-index-aw-select))
  :config
  (zk-desktop-setup-embark)
  :hook
  (zk-desktop-mode-hook . (lambda () (variable-pitch-mode -1)))
  (zk-desktop-mode-hook . cursor-face-highlight-mode)
  (zk-desktop-mode-hook . (lambda () (setq-local cursor-face-highlight-nonselected-window t)))
  :custom
  (zk-desktop-prefix "- ")
  (zk-desktop-major-mode 'org-mode)
  (zk-desktop-add-pos 'at-point)
  (zk-desktop-directory "~/Documents/ZK/ZK-Desktops")
  :custom-face
  (zk-desktop-button ((t (:background "gray85" :height .9)))))

;;; zk-extras

(use-package zk-extras
  :ensure nil
  :after zk zk-luhmann zk-consult
  :demand t
  :commands zk-index-aw-select zk-daily-note
  :bind
  (:map gr-map
        ("C-d" . zk-index-daily-notes)
        ("d" . zk-daily-note)
        ("O" . link-hint-other-tab))
  (:map zk-index-mode-map
        ("o" . zk-index-aw-select)
        ("L" . zk-lit-notes-index)))

(use-package zk-consult
  :ensure nil
  :after zk
  :defer 1
  :commands zk-consult-select-file
  :custom
  (zk-tag-search-function #'zk-consult-grep-tag-search) ;; #'zk-grep #'zk-xref
  ;; this is overridden by something...
  (zk-consult-preview-functions
   '(zk-current-notes
     zk-consult-grep
     zk-docsim
     zk-consult-grep-tag-search
     zk-unlinked-notes))

  (zk-select-file-function #'zk-consult-select-file)

  :config
  (add-to-list 'consult-buffer-sources 'zk-consult-source)

  (consult-customize
   zk-tag-search
   zk-consult-grep
   zk-consult-grep-tag-search
   zk-docsim
   :preview-key 'any)

  (consult-customize
   zk-find-file
   zk-find-file-by-full-text-search
   zk-network zk-backlinks zk-links-in-note
   :preview-key "M-\\")
  )

(use-package zk-citar
  :ensure nil
  :after zk
  :defer 1
  :config
  (setq citar-notes-source 'zk)
  :custom
  (zk-citar-title-template "${=key=} - ${title} (${year})")
  (zk-citar-citekey-regexp "[a-z]+[0-9]\\{4\\}[a-z]?"))

(use-package zk-link-hint
  :ensure nil
  :after zk
  :defer 1
  :bind
  (:map zk-file-map
        ("p" . zk-preview))
  (:map zk-id-map
        ("p" . zk-preview))
  :config
  (require 'link-hint-preview)
  ;; (add-to-list 'avy-ignored-modes 'zk-index-mode)

  (defun avy-action-zk-preview (pt)
    (goto-char pt)
    (zk-preview))

  (require 'link-hint-aw-select)
  (add-to-list 'link-hint-aw-select-dispatch-alist '(?p .   avy-action-zk-preview))

  (add-to-list 'link-hint-aw-select-dispatch-alist '(?, .   avy-action-embark))

  )

(with-eval-after-load "embark"
  (defvar-keymap embark-become-zk-file-map
    :doc "Keymap for Embark zk-file actions."
    :parent embark-meta-map
    "f" #'zk-find-file
    "g" #'consult-grep
    "s" #'zk-find-file-by-full-text-search))


;;; next/previous zk buffer

(defun zk--switch-buffer (fn)
  "Switch to the next or previous zk buffer using MOVE-FN to navigate."
  (let ((start (current-buffer)))
    (funcall fn)
    (while (and (not (zk-buffer-p (current-buffer) nil))
                (not (eq (current-buffer) start)))
      (funcall fn))
    (unless (zk-buffer-p (current-buffer) nil)
      (switch-to-buffer start)
      (message "No more zk buffers."))))

(defun zk-next-buffer ()
  "Switch to the next zk buffer."
  (interactive)
  (zk--switch-buffer #'bury-buffer))

(defun zk-previous-buffer ()
  "Switch to the previous zk buffer."
  (interactive)
  (zk--switch-buffer #'previous-buffer))


;;; zk keymaps

(defvar-keymap zk-keymap-h
  :prefix t
  :doc "Keymap for 'h' prefix in zk-keymap."
  "h" (cons "daily note"
            (lambda () (interactive)
              (zk-find-file-by-id "201801190001")))
  "i" (cons "index"
            (lambda () (interactive)
              (zk-find-file-by-id "201801180001")))
  "s" (cons "structure"
            (lambda () (interactive)
              (zk-find-file-by-id "201801180002")))
  "c" (cons "clogs"
            (lambda () (interactive)
              (zk-find-file-by-id "202409221010")))
  "d" #'zk-daily-note)

(defvar-keymap zk-keymap
  :doc "Main zk keymap."
  ;; :prefix t
  ;; Prefix "B"
  "B" #'hydra-bib/body
  ;; Prefix "h"
  "h" #'zk-keymap-h
  ;; Direct bindings
  "d" #'zk-daily-note
  "N" #'zk-new-note
  "n" #'zk-capture
  "r" #'zk-rename-note
  "e" #'ebib-open
  "'" #'zk-index
  "C-'" #'zk-index
  "I" #'zk-index
  "i" #'gr/zk-insert-org-link
  ;; "l" (cons "luhmann index"
  ;;           (lambda ()
  ;;             (interactive)
  ;;             (zk-index)
  ;;             (zk-luhmann-index-top)))
  "<left>" #'zk-previous-buffer
  "<right>" #'zk-next-buffer
  "k" #'zk-copy-with-backlink
  "C-k" #'zk-copy-with-backlink
  "G" #'gr/zk-luhmann-index-goto
  "L" #'zk-lit-notes-index
  "m" #'zk-make-link-buttons
  "o" #'link-hint-aw-select
  "C-b" #'zk-index-backlinks
  "b" #'zk-backlinks
  "C-c" #'zk-index-current-notes
  "c" #'zk-current-notes
  ;; "S" #'zk-desktop-select
  "f" #'zk-find-file
  "F" #'zk-find-file-by-full-text-search
  "t" #'zk-consult-grep-tag-search
  "z" #'zk-consult-grep
  "g" #'zk-grep
  "x" #'zk-xref
  "s" #'zk-search
  "P" #'gr/database-ripgrep-all
  "p" #'gr/database-find-file
  "W" #'toggle-zk-note-breaks-mode)

(dolist (cmd '(zk-previous-buffer zk-next-buffer))
  (put cmd 'repeat-map 'zk-keymap))

(define-key global-map (kbd "C-z") zk-keymap)



;;; zk hydras

(eval-and-compile
  (defhydra hydra-bib (:hint nil
                             :color blue)
    "
       _r_: Insert Ref          _e_: ebib-hydra        _d_: DOI Lookup
       _b_: Insert Bib          _I_: Auto Import       _i_: ISBN Look up"

    ("b" gr/append-bibliography)
    ("r" citar-insert-reference)
    ("e" hydra-ebib/body)
    ("I" ebib-import-from-doi-or-isbn)
    ("i" ebib-isbn-web-search)
    ("d" crossref-lookup)
    ("c" gr/citar-insert-citation)
    ("q" nil)))

(eval-and-compile
  (defhydra hydra-zk-index (:hint nil)
    "
_S_: Size          List:
_M_: Modified      _l_: Luhmann  _L_: lit   _n_: non-L
_C_: Created       _a_: all      _c_: core  _e_: ED"
    ("a" zk-index-refresh :color blue)
    ("l" zk-luhmann-index-top :color blue)
    ("c" zk-core-index :color blue)
    ("n" zk-non-luhmann-index :color blue)
    ("L" zk-lit-notes-index :color blue)
    ("e" zk-ed-index :color blue)
    ("M" zk-index-sort-modified)
    ("C" zk-index-sort-created)
    ("S" zk-index-sort-size)
    ("q" nil)))


;; (eval-and-compile
;;   (defhydra hydra-zk (:hint nil
;;                             :idle .6
;;                             :pre (require 'zk-extras)
;;                             :color blue)
;;     "
;;   _h h_: Inbox      _i_: Insert Link   _N_: New Note       _k_: copy w/ link
;;   _h s_: Strct Nts                   _r_: Rename Note    _z_: zk grep
;;   _h i_: Index      _f_: Find File     _o_: Open Link      _e_: ebib-open
;;   _h c_: Clogs      _b_: Backlinks     _c_: Current Notes  _B_: Biblio.biz"
;;     ;;  [Luhmann: %(zk-luhmann-notes-count) | Lit: %(zk-lit-notes-count)]"
;;     ("h h" (zk-find-file-by-id "201801190001"))
;;     ("h i" (zk-find-file-by-id "201801180001"))
;;     ("h s" (zk-find-file-by-id "201801180002"))
;;     ("h c" (zk-find-file-by-id "202409221010"))
;;     ("h d" zk-daily-note)
;;     ("d" zk-daily-note)
;;     ("N" zk-new-note)
;;     ("n" zk-capture)
;;     ("r" zk-rename-note)
;;     ("i" zk-insert-link)
;;     ("e" ebib-open)
;;     ("B" hydra-bib/body)
;;     ("'" zk-index)
;;     ("C-'" zk-index)
;;     ("I" zk-index)
;;     ("l" (progn (zk-index) (zk-luhmann-index-top)))
;;     ("k" zk-copy-with-backlink)
;;     ("C-k" zk-copy-with-backlink)
;;     ("G" gr/zk-luhmann-index-goto)
;;     ("L" zk-lit-notes-index)
;;     ("c" zk-index-current-notes)
;;     ("m" zk-make-link-buttons)
;;     ("o" link-hint-aw-select)
;;     ("b" zk-backlinks)
;;     ("<left>" zk-previous-buffer :repeat t)
;;     ("C-b" zk-index-backlinks)
;;     ("C-c" zk-current-notes)
;;     ("S" zk-desktop-select)
;;     ("f" zk-find-file)
;;     ("F" zk-find-file-by-full-text-search)
;;     ("t" zk-consult-grep-tag-search)
;;     ("z" zk-consult-grep)
;;     ("g" zk-grep)
;;     ("x" zk-xref)
;;     ("s" zk-search)
;;     ;; ("d" zk-desktop-send-to-desktop)
;;     ;; ("D" zk-desktop)
;;     ("P" gr/database-ripgrep-all)
;;     ("p" gr/database-find-file)
;;     ("W" toggle-zk-note-breaks-mode)
;;     ("q" nil)))


(provide 'zk-setup)
;;; zk-setup.el ends here
