;;; zk-setup.el --- Setup for zk, zk-index, zk-luhmann, etc.  -*- lexical-binding: t; -*-

;;;; zk

(use-package zk
  :load-path "my-lisp/zk"
  :straight nil
  :defer 1
  :after consult
  :mode (("\\.md$" . org-mode))
  :bind
  (:map zk-file-map
        ("G" . zk-luhmann-index-goto))
  (:map zk-id-map
        ("s" . zk-search)
        ("z" . zk-grep) ;; zk-consult-grep does not work as embark action
        ("G" . zk-luhmann-index-goto)
        ("o" . link-hint--aw-select-zk-link))
  :hook
  (completion-at-point-functions . zk-completion-at-point)
  (completion-at-point-functions . gr/mmd-citation-completion-at-point)
  :custom
  (zk-directory "~/Dropbox/ZK/Zettels")
  (zk-file-extension "md")
  (zk-tag-regexp "\\s#[a-zA-Z0-9]\\+")
  (zk-new-note-header-function #'gr/zk-new-note-header)
  (zk-link-and-title 'ask)
  (zk-new-note-link-insert 'ask)
  (zk-link-format "[[%s]]")
  (zk-link-and-title-format "%t [[%i]]")
  (zk-completion-at-point-format "%t [[%i]]")
  (zk-search-function #'zk-xref) ;; #'zk-consult-grep) ;; #'zk-grep ;;
  (zk-tag-search-function #'zk-consult-grep-tag-search) ;; #'zk-grep #'zk-xref
  (zk-current-notes-function nil)
  (zk-consult-preview-functions
   '(zk-current-notes
     zk-unlinked-notes))
  :config
  (zk-setup-auto-link-buttons)
  (zk-setup-embark)
  (with-eval-after-load 'embark
    (add-to-list 'embark-become-keymaps 'embark-become-zk-file-map))
  (with-eval-after-load 'consult
    (add-to-list 'consult-buffer-sources 'zk-consult-source 'append))
  (consult-customize
   zk-find-file zk-find-file-by-full-text-search zk-network zk-backlinks zk-links-in-note
   :preview-key '("C-{")))

(defun gr/zk-new-note-header (title new-id &optional orig-id)
  "Insert header in new notes with args TITLE and NEW-ID.
Optionally use ORIG-ID for backlink."
  (insert (format "#+TITLE: %s %s\n===\n#+tags: \n" new-id title))
  (when (ignore-errors (zk--parse-id 'title orig-id)) ;; check for file
    (progn
      (insert "===\n<- ")
      (zk--insert-link-and-title orig-id (zk--parse-id 'title orig-id))
      (newline)))
  (insert "===\n\n\n"))

(defun zk-org-try-to-follow-link (fn &optional arg)
  "When 'org-open-at-point' FN fails, try 'zk-follow-link-at-point'.
Optional ARG."
  (let ((org-link-search-must-match-exact-headline t))
    (condition-case nil
	(apply fn arg)
      (error (unless (ignore-errors (zk-follow-link-at-point))
               (message "Invalid org-link type"))))))

(advice-add 'org-open-at-point :around #'zk-org-try-to-follow-link)

;;;; zk-index

(use-package zk-index
  :load-path "my-lisp/zk"
  :after zk
  :straight nil
  :bind
  (:map zk-index-mode-map
        ("o" . zk-index-aw-select)
        ("v" . zk-index-view-note)
        ("P" . link-hint-preview-button)
        ("j" . consult-line) ;; "jump"
        ("?" . hydra-zk-index/body))
  (:map zk-index-view-mode-map
        ("RET" . zk-index-view-mode))
  :hook
  (zk-index-mode-hook . (lambda () (setq-local line-spacing 1)))
  :config
  (zk-index-setup-embark)
  :custom
  (zk-index-prefix " "))

;;;; zk-desktop

(use-package zk-desktop
  :load-path "my-lisp/zk"
  :straight nil
  :commands zk-desktop
  :bind
  (:map zk-desktop-button-map
        ("v" . link-hint-preview-button)
        ("o" . zk-index-aw-select))
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
  (zk-desktop-directory "~/Dropbox/ZK/ZK-Desktops")
  :custom-face
  (zk-desktop-button ((t (:background "gray85" :height .9)))))

;;;; zk-luhmann

(use-package zk-luhmann
  :load-path "my-lisp/zk-luhmann"
  :after zk-index
  :straight nil
  :bind (:map zk-index-mode-map
              ("l" . zk-luhmann-index-top)
              ("C-f" . zk-luhmann-index-forward)
              ("C-b" . zk-luhmann-index-back)
              ("C-t" . zk-luhmann-index-unfold)
              ("t" . zk-luhmann-index-top))
  :hook (completion-at-point-functions . zk-luhmann-completion-at-point)
  :custom
  (zk-luhmann-id-prefix "{")
  (zk-luhmann-id-postfix "}")
  (zk-luhmann-indent-index t))

;;;; zk-extras

(use-package zk-consult
  :straight nil
  :load-path "my-lisp/zk"
  :after zk
  :defer 1
  :custom
  (zk-select-file-function 'zk-consult-select-file))

(use-package zk-citar
  :straight nil
  :load-path "my-lisp/zk"
  :after zk
  :defer 1
  :config
  (setq citar-notes-source 'zk)
  :custom
  (zk-citar-citekey-regexp "[a-z]+[0-9]\\{4\\}[a-z]?"))

(use-package zk-link-hint
  :straight nil
  :load-path "my-lisp/zk"
  :after zk
  :defer 1
  :bind
  (:map zk-file-map
        ("p" . zk-preview))
  (:map zk-id-map
        ("p" . link-hint-preview-zk-link))
  :config
  (require 'link-hint-preview))

(with-eval-after-load "embark"
  (defvar-keymap embark-become-zk-file-map
    :doc "Keymap for Embark zk-file actions."
    :parent embark-meta-map
    "f" #'zk-find-file
    "g" #'consult-grep
    "s" #'zk-find-file-by-full-text-search))

;;;; zk hydras

(use-package zk-extras
  :straight nil
  :load-path "my-lisp/zk"
  :after zk zk-luhmann
  :bind (:map zk-index-mode-map
              ("L" . zk-lit-notes-index)))

(eval-and-compile
  (defhydra hydra-zk (:hint nil
                            :pre (require 'zk-extras)
                            :color blue
                            :idle 1.0)
    "
    _h h_: Inbox      _i_: Insert Link   _N_: New Note       _d_: to desktop
    _h s_: Strct Nts  _c_: Insert Cite   _r_: Rename Note    _z_: zk grep
    _h i_: Index      _f_: Find File     _o_: Open Link      _e_: ebib-open
                    _b_: Backlinks     _C_: Current Notes  _B_: Biblio.biz"
    ;;  [Luhmann: %(zk-luhmann-notes-count) | Lit: %(zk-lit-notes-count)]"
    ("h h" (zk-find-file-by-id "201801190001"))
    ("h i" (zk-find-file-by-id "201801180001"))
    ("h s" (zk-find-file-by-id "201801180002"))
    ("N" zk-new-note)
    ("r" zk-rename-note)
    ("i" zk-luhmann-insert-link)
    ("e" ebib-open)
    ("B" hydra-bib/body)
    ("I" zk-index)
    ("l" (progn (zk-index) (zk-luhmann-index-top)))
    ("G" zk-luhmann-index-goto)
    ("L" zk-lit-notes-index)
    ("c" gr/citar-insert-citation)
    ("C" zk-current-notes)
    ("m" zk-make-link-buttons)
    ("o" link-hint-aw-select)
    ("b" zk-network)
    ("S" zk-desktop-select)
    ("f" zk-find-file)
    ("F" zk-find-file-by-full-text-search)
    ("t" zk-consult-grep-tag-search)
    ("z" zk-consult-grep)
    ("g" zk-grep)
    ("x" zk-xref)
    ("s" zk-search)
    ("d" zk-desktop-send-to-desktop)
    ("D" zk-desktop)
    ("p" devonthink-dir-find-file)
    ("q" nil)))

(eval-and-compile
  (defhydra hydra-bib (:hint nil
                             :color blue)
    "
       _r_: Insert Ref          _e_: ebib-hydra        _d_: DOI Lookup
       _b_: Insert Bib          _I_: Auto Import       _i_: ISBN Look up"

    ("b" gr/append-bibliography)
    ("r" citar-insert-reference)
    ("e" hydra-ebib/body)
    ("I" ebib-auto-import)
    ("i" ebib-isbn-web-search)
    ("d" crossref-lookup)
    ("c" gr/citar-mmd-insert-citation)
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

(provide 'zk-setup)
;;; zk-setup.el ends here



