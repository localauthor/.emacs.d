;;; zk-setup.el --- Setup for zk, zk-index, zk-luhmann, etc.  -*- lexical-binding: t; -*-

;;;; zk

(use-package zk
  :straight (zk :type nil
                :local-repo "~/.dotfiles/.emacs.d/my-lisp/zk/"
                :files (:defaults "zk-consult.el"))
  :defer 1
  :bind
  (:map zk-id-map
        ("p" . zk-preview)
        ("s" . zk-search)
        ("r" . zk-consult-grep)
        ("o" . link-hint--aw-select-zk-link))
  :hook
  (completion-at-point-functions . zk-completion-at-point)
  (completion-at-point-functions . gr/mmd-citation-completion-at-point)
  :custom
  (zk-directory "~/Dropbox/ZK/Zettels")
  (zk-file-extension "md")
  (zk-default-backlink "201801190001")
  (zk-tag-regexp "\\s#[a-zA-Z0-9]\\+")
  (zk-link-and-title 'ask)
  (zk-new-note-link-insert 'ask)
  (zk-link-format "[[%s]]")
  (zk-link-and-title-format "%t [[%i]]")
  (zk-completion-at-point-format "%t [[%i]]")
  (zk-search-function #'zk-xref) ;; #'zk-consult-grep) ;; #'zk-grep ;; 
  (zk-tag-search-function #'zk-xref) ;; #'zk-consult-grep-tag-search) ;; #'zk-grep 
  (zk-current-notes-function nil)
  (zk-select-file-function 'zk-consult-select-file)
  (zk-consult-preview-functions
   '(zk-current-notes
     zk-unlinked-notes))
  :config
  (zk-setup-auto-link-buttons)
  (with-eval-after-load 'embark-org
    (zk-setup-embark))
  (add-to-list 'auto-mode-alist '("\\.md$" . org-mode))
  (with-eval-after-load 'embark
    (add-to-list 'embark-become-keymaps 'embark-become-zk-file-map))
  (with-eval-after-load 'consult
    (add-to-list 'consult-buffer-sources 'zk-consult-source 'append))
  (consult-customize
   zk-find-file zk-find-file-by-full-text-search zk-network zk-backlinks zk-links-in-note
      :preview-key (list (kbd "C-{"))))

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
  :after zk
  :straight (zk-index :local-repo "~/.dotfiles/.emacs.d/my-lisp/zk/"
                      :type nil
                      :files ("zk-index.el"))
  :bind
  (:map zk-index-mode-map
        ("o" . zk-index-aw-select)
        ("v" . link-hint-preview-button)
        ("P" . link-hint-preview-button))
        ("j" . consult-line) ;; "jump"
        ("?" . hydra-zk-index/body))
  (:map zk-index-desktop-button-map
        ("v" . link-hint-preview-button)
        ("o" . zk-index-aw-select))
  (:map zk-index-view-mode-map
        ("RET" . zk-index-view-mode))
  :config
  (zk-index-setup-embark)
  :hook
  (zk-index-desktop-mode-hook . variable-pitch-mode)
  (zk-index-desktop-mode-hook . cursor-face-highlight-mode)
  (zk-index-desktop-mode-hook . (lambda () (setq-local cursor-face-highlight-nonselected-window t)))
  :custom
  (zk-index-prefix nil)
  (zk-index-desktop-prefix "- ")
  (zk-index-desktop-major-mode 'outline-mode)
  (zk-index-desktop-add-pos 'at-point)
  (zk-index-desktop-directory "~/Dropbox/ZK/Desktops")
  :custom-face
  (zk-index-desktop-button ((t (:background "gray95" :height .9)))))

;;;; zk-luhmann

(use-package zk-luhmann
  :after zk-index
  :straight (zk-luhmann :local-repo "~/.dotfiles/.emacs.d/my-lisp/zk-luhmann")
  :bind (:map zk-index-mode-map
              ("L" . zk-luhmann-index-sort)
              ("l" . zk-luhmann-index)
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
  :defer 1)

(use-package zk-extras
  :straight nil
  :defer 1
  :config
  (setq zk-core-notes-count (length (zk-non-luhmann-list)))
  (setq zk-luhmann-notes-count (length (zk-luhmann-files))))

  (use-package zk-link-hint
  :straight (zk-link-hint :local-repo "~/.dotfiles/.emacs.d/my-lisp/zk")
  :defer 1)

(with-eval-after-load "embark"
  (embark-define-keymap embark-become-zk-file-map
    "Keymap for Embark zk-file actions."
    :parent embark-meta-map
    ("f" zk-find-file)
    ("g" consult-grep)
    ("s" zk-find-file-by-full-text-search)))

;;;; zk hydras

(eval-and-compile
  (defhydra hydra-zk (:hint nil
                            :color blue)
    "
  _h h_: Inbox      _i_: Insert Link   _N_: New Note       _d_: to desktop
  _h s_: Strct Nts  _c_: Insert Cite   _r_: Rename Note    _z_: zk grep
  _h i_: Index      _f_: Find File     _o_: Open Link      _e_: ebib-open
                  _b_: Backlinks     _C_: Current Notes  _B_: Biblio.biz
   [Luhmann: %`zk-luhmann-notes-count | Notes: %`zk-core-notes-count]"
    ("h h" (lambda () (interactive) (zk-find-file-by-id "201801190001")))
    ("h i" (lambda () (interactive) (zk-find-file-by-id "201801180001")))
    ("h s" (lambda () (interactive) (zk-find-file-by-id "201801180002")))
    ("N" zk-new-note)
    ("R" zk-rename-note)
    ("r" zk-rename-note)
    ("i" zk-luhmann-insert-link)
    ("e" ebib-open)
    ("B" hydra-bib/body)
    ;;("B b" gr/append-bibliography)
    ;;("B r" citar-insert-reference)
    ;;("B p" pullbib-pull)
    ("I" zk-index)
    ("l" zk-luhmann-index)
    ("G" zk-luhmann-index-go-to-current)
    ("L" zk-lit-notes)
    ("c" gr/citar-mmd-insert-citation)
    ("C" zk-current-notes)
    ("m" zk-make-link-buttons)
    ("o" link-hint-aw-select)
    ("b" zk-network)
    ("S" (lambda () (interactive) (zk-stats 1)) :color red)
    ("f" zk-find-file)
    ("F" zk-find-file-by-full-text-search)
    ("z" zk-consult-grep)
    ("g" zk-consult-grep)
    ("s" zk-search)
    ("d" zk-index-send-to-desktop)
    ("D" zk-index-desktop)
    ;; ("d" gr/consult-ripgrep-select-dir)
    ("p" devonthink-dir-find-file)
    ("q" nil)))

(bind-key* (kbd "C-z") 'hydra-zk/body)
(bind-key* (kbd "M-z") 'hydra-zk/body)

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
_M_: Modified      _l_: Luhmann  _L_: lit
_C_: Created       _a_: all      _c_: core "
    ("a" zk-index-refresh)
    ("l" zk-luhmann-index)
    ("c" zk-core-index)
    ("n" zk-non-luhmann-index)
    ("L" zk-lit-notes-index)
    ("e" zk-ed-index)
    ("M" zk-index-sort-modified)
    ("C" zk-index-sort-created)
    ("S" zk-index-sort-size)
    ("q" nil)))

(provide 'zk-setup)
;;; zk-setup.el ends here


