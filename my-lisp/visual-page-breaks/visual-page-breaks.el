;;; visual-page-breaks.el --- Visual page break overlays -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: May 20, 2025
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "27.2"))

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; visual-page-breaks.el inserts visual indicators for page breaks at regular
;; intervals in buffers. These overlays help users identify logical divisions
;; in large text files, such as chapters or sections, based on either a fixed
;; number of words or lines.

;; This package supports buffer-local and global activation through a minor
;; mode. Users can customize the display string and the interval for page
;; breaks, and use predicate-based settings to tailor page break display for
;; different buffer types or file content. The overlays do not modify buffer
;; text and are easily toggled on or off.

;; Typical uses include drafting, editing prose, or viewing long code files
;; where clear document structure is useful.

;;; Code:

(defgroup visual-page-breaks nil
  "Visual page break overlays in buffers."
  :group 'text
  :prefix "visual-page-breaks-")

(defcustom visual-page-breaks-alist nil
  "Alist defining buffer-specific visual page break settings.

Each element has the form (PREDICATE PARAMETERS STRING), where:

- PREDICATE is a function that, when called in a buffer, returns non-nil
  if the settings should apply to that buffer.
- PARAMETERS is a cons cell similar to `visual-page-breaks-parameters’,
  defining display properties for the page breaks.
- STRING is the text to be displayed at page breaks, similar to
  `visual-page-breaks-string’.

The first element whose PREDICATE returns non-nil when evaluated in the
current buffer determines the settings used for that buffer."
  :type '(repeat
          (list
           (function :tag "Predicate")
           (cons :tag "Parameters"
                 (choice (const words)
                         (const lines))
                 integer)
           (string :tag "Overlay String"))))

(defcustom visual-page-breaks-string
  "|---------------------------- END OF PAGE %p --------------------[%w]---|"
  "Default page break indicator."
  :type 'string :tag "Overlay String" :local t)

(defcustom visual-page-breaks-parameters '(words . 333)
  "Default parameters for page breaks.
Must be a cons cell where the car is the symbol `words’ or `lines’, and
the cdr is the number of words or lines after which the break should
occur."
  :type '(cons :tag "Parameters"
               (choice (const words)
                       (const lines))
               integer)
  :local t)

(defvar visual-page-breaks-overlays-all nil)

(defvar-local visual-page-breaks-overlays-local nil)
;; prevent local value from begin killed by change-major-mode-hook
(put 'visual-page-breaks-overlays-local 'permanent-local t)

(defvar-local visual-page-breaks-local nil)
(defvar visual-page-breaks-timer nil)

(defun visual-page-breaks-set-params ()
  "Set parameters according to `visual-page-breaks-alist’."
  (catch 'exit
    (dolist (item visual-page-breaks-alist)
      (pcase-let ((`(,cond ,param ,string) item))
        (when (eval cond)
          (setq visual-page-breaks-parameters param
                visual-page-breaks-string string)
          (throw 'exit t))))))

;;;###autoload
(define-minor-mode visual-page-breaks-mode
  "Insert page-break indicator overlays into buffer."
  :global t
  :lighter " VPB"
  (if visual-page-breaks-mode
      (progn
        (visual-page-breaks)
        (add-hook 'find-file-hook #'visual-page-breaks)
        (add-hook 'kill-buffer-hook #'visual-page-breaks-delete-local)
        (setq visual-page-breaks-timer
              (run-with-idle-timer
               2 t #'visual-page-breaks)))
    (when visual-page-breaks-timer
      (cancel-timer visual-page-breaks-timer))
    (remove-hook 'find-file-hook #'visual-page-breaks)
    (remove-hook 'kill-buffer-hook #'visual-page-breaks-delete-local)
    (visual-page-breaks-delete-all)))

;;;###autoload
(defun visual-page-breaks ()
  "Create visual page-breaks.
Set parameters with `visual-page-breaks-parameters’."
  (interactive)
  (save-excursion
    (unless (eq visual-page-breaks-local 'local-off)
      (when (or (visual-page-breaks-set-params)
                visual-page-breaks-local)
        (visual-page-breaks-delete-local)
        (let* ((page 0)
               (param visual-page-breaks-parameters)
               (string (concat "\n\n"
                               visual-page-breaks-string
                               "\n\n"))
               (is-org-mode (derived-mode-p 'org-mode)))
          (goto-char (point-min))
          (when is-org-mode
            (while (org-at-keyword-p)
              (forward-line)))
          (while (not (eobp))
            (let ((beg (point))
                  (end (progn
                         (if (eq 'words (car param))
                             (forward-word (cdr param))
                           (vertical-motion (cdr param)))
                         (when (and is-org-mode
                                    (org-at-heading-or-item-p))
                           (forward-line))
                         (unless (looking-at-p "$")
                           (forward-paragraph))
                         (point)))
                  (ov (make-overlay (line-beginning-position)
                                    (line-end-position))))
              (setq page (1+ page))
              (overlay-put ov
                           'after-string
                           (format-spec
                            string
                            `((?p . ,page) (?w . ,(count-words beg end)))))
              (push ov visual-page-breaks-overlays-local)
              (unless visual-page-breaks-local
                (push ov visual-page-breaks-overlays-all))))
          (when (called-interactively-p 'any)
            (message "%s pages" page)))))))

;;;###autoload
(defun visual-page-breaks-delete-all ()
  "Delete page-break overlays in current buffer."
  (interactive)
  (dolist (ov visual-page-breaks-overlays-all)
    (delete-overlay ov))
  (setq visual-page-breaks-overlays-all nil))

;;;###autoload
(defun visual-page-breaks-delete-local ()
  "Delete page-break overlays in current buffer."
  (interactive)
  (dolist (ov visual-page-breaks-overlays-local)
    (delete-overlay ov))
  (setq visual-page-breaks-overlays-local nil))

;;;###autoload
(defun toggle-visual-page-breaks-local ()
  "Toggle `visual-page-breaks` in the current buffer."
  (interactive)
  (cond
   ;; If explicitly off locally, enable it
   ((eq visual-page-breaks-local 'local-off)
    (setq visual-page-breaks-local t)
    (visual-page-breaks)
    (message "Visual page breaks enabled locally"))
   ;; If enabled locally, disable it
   ((and visual-page-breaks-local
         (not (eq visual-page-breaks-local 'local-off)))
    (setq visual-page-breaks-local 'local-off)
    (visual-page-breaks-delete-local)
    (message "Visual page breaks disabled locally"))
   ;; visual-page-breaks-local is nil
   (t
    (if (and (visual-page-breaks-set-params)
             visual-page-breaks-mode)
        ;; If mode is active but no local setting, disable locally
        (progn
          (setq visual-page-breaks-local 'local-off)
          (visual-page-breaks-delete-local)
          (message "Visual page breaks disabled locally"))
      ;; Otherwise enable locally
      (setq visual-page-breaks-local t)
      (visual-page-breaks)
      (message "Visual page breaks enabled locally")))))

(provide 'visual-page-breaks)

;;; visual-page-breaks.el ends here
