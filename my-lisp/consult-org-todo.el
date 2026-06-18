;;; consult-org-todo.el --- Consult interface for org-todo headings  -*- lexical-binding: t; -*-

;;; Code:

(require 'consult-org)

(defun consult-org-todo (match scope)
  "Jump to an Org TODO heading.

MATCH and SCOPE are as in `org-map-entries' and determine which
entries are offered.  By default, all entries of the current
buffer are offered."
  (interactive)
  (consult--read
   (consult--slow-operation "Collecting headings..."
     (or (consult-org-todo--headings match scope)
         (user-error "No TODO headings")))
   :prompt (format "Tasks [%s]: " match)
   :category 'org-heading
   :sort nil
   :require-match t
   :history '(:input consult-org--history)
   :narrow (consult-org--narrow)
   :state (consult--jump-state)
   ;;:annotate #'consult-org--annotate
   :group #'consult-org-todo--group
   :lookup (apply-partially #'consult--lookup-prop 'org-marker)))

(defun consult-org-todo--headings (match scope &rest skip)
  "Return a list of Org heading candidates."
  (let ((idx 0))
    (remq nil
          (apply
           #'org-map-entries
           (lambda ()
             (pcase-let* ((`(_ ,level ,todo ,prio ,hl ,_tags) (org-heading-components))
                          (cand hl))
               (when todo
                 (put-text-property 0 (length todo) 'face (org-get-todo-face todo) todo)
                 (setq cand (concat cand (consult--tofu-encode idx)))
                 (cl-incf idx)
                 (add-text-properties 0 1
                                      `(org-marker ,(point-marker)
                                                   consult-org--heading (,level ,todo ,prio . _))
                                      cand)
                 cand)))
           match scope skip))))

(defun consult-org-todo--group (cand transform)
"Return title for CAND or TRANSFORM the candidate."
(pcase-let ((`(,_level ,todo ,_prio . ,buffer)
             (get-text-property 0 'consult-org--heading cand)))
  (if transform cand todo)))

(provide 'consult-org-todo)
;;; consult-org-todo.el ends here
