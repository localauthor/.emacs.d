;; Experimental citar/link-hint integration

;;; org-cite

(defun link-hint--next-org-cite (bound)
  (link-hint--next-regexp org-element-citation-prefix-re bound))

(link-hint-define-type 'org-cite
  :next #'link-hint--next-org-cite
  :at-point-p #'citar-org--citation-at-point
  :open #'org-open-at-point
  :copy #'kill-new ;; error, or doesn't do anything useful
  :aw-select #'org-open-at-point) ;; not general

(push 'link-hint-org-cite link-hint-types)


;; what should copy do?
;; maybe copy references, via citar-copy-reference?

;;; latex

;; what should open do?

(defun link-hint--next-latex-cite (bound)
  (link-hint--next-regexp org-element-citation-prefix-re bound))

(link-hint-define-type 'latex-cite
  :next #'link-hint--next-latex-cite
  :at-point-p #'citar-latex-citation-at-point
  :open #'org-open-at-point
  :copy #'kill-new ;; error, or doesn't do anything useful
  :aw-select #'org-open-at-point)

(push 'link-hint-org-cite link-hint-types)
