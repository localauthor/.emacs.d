;;; link-hint-embark-act --- Add embark-act action to link-hint -*- lexical-binding: t; -*-

;;; Commentary:

;; Adds 'embark-act' action to link-hint for all link types

;;; Code:

(require 'link-hint)
(require 'embark)

;;;###autoload
(defun link-hint-embark-act ()
  "Use avy to call 'embark-act' on a link."
  (interactive)
  (avy-with link-hint-embark
    (link-hint--one :embark-act)))

(mapc
 (lambda (x)
   (let ((fn (string-trim-left (symbol-name x) "link-hint-")))
     (link-hint-define-type `,(intern fn)
       :embark-act #'(lambda ()
                       (call-interactively
                        #'embark-act)))))
 link-hint-types)

(provide 'link-hint-embark-act)

;;; link-hint-embark-act.el ends here
