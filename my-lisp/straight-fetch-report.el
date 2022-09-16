;;; straight-fetch-report.el                    -*- lexical-binding: t; -*-

(defun straight-fetch-report (&rest _)
  "Show fetched commit summary."
  (interactive)
  (with-current-buffer (get-buffer-create "*straight-fetch-report*")
    (read-only-mode -1)
    (erase-buffer)
    (let ((updates nil)
          (error-repos nil))
      (straight--map-repos
       (lambda (recipe)
         (straight--with-plist recipe (package local-repo)
           (let* ((default-directory (straight--repos-dir local-repo))
                  (commits))
             (condition-case nil
                 (setq commits (straight--process-output "git" "log" "..@{u}" "--oneline"))
               (error (push local-repo error-repos)))
             (unless (or (string-empty-p commits)
                         (not commits))
               (push (flatten-list (list package local-repo (split-string commits "\n"))) updates))))))
      (if updates
          (progn
            (insert (propertize "Recently Fetched Commits" 'face 'outline-1)
                    "\n\n")
            (mapc (lambda (update)
                    (let* ((commits (cddr update))
                           (package (car update))
                           (dir (straight--repos-dir (nth 1 update))))
                      (insert
                       (propertize
                        (format "%s [%s commit%s]\n"
                                package
                                (number-to-string (length commits))
                                (if (cdr commits) "s" ""))
                        'face 'font-lock-constant-face))
                      (mapc (lambda (commit)
                              (let ((rev (string-limit commit 7)))
                                (insert-text-button
                                 (concat "  "
                                         commit)
                                 'follow-link t
                                 'face 'default
                                 'action (lambda (_)
                                           (let ((default-directory dir))
                                             (magit-show-commit (magit-commit-p rev)))))
                                (newline)))
                            (cddr update)))
                    (newline))
                  (cl-sort updates #'string< :key #'car)))
        (insert (propertize "No Recently Fetched Commits" 'face 'outline-1)))
      (when error-repos
        (insert (propertize "Repos With No Upstream Branch" 'face 'outline-1) "\n")
        (mapc (lambda (repo)
                (insert "Click to set upstream: ")
                (insert-text-button
                 (file-name-base repo)
                 'follow-link t
                 'face 'default
                 'action (lambda (_)
                           (let ((default-directory repo)
                                 (branch (magit-get-current-branch)))
                             (magit-status repo)
                             (magit-branch-configure branch)))))
              error-repos))
      (special-mode)
      (pop-to-buffer (current-buffer))
      (goto-char (point-min)))))

(advice-add #'straight-fetch-all :after #'straight-fetch-report)

(provide 'straight-fetch-report)
;;; straight-fetch-report ends here
