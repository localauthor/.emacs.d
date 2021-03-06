(defun poem-machine (text)
  (interactive "fText: ")
  (let ((corpus
         (with-temp-buffer
           (insert-file text)
           (split-string (buffer-string))))
        (lines (read-number "Lines: "))
        (words)
        (count1 0)
        (count2 0))
    (while (< count1 lines)
      (with-current-buffer (get-buffer-create "*Poem*")
        (setq words (random 10))
        (while (< count2 words)
          (goto-char (point-max))
          (insert " " (nth (random (length corpus)) corpus))
          (setq count2 (1+ count2)))
        (newline)
        (setq count2 0)
        (setq count1 (1+ count1))))
    (pop-to-buffer "*Poem*")))
