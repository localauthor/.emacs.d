(require 'transient)

(defvar transient-zk-index--auto-scroll nil
  "Transient toggle state for `zk-index-auto-scroll`.")

(defvar transient-zk-index--invisible-ids nil
  "Transient toggle state for `zk-index-invisible-ids`.")

(defun transient-zk-index-toggle-auto-scroll ()
  "Toggle `zk-index-auto-scroll` and refresh index if visible."
  (interactive)
  (setq zk-index-auto-scroll (not zk-index-auto-scroll))
  (setq transient-zk-index--auto-scroll zk-index-auto-scroll)
  (when (get-buffer zk-index-buffer-name)
    (with-current-buffer zk-index-buffer-name
      (zk-index-refresh)))
  (message "zk-index-auto-scroll is now %s" (if zk-index-auto-scroll "ON" "OFF")))

(defun transient-zk-index-toggle-invisible-ids ()
  "Toggle `zk-index-invisible-ids` and refresh index if visible."
  (interactive)
  (setq zk-index-invisible-ids (not zk-index-invisible-ids))
  (setq transient-zk-index--invisible-ids zk-index-invisible-ids)
  (when (get-buffer zk-index-buffer-name)
    (with-current-buffer zk-index-buffer-name
      (zk-index-refresh)))
  (message "zk-index-invisible-ids is now %s" (if zk-index-invisible-ids "ON" "OFF")))

(defun transient-zk-index-rename ()
  "Prompt to rename the current `zk-index` buffer."
  (interactive)
  (if (derived-mode-p 'zk-index-mode)
      (call-interactively 'zk-index-rename)
    (message "Not in a zk-index buffer")))

(defun transient-zk-index--auto-scroll-indicator ()
  "Return display string for `zk-index-auto-scroll` toggle."
  (if zk-index-auto-scroll "ON" "off"))

(defun transient-zk-index--invisible-ids-indicator ()
  "Return display string for `zk-index-invisible-ids` toggle."
  (if zk-index-invisible-ids "ON" "off"))

(transient-define-prefix transient-zk-index ()
  "Transient commands for zk-index."
  [["ZK-Index: Main Actions"
    ("o" "Open index" zk-index)
    ("r" "Refresh" zk-index-refresh :transient t)
    ("R" "Rename current index" transient-zk-index-rename)
    ("q" "Quit" transient-quit-one)]
   ["Navigation & Search"
    ("n" "Next line" zk-index-next-line :transient t)
    ("p" "Previous line" zk-index-previous-line :transient t)
    ("v" "View note at point" zk-index-view-note :transient t)
    ("s" "Search notes" zk-index-search :transient t)
    ("f" "Focus by title" zk-index-focus :transient t)
    ("c" "List open notes" zk-index-current-notes :transient t)]
   ["Sorting"
    ("m" "Sort by modified time" zk-index-sort-modified :transient t)
    ("C" "Sort by creation time" zk-index-sort-created :transient t)
    ("S" "Sort by size" zk-index-sort-size :transient t)]
   ["Filter by Modification Date"
    ("t" "Modified today" zk-index-modified-today :transient t)
    ("y" "Modified yesterday" zk-index-modified-yesterday :transient t)
    ("w" "Modified this week" zk-index-modified-this-week :transient t)]])
