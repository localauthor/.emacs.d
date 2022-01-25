;;; text-to-speech.el --- Text To Speech / Mac Speak functions    -*- lexical-binding: t; -*-

(require 'hydra)

(defvar words-voice "Ava"
  "Mac voice to use for speaking.")

(defvar words-rate "250")

(defun words-set-rate (rate)
  "Set WPM rate for mac-speak function."
  (interactive "nWPM Rate: ")
  (setq words-rate rate))

(defun words-speak (&optional text)
  "Speak word at point or region. Mac only."
  (interactive)
  (unless text
    (setq text (if (use-region-p)
                   (buffer-substring
                    (region-beginning) (region-end))
                 (thing-at-point 'word))))
  ;; escape some special applescript chars
  (setq text (replace-regexp-in-string "\\\\" "\\\\\\\\" text))
  (setq text (replace-regexp-in-string "\"" "\\\\\"" text))
  (do-applescript
   (format
    "say \"%s\" using \"%s\" speaking rate %s"
    text
    words-voice
    words-rate)))

(defun mac-say-word (&optional arg)
  "Speak word at point. With ARG, go forward ARG words."
  (interactive "P")
  ;; arg can be (4), 4, "-", or -1. we handle these like this.
  (let ((newarg))
    (when arg
      (setq newarg (cond
                    ((listp arg)
                     (round (log (car arg) 4)))
                    ((and (stringp arg) (string= "-" arg))
                     ((< 0 arg) arg)
                     -1)
                    (t arg)))
      (forward-word newarg))
    (when (thing-at-point 'word)
      (words-speak (thing-at-point 'word)))))

(defun mac-say-sentence (&optional arg)
  "Speak sentence at point. With ARG, go forward ARG sentences."
  (interactive "P")
  ;; arg can be (4), 4, "-", or -1. we handle these like this.
  (let ((newarg))
    (when arg
      (setq newarg (cond
                    ((listp arg)
                     (round (log (car arg) 4)))
                    ((and (stringp arg) (string= "-" arg))
                     ((< 0 arg) arg)
                     -1)
                    (t arg)))
      (forward-sentence newarg)
      (when (< 0 newarg) (forward-word)))
    (when (thing-at-point 'sentence)
      (words-speak (thing-at-point 'sentence)))))

(defun mac-say-paragraph (&optional arg)
  "Speak paragraph at point. With ARG, go forward ARG paragraphs."
  (interactive "P")
  ;; arg can be (4), 4, "-", or -1. we handle these like this.
  (let ((newarg))
    (when arg
      (setq newarg (cond
                    ((listp arg)
                     (round (log (car arg) 4)))
                    ((and (stringp arg) (string= "-" arg))
                     ((< 0 arg) arg)
                     -1)
                    (t arg)))
      (forward-paragraph newarg)
      (when (< 0 newarg) (forward-word)))
    (when (thing-at-point 'paragraph)
      (words-speak (thing-at-point 'paragraph)))))

(defhydra hydra-mac-speak (:hint nil)
  "
 Mac-Speak                       Current WPM: %`words-rate
     _s_: next sent.   _p_: next para.   _l_: selection
     _S_: prev sent.   _P_: prev para.   _r_: wpm rate
    "
  ("q" nil)
  ("l" (words-speak))
  ("r" (call-interactively #'words-set-rate))
  ("W" (progn (mac-say-word) (forward-word)))
  ("w" (mac-say-word -1))
  ("s" (progn (mac-say-sentence) (forward-sentence)(forward-word)) :exit nil)
  ("S" (mac-say-sentence -1))
  ("p" (progn (mac-say-paragraph) (forward-paragraph)))
  ("P" (mac-say-paragraph -1)))

(define-prefix-command 'mac-speak-keymap)
(define-key mac-speak-keymap (vector ?w) 'mac-say-word)
(define-key mac-speak-keymap (vector ?s) 'mac-say-sentence)
(define-key mac-speak-keymap (vector ?p) 'mac-say-paragraph)
(define-key mac-speak-keymap (vector ?h) 'hydra-mac-speak/body)
(global-set-key (kbd "\C-cr") 'mac-speak-keymap)

(provide 'text-to-speech)
