;;;###autoload
(defun my/move-start-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;;###autoload
(defun my/open-line-below ()
  "Open line below"
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

;;;###autoload
(defun my/open-line-above ()
  "Open line above"
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-for-tab-command))

;;;###autoload
(defun my/mark-forward-line (count)
  "mark to the next `count' lines"
  (interactive "p")
  (set-mark (point))
  (next-line count))

;;;###autoload
(defun my/mark-backward-line (count)
  "mark to the previous `count' lines from current position"
  (interactive "p")
  (set-mark (point))
  (previous-line count))

;;;###autoload
(defun my/mark-forward-char (count)
  "mark to the next `count' chars from current position"
  (interactive "p")
  (set-mark (point))
  (forward-char count))

;;;###autoload
(defun my/mark-backward-char (count)
  "mark to the previous `count' chars"
  (interactive "p")
  (set-mark (point))
  (backward-char count))

;;;###autoload
(defun my/mark-forward-word (count)
  "mark to end of word from current position"
  (interactive "p")
  (set-mark (point))
  (forward-word count))

;;;###autoload
(defun my/mark-backward-word (count)
  "mark to the beginning of word from current position"
  (interactive "p")
  (set-mark (point))
  (backward-word count))

;;;###autoload
(defun my/mark-forward-paragraph (count)
  "mark to end of paragraph from current position"
  (interactive "p")
  (set-mark (point))
  (forward-paragraph count))

;;;###autoload
(defun my/mark-backward-paragraph (count)
  "mark to the beginning of paragraph from current position"
  (interactive "p")
  (set-mark (point))
  (backward-paragraph count))

(provide 'hge-ryo-functions)
