(defvar my/terminal-position-below-p t)

(defun my/toggle-terminal ()
  (interactive)
  (let* ((buffer-name "*ansi-term*")
	(buffer      (get-buffer buffer-name))
	(win         (and buffer (get-buffer-window buffer))))
    (cond
     ;; Case 1: terminal window is visible and selected → close it
     ((and win (eq win (selected-window)))
      (delete-window win))

     ;; Case 2: terminal window is visible but not selected → jump to it
     (win (select-window win))

     ;; Case 3: buffer exists but no window → create a bottom split and show it
     (buffer
      (let ((new-win (split-window (selected-window) -15 'below)))
	(select-window new-win)
	(switch-to-buffer buffer)))
     (t
      (let ((new-win (split-window (selected-window) -15 'below)))
	(select-window new-win)
	(ansi-term (getenv "SHELL")))))))


(defun my/toggle-terminal-move ()
  (interactive)
  (let* ((buffer-name "*ansi-term*")
	(buffer      (get-buffer buffer-name))
	(win         (and buffer (get-buffer-window buffer))))
    (when buffer
      (setq my/terminal-position-below-p (not my/terminal-position-below-p))

      (when win
	(delete-window win))

      (let ((new-win
	     (if my/terminal-position-below-p
		 (split-window (selected-window) -15 'below)
	       (split-window (selected-window) -70 'right))))
	(select-window new-win)
	(switch-to-buffer buffer)))))


(provide 'terminal)
;;; terminal.el ends here
