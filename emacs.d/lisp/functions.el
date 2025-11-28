;;; functions.el --- General Functions -*- lexical-binding: t; -*-

;; Dotfiles / config helpers
(defun my/open-init-file ()
  "Open my main Emacs init file."
  (interactive)
  (find-file user-init-file))

(defun my/open-dotfiles ()
  "Open my dotfiles directory in Dired."
  (interactive)
  (dired "~/Workspace/dotfiles"))

(defun my/reload-config ()
  "Reload init.el."
  (interactive)
  (load-file user-init-file)
  (message "Config reloaded!"))

(defun my/move-up ()
  "Move current line up, or move active region up by one line.
If a region is active, move all the lines it touches.
If no region, move just the current line."
  (interactive)
  (if (use-region-p)
      ;; ── Move region up ───────────────────────────────
      (let* ((beg (region-beginning))
             (end (region-end)))
        (save-excursion
          ;; Make sure we work on full lines
          (goto-char beg)
          (beginning-of-line)
          (setq beg (point))
          (goto-char end)
          (unless (bolp) (end-of-line))
          (setq end (point))

          ;; Don't move if region starts at very top
          (goto-char beg)
          (if (bobp)
              (message "Already at the top")
            ;; Include the newline after the region
            (setq end (min (point-max) (1+ end)))

            ;; Grab text
            (let* ((text (buffer-substring beg end))
                   (line-above-start (save-excursion
                                       (goto-char beg)
                                       (forward-line -1)
                                       (point))))
              ;; Delete region and insert above
              (delete-region beg end)
              (goto-char line-above-start)
              (insert text)

              ;; Reselect moved region
              (set-mark (point))
              (goto-char (+ (point) (- end beg)))
              (exchange-point-and-mark)))))
    ;; ── No region: move single line up ────────────────
    (if (bobp)
        (message "Already at the top")
      (transpose-lines 1)
      (forward-line -2))))


(defun my/move-bie-of-line ()
  (interactive)
  (let ((beg-pos (line-beginning-position))
	(end-pos (line-end-position))
	(cur-pos (point))
	(idt-pos (save-excursion
		   (back-to-indentation)
		   (point))))
    (cond
     ((= cur-pos end-pos)
      (move-beginning-of-line 1))
     ((and (>= cur-pos beg-pos)
	   (< cur-pos idt-pos))
      (back-to-indentation))
     (t
      (move-end-of-line 1)))))



(provide 'functions)
;;; functions.el ends here
