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
  "jump to beginning --> indentation --> end of line"
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


(defun my/goto-line (arg)
  "Goto Line Absolute or Relative
- N     -> move N lines down (relative)
- -N    -> move N lines up (relative)
- @N    -> go to absolute line N"
  (interactive (list (read-string "Goto Line (@N, N, -N): ")))
  (let ((str (string-trim arg)))
    (cond
     ;; Relative: N or -N
     ((string-match-p "\\`[-]?[0-9]+\\'" str)
      (forward-line (string-to-number str)))

     ;; Absolute: @N
     ((string-match-p "\\`@[0-9]+\\'" str)
      (goto-char (point-min))
      (forward-line  (1- (string-to-number (substring str 1)))))
     (t
      (user-error "Invalid input: %s (use @N, N or -N)" arg)))))


(defun my/python-use-project-venv ()
  "If a .venv directory exists in a parent, use its Python for this buffer."
  (let* ((root (locate-dominating-file default-directory ".venv"))
         (venv (when root (expand-file-name ".venv" root)))
         (python-bin (when venv (expand-file-name "bin/python" venv))))
    (when (and python-bin (file-exists-p python-bin))
      ;; Foer python-mode's `run-python`
      (setq-local python-shell-interpreter python-bin)
      ;; Optional: also adjust PATH/exec-path so black/ruff inside .venv are used
      (make-local-variable 'exec-path)
      (add-to-list 'exec-path (expand-file-name "bin" venv))
      (setenv "PATH"
              (concat (expand-file-name "bin" venv) ":" (getenv "PATH"))))))
(add-hook 'python-mode-hook #'my/python-use-project-venv)


(defun format-current-buffer ()
  "Format current buffer with clang-format (for C/C++) or black (for Python)."
  (interactive)
  (when buffer-file-name
    (save-buffer) ;; save before formatting
    (cond
     ((derived-mode-p 'c-mode 'c++-mode)
      (shell-command (format "clang-format -i %s" (shell-quote-argument buffer-file-name))))
     ((derived-mode-p 'python-mode)
      (shell-command (format "black %s" (shell-quote-argument buffer-file-name)))))
    (revert-buffer t t t))) ;; reload buffer after formatting
(global-set-key (kbd "C-c f") 'format-current-buffer)




(setq my/programming-modes
      '((c-mode     . "gcc %s -o %s && %s")
	(c-ts-mode  . "gcc %s -o %s && %s")))

(defun my/get-compile-command ()
  (cdr (assoc major-mode my/programming-modes)))

(defun my/run-code-block ()
  (interactive)
  (let* ((file (buffer-file-name))
	 (base (file-name-sans-extension file))
	 (cmd  (my/get-compile-command)))
    (if (and file cmd)
	(progn
	  (my/toggle-terminal)
	  (let ((term-buf (get-buffer "*ansi-term*")))
	    (when term-buf
	      (with-current-buffer term-buf
		(term-send-raw-string
		 (concat (format cmd file base base) "\n"))))))
      (message "No command defined for %s" major-mode))))

(global-set-key (kbd "C-c C-t") #'my/run-code-block)


(add-hook 'python-mode-hook
          (lambda ()
            (setq python-shell-interpreter "ipython"
                  python-shell-interpreter-args "-i --simple-prompt")))

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-x C-e")
    (lambda ()
      (interactive)
      (python-shell-send-region
       (save-excursion
         (python-nav-beginning-of-statement)
         (point))
       (point)))))



(provide 'functions)
;;; functions.el ends here
