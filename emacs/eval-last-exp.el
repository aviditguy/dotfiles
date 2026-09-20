;; -*- lexical-binding: t; -*-

(provide 'eval-last-exp)


(defun my-mode-p (mode lang)
  (or (derived-mode-p mode)
      (and (org-in-src-block-p)
	   (string=
	    (my-org-src-get :language)
	    lang))))


(defun my-indicate (beg end)
  (pulse-momentary-highlight-region beg end))


(defun my-eval-last-exp ()
  (interactive)

  (cond
   ((my-mode-p 'python-mode "python")
    (my-eval-last-pyexp))

   ((my-mode-p 'lisp-mode "lisp")
    (my-eval-last-lspexp))

   (t
    (message "Context not supported"))))


;; (global-set-key (kbd "C-M-e") #'my-eval-last-exp)


;; ============================================================
;; EVAL LAST EXPRESSION PYTHON
;; ============================================================

(defun my-eval-last-pyexp ()
  (let ((end  (point))
	(beg  nil)
	(expr nil))
    
    (save-excursion
      (beginning-of-line)

      (while (or (looking-at "^[ \t]")
		 (looking-at "^\\(else\\|elif\\|except\\|]\\|\\)\\|})"))
	(forward-line -1))

      (setq beg (point)))

    (setq expr (string-trim
		(buffer-substring-no-properties
		 beg end)))
    
    (my-indicate beg end)
  
    (my-vterm-send expr)

    (catch 'break
      (dolist (prefix '("if" "for" "while" "with" "def" "class" "async" "try"))
	(when (string-prefix-p prefix expr)
	  (my-vterm-send "\n")
	  (throw 'break nil))))))

;; ============================================================
;; EVAL LAST EXPRESSION LISP
;; ============================================================

(use-package sly
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (sly-setup '(sly-fancy)))


(defun my-get-last-expression-lisp ()
  (with-syntax-table emacs-lisp-mode-syntax-table
    (thing-at-point 'sexp t)))


(defun my-eval-last-lspexp ()
  (let ((expr (my-get-last-expression-lisp)))
    (if (string-prefix-p "(defun " expr)
	(sly-eval-last-expression)
      (if-let* ((buf (get-buffer "*sly-mrepl for sbcl*")))
	  (with-current-buffer buf
	    (goto-char (point-max))
	    (insert expr)
	    (sly-mrepl-return)
	    (goto-char (point-max)))
	(message "SLY REPL not running")))))
