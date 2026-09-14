;; -*- lexical-binding: t; -*-

(defvar my--build-commands
  '(("py"  . "python %s %s")
    ("js"  . "node %s %s")
    ("sh"  . "bash %s %s")
    ("awk" . "awk -f %s %s")))


(defun my-parse-header (key &optional file)
  (let ((inp (or file (buffer-file-name))))

    (with-temp-buffer
      (insert-file-contents inp nil 0 300)
      (goto-char (point-min))

      (when (looking-at "^\\(?://\\|#\\)[ \t]*\\(.*\\)$")
        (let ((params (match-string 1)))
          (when (string-match
                 (format "%s[ \t]+\\([^:]*\\)" key)
                 params)
            (string-trim (match-string 1 params))))))))


(defun my-eval-buffer (&optional file)
  (interactive)
  
  (let ((inp  (or file (buffer-file-name))))
    (unless inp
      (user-error "Buffer is not a visiting a file"))

    (save-buffer)  ;; save file before run
    
    (let* ((out   (file-name-sans-extension inp))
	   (ext   (file-name-extension inp))
	   (sinp  (shell-quote-argument inp))
	   (sout  (shell-quote-argument out))
	   (flags (or (my-parse-header :flags) ""))
	   (args  (or (my-parse-header :args) "")))
    
      (cond
       ;; for c, c++
       ((member ext '("c" "cpp"))
	(let ((cc (if (string= ext "c") "gcc" "g++")))
	  (my-vterm-send
	   (format "%s -o %s %s %s && %s %s"
		   cc sout sinp flags sout args))))

       ;; for js, python, sh, awk
       ((assoc ext my--build-commands)
	(my-vterm-send
	 (format (cdr (assoc ext my--build-commands)) sinp args)))

       (t
	(message "No command for language: %s" ext))))))


(with-eval-after-load 'cc-mode
  (mapc
   (lambda (mode)
     (define-key
      (symbol-value mode)
      (kbd "C-c C-c")
      #'my-eval-buffer))
   '(c-mode-map
     c++-mode-map)))

(with-eval-after-load 'python
  (define-key python-mode-map
    (kbd "C-c C-c")
    #'my-eval-buffer))

(with-eval-after-load 'js
  (define-key js-mode-map
    (kbd "C-c C-c")
    #'my-eval-buffer))

(with-eval-after-load 'sh-script
  (define-key sh-mode-map
    (kbd "C-c C-c")
    #'my-eval-buffer))

;; `awk-mode` keybinding not working
(add-hook 'awk-mode-hook
          (lambda ()
            (local-set-key
             (kbd "C-c C-c")
             #'my-eval-buffer)))
