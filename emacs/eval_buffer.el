;; -*- lexical-binding: t; -*-

(defvar my--build-commands
  '(("py"  . "python %s")
    ("js"  . "node %s")
    ("sh"  . "bash %s")
    ("awk" . "awk -f %s")))


;; (defvar my--build-command-modes
;;   '((cc-mode     c-mode-map c++-mode-map)
;;     (python      python-mode-map)
;;     (js          js-mode-map)
;;     (sh-script   sh-mode-map)
;;     (awk-mode    awk-mode-map)))


(defun my-eval-buffer (&optional file)
  (interactive)
  
  (let* ((inp (or file (buffer-file-name))))
    (unless inp
      (user-error "Buffer is not a visiting a file"))

    (let* ((out   (file-name-sans-extension inp))
	   (ext   (file-name-extension inp))
	   (flags nil)
	   (sinp  (shell-quote-argument inp))
	   (sout  (shell-quote-argument out)))

      ;; get the flags
      (with-temp-buffer
	(insert-file-contents inp nil 0 300)
	(goto-char (point-min))
	  
	(setq flags
	      (if (looking-at "^//[ \t]*\\(.*\\)$")
		  (match-string 1)
		"")))
      
      (cond
       ((member ext '("c" "cpp"))
	(let ((cc (if (string= ext "c") "gcc" "g++")))
	  (my-vterm-send
	   (format "%s -o %s %s %s && %s"
		   cc sout sinp flags sout))))
       
       ((assoc ext my--build-commands)
	(my-vterm-send
	 (format (cdr (assoc ext my--build-commands)) sinp)))

       (t
	(message "No command for language: %s" ext))))))


;; (defun my-setup-build-key ()
;;   (mapc
;;    (lambda (entry)
;;      (let ((feature (car entry))
;;            (maps    (cdr entry)))

;;        (eval-after-load
;;            feature
;;          `(progn
;;             ,@(mapcar
;;                (lambda (map)
;;                  `(define-key
;;                     ,map
;;                     (kbd "C-c C-c")
;;                     #'my-eval-buffer))
;;                maps)))))
;;    my--build-command-modes))

;; (defun my-setup-build-key ()
;;   (mapc
;;    (lambda (entry)
;;      (let ((feature (car entry))
;;            (maps    (cdr entry)))

;;        (with-eval-after-load feature
;;          (mapc
;;           (lambda (map)
;;             (define-key
;;              (symbol-value map)
;;              (kbd "C-c C-c")
;;              #'my-eval-buffer))
;;           maps))))
;;    my--build-command-modes))

;; (my-setup-build-key)


;; (with-eval-after-load 'cc-mode
;;   (mapc
;;    (lambda (mode)
;;      (define-key
;;       (symbol-value mode)
;;       (kbd "C-c C-c")
;;       #'my-eval-buffer))
;;    '(c-mode-map
;;      c++-mode-map)))

;; (with-eval-after-load 'python
;;   (define-key python-mode-map
;;     (kbd "C-c C-c")
;;     #'my-eval-buffer))

;; (with-eval-after-load 'js
;;   (define-key js-mode-map
;;     (kbd "C-c C-c")
;;     #'my-eval-buffer))

;; (with-eval-after-load 'sh-script
;;   (define-key sh-mode-map
;;     (kbd "C-c C-c")
;;     #'my-eval-buffer))

;; (with-eval-after-load 'awk-mode
;;   (define-key awk-mode-map
;;     (kbd "C-c C-c")
;;     #'my-eval-buffer))


;; (add-hook 'awk-mode-hook
;;           (lambda ()
;;             (local-set-key
;;              (kbd "C-c C-c")
;;              #'my-eval-buffer)))



(defvar my--build-command-hooks
  '(c-mode-hook
    c++-mode-hook
    python-mode-hook
    js-mode-hook
    sh-mode-hook
    awk-mode-hook))

(mapc
 (lambda (hook)
   (add-hook hook
             (lambda ()
               (local-set-key
                (kbd "C-c C-c")
                #'my-eval-buffer))))
 my--build-command-hooks)

