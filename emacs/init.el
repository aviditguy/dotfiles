;; -*- lexical-binding: t; -*-

;; ============================================================
;; CORE
;; ============================================================

;; PACKAGE SYSTEM INITIALIZATION
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)  ;; auto-install packages


;; UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(show-paren-mode 1)
(save-place-mode 1)          ;; Remember cursor positions in files
(global-auto-revert-mode 1)  ;; if file changes on disk reload its buffer

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)


;; LINE NUMBERS
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Disable Line Numbers for certain modes
(defun my-disable-line-numbers ()
  (setq-local disable-line-numbers nil)
  (display-line-numbers-mode -1))

(dolist (mode '(org-mode-hook
		vterm-mode-hook
		pdf-view-mode-hook))
  (add-hook mode #'my-disable-line-numbers))


;; BACKUP FILES
(setq auto-save-default nil)   ;; Disable auto-saving
(setq make-backup-files nil)   ;; Disable backup~ files
(setq create-lockfiles nil)     ;; Disable .#lock files


;; PERFORMANCE
;; Something Performace Wise via ChatGPT
(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 2 1000 1000))))


;; FONT
(set-face-attribute 'default nil :font "Iosevka" :height 120)


(use-package doom-themes)
(load-theme 'doom-material-dark t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-org-src-get (key)
  (when (org-in-src-block-p)
    
    (let* ((el   (org-element-context))
	   (res  (org-element-property key el))
	   (args (org-babel-parse-header-arguments
		  (org-element-property :parameters el))))

      (if (eq key :end)
	  (let ((beg (org-element-property :begin el)))
	    (save-excursion
	      (goto-char beg)
	      (setf res
		    (re-search-forward "^#\\+end_src" nil t))))
	(or res
	    (cdr (assoc key args)))))))


(defun my-org-in-src-block-p (blocks)
  (when (member (my-org-src-get :language) blocks)
    (let ((start (point))
	  (beg   (my-org-src-get :begin))
	  (end   (my-org-src-get :end)))
      (<= start end))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'load-path
	     (file-name-directory
	      (or load-file-name buffer-file-name)))

(require 'org-config)
(require 'keybindings)

(require 'templates)
;; (require 'terminal)
;; (require 'carousel)
;; (require 'timer)
;; (require 'eval-last-exp)
(require 'org-typst)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-org-todo-toggle ()
  (interactive)

  (save-excursion

    ;; jump to heading
    (unless (org-at-heading-p)
      (re-search-backward "^[ \t]*\\*+ " nil t))

    (let ((state (org-get-todo-state)))
	
      (cond

       ;; TODO -> DONE
       ((string= state "TODO")
	(org-todo "DONE")
	(re-search-forward "^[ \t]*:CLOSED:" nil t)
	(insert (format " [%s]"
			(format-time-string "%Y-%m-%d %a %H:%M"))))

       ;; Remove TODO [Change normal heading to TODO]
       ((string= state "DONE")
	(org-todo "")
	(re-search-forward "^[ \t]*:PROPERTIES:" nil t)
	(beginning-of-line)
	(kill-line 4))

       ;; Create TODO [Change normal heading to TODO]
       (t
	(org-todo "TODO")
	(end-of-line)
	(insert "\n :PROPERTIES:\n")
	(insert (format " :CREATED: [%s]\n"
			(format-time-string "%Y-%m-%d %a %H:%M")))
	(insert (format " :CLOSED:\n"))
	(insert  " :END:"))))))


(with-eval-after-load 'org
  (define-key org-mode-map
	      (kbd "C-c C-t")
	      #'my-org-todo-toggle)

  (define-key org-mode-map
	      (kbd "S-<right>")
	      #'my-org-todo-toggle))



;; ============================================================
;; HELPER FUNCTIONS
;; ============================================================

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


;; (defun my-org-src-get (key)
;;   (when (org-in-src-block-p)
    
;;     (let* ((el   (org-element-context))
;; 	   (res  (org-element-property key el))
;; 	   (args (org-babel-parse-header-arguments
;; 		  (org-element-property :parameters el))))

;;       (or res
;; 	  (cdr (assoc key args))))))


(defun my-org-src-extension ()
  (let ((lang (my-org-src-key :language)))
    (cond
     ((string= lang "c")   ".c")
     ((string= lang "cpp") ".cpp")
     ((string= lang "py")  ".py")
     ((string= lang "js")  ".js")
     ((string= lang "emacs-lisp") ".el"))))


;; (defun my-org-src-name ()
;;   (when (org-in-src-block-p)
;;     (concat my--org-src-directory
;; 	    (file-name-sans-extension (buffer-name))
;; 	    (my-org-src-extension))))


  
(defun my-org-src-name ()
  (when (org-in-src-block-p)
    (let ((id (secure-hash
	       'sha256
	       (my-org-src-key :value))))
      
      (concat my--org-src-directory
	      (file-name-sans-extension (buffer-name))
	      "-"
	      (substring id 0 30)
	      (my-org-src-extension)))))

;; ============================================================
;; CORE
;; ============================================================

;; (defvar my--org-src-directory "/tmp/org-src/")

;; (defun my-eval-buffer (&optional path)
;;   (let* ((inp   (expand-file-name
;; 		 (or path (buffer-file-name))))
;; 	 (out   (file-name-sans-extension inp))
;; 	 (ext   (file-name-extension inp))
;; 	 (flags (or (my-extract-c-flags inp) "")))

;;     (cond
;;      ((string= ext "c")
;;       (my-vterm-send (format "gcc -o %s %s %s && %s"
;; 			     out inp flags out)))
     
;;      ((string= ext "cpp")
;;       (my-vterm-send (format "g++ -o %s %s %s && %s"
;; 			     out inp flags out)))
     
;;      ((string= ext "py")
;;       (my-vterm-send (format "python %s" inp)))
    
;;      ((string= ext "js")
;;       (my-vterm-send (format "node %s" inp))))))


;; (defun my-org-eval-src ()
;;   (when (org-in-src-block-p)
    
;;     (let* ((path  (my-org-src-key :file))
;; 	   (file  (or path (my-org-src-file-name)))
;; 	   (data  (my-org-src-key :value))
;; 	   (flags (my-org-src-key :flags))
;; 	   (wrap  (my-org-src-key :wrap)))

;;       (make-directory my--org-src-directory t)

;;       (unless path
;; 	(setf flags
;; 	      (if flags
;; 		  (format "// %s\n" flags)
;; 		""))

;; 	(setf data
;; 	      (if wrap
;; 		  (if (string= wrap "raylib")
;; 		      (format my--raylib-template data)
;; 		    (format (concat flags my--c-template) data))
;; 		(concat flags data)))
	
;; 	(with-temp-file file (insert data)))

;;       (my-eval-buffer file))))


;; (defun my-eval-src (&optional path)
;;   (interactive)
  
;;   (if (and (derived-mode-p 'org-mode)
;; 	   (org-in-src-block-p))
;;       (my-org-eval-src)
;;     (my-eval-buffer path)))


;; (with-eval-after-load 'cc-mode
;;   (mapc
;;    (lambda (mode)
;;      (define-key
;;       (symbol-value mode)
;;       (kbd "C-c C-c")
;;       (lambda ()
;; 	(interactive)
;; 	(my-eval-src))))
;;    '(c-mode-map c++-mode-map)))

;; (with-eval-after-load 'org
;;   (define-key org-mode-map
;; 	      (kbd "C-c C-c")
;; 	      #'my-eval-src))

;; (load "/home/ashura/Workspace/dotfiles/emacs/eval_buffer.el")

