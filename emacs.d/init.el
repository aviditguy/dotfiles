(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)  ;; auto-install packages

;; Doom themes
(use-package doom-themes)
(load-theme 'doom-material-dark t)

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)
  (add-to-list 'company-backends 'company-capf))

(use-package eglot
  :ensure t
  :hook ((python-mode . eglot-ensure)
	 (c-mode      . eglot-ensure)
	 (c++-mode    . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(c-mode      . ("clangd")))
  (add-to-list 'eglot-server-programs '(c++-mode    . ("clangd")))
  ;; Handy keybindings
  (define-key eglot-mode-map (kbd "C-c r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c a") #'eglot-code-actions))

;; Eldoc-box: automatic hover docs
(use-package eldoc-box
  :ensure t
  :after eglot
  :config
  (define-key eglot-mode-map (kbd "C-c h") #'eldoc-box-help-at-point))




;; UI Settings
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq ring-bell-function 'ignore)

;; Line Numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Other Useful settings
(ido-mode 1)
(ido-everywhere 1)
(show-paren-mode 1)
(save-place-mode 1)          ;; Remember cursor positions in files
;;(desktop-save-mode 1)        ;; Remembers buffers, window layout, etc
(global-auto-revert-mode 1)  ;; if file changes on disk reload its buffer

;; Never let point get closer than 10 lines to top/bottom of window
(setq scroll-margin 10)
(setq scroll-conservatively 101)   ;; smooth scrolling, no recentering
(setq scroll-step 1)               ;; scroll by 1 line when needed

;; electric pair
(setq electric-pair-pairs '((?\( . ?\))  ; parentheses
                            (?\[ . ?\])  ; square brackets
                            (?\{ . ?\})  ; curly braces
                            (?\" . ?\")  ; double quotes
                            (?\' . ?\'))) ; single quotes

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq-local electric-pair-pairs '((?\( . ?\))
					      (?\" . ?\")))))
(electric-pair-mode 1)

;; Handle temporary files
(setq auto-save-default nil)   ;; Disable auto-saving
(setq make-backup-files nil)   ;; Disable backup~ files
(setq create-lockfiles nil)    ;; Disable .#lock files

;; Something Performace Wise via ChatGPT
(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 2 1000 1000))))

;; Set Font
(set-face-attribute 'default nil :font "Iosevka ExtraLight Extended" :height 115)




(setq org-hide-emphasis-markers t)
(setq org-startup-folded 'overview)
(setq org-confirm-babel-evaluate nil)

(setq org-src-window-setup 'current-window)
(setq org-src-preserve-indentation t)
(setq org-edit-src-content-indentation 0)
(setq org-ellipsis " ▼ ")

(setq org-indent-indentation-per-level 3)
(add-hook 'org-mode-hook 'org-indent-mode)

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :custom
  ;; Headings & Lists
  (org-modern-star '("◉" "○" "◆" "◇" "▶" "▷"))
  (org-modern-list '((?- . "•") (?+ . "‣") (?* . "⁃")))
  ;; Checkboxes
  (org-modern-checkbox
   '((?X . "🟩") (?- . "▢") (?\s . "⬜")))
  ;; Tables & blocks
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 1)
  (org-modern-block-fringe 4)
  (org-modern-block-name t)
  (org-modern-block-border t)
  ;; Tags & todo keywords
  (org-modern-todo t)
  (org-modern-tag t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit default :weight normal :height 1.5))))
 '(org-level-2 ((t (:inherit default :height 1.4))))
 '(org-level-3 ((t (:inherit default :height 1.3))))
 '(org-level-4 ((t (:inherit default :height 1.2))))
 '(org-level-5 ((t (:inherit default :height 1.1))))
 '(org-level-6 ((t (:inherit default :height 1.1))))
 '(org-level-7 ((t (:inherit default :height 1.1))))
 '(org-level-8 ((t (:inherit default :height 1.1)))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell      . t)
   (C          . t)
   (js         . nodejs)
   (python     . t)))

(require 'org-tempo)
(setq org-structure-template-alist
      '(("C"      . "src C")
        ("cpp"    . "src C++")
        ("py"     . "src python")
        ("sh"     . "src shell")
	("script" . "src shell-script")
        ("js"     . "src js")
        ("el"     . "src emacs-lisp")
	("lisp"   . "src lisp")
	("mk"     . "src makefile")))

(with-eval-after-load 'org
  (add-to-list 'org-file-apps '("\\.png\\'" . "feh %s"))
  (add-to-list 'org-file-apps '("\\.jpg\\'" . "feh %s"))
  (add-to-list 'org-file-apps '("\\.jpeg\\'" . "feh %s"))
  (add-to-list 'org-file-apps '("\\.gif\\'" . "feh %s"))
  (add-to-list 'org-file-apps '("\\.webp\\'" . "feh %s"))
  (add-to-list 'org-file-apps '("\\.svg\\'" . "feh %s")))









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







;; ────────────────────────────────────────────────────────────────────────────────────────────────────
;; Formatter
;; ────────────────────────────────────────────────────────────────────────────────────────────────────
(setq my/lang-formatters
      '((c-mode          . "clang-format -i %f")
	(c++-mode        . "clang-format -i %f")
	(python-mode     . "black %f")
	(org-mode        . org)))

(defun my/format-region-with-temp-file (formatter)
  (let* ((tmp (make-temp-file "orgfmt" nil ".tmp"))
	 (content (buffer-string)))
    (unwind-protect
	(progn
	  ;; Write src buffer content
	  (with-temp-file tmp
	    (insert content))

	  ;; run formatter
	  (shell-command
	   (replace-regexp-in-string
	    "%f"
	    (shell-quote-argument tmp)
	    formatter))

	  ;; replace buffer with formatted content
	  (erase-buffer)
	  (insert-file-contents tmp))
      (delete-file tmp))))

(defun my/apply-formatter (formatter)
  (cond
   ;; Org src edit buffer → region/temp file
   ((and (stringp formatter)
	 (org-src-edit-buffer-p))
    (my/format-region-with-temp-file formatter))

   ;; Normal file buffer
   ((stringp formatter)
    (when buffer-file-name
      (save-buffer)
      (let ((cmd (replace-regexp-in-string
		  "%f"
		  (shell-quote-argument buffer-file-name)
		  formatter)))
	(shell-command cmd)
	(revert-buffer t t t))))

   ;; Org
   ((eq formatter 'org)
    (org-indent-indent-buffer))))


(defun my/format-buffer ()
  (interactive)
  (let* ((formatter (assoc major-mode my/lang-formatters)))
    (if (not formatter)
	(message "No formatters for %s" mode)
      (my/apply-formatter (cdr formatter)))))

(global-set-key (kbd "C-c f") #'my/format-buffer)




;; ────────────────────────────────────────────────────────────────────────────────────────────────────
;; Terminal
;; ────────────────────────────────────────────────────────────────────────────────────────────────────
;; :win     → terminal created hai ya nahi (existence flag)
;; :active  → terminal visible hai ya background me
;; :below   → split direction (t = bottom, nil = right)
(setq my/term-state '(:win nil :active nil :below t))

a
efun my/term-create ()
  "Create ansi-term in background and update my/term-state."
  (interactive)
  (unless (plist-get my/term-state :win)
    (save-window-excursion
      (ansi-term (getenv "SHELL")))
    ;; update state
    (setq my/term-state (plist-put my/term-state :win t))))


(defun my/term-show ()
  (interactive)
  (unless (plist-get my/term-state :win)
    (my/term-create))

  ;; only show if not already active
  (unless (plist-get my/term-state :active)
    (let ((win (split-window (selected-window)
			     (if (plist-get my/term-state :below) -15 -100)
			     (if (plist-get my/term-state :below) 'below 'right))))
      (set-window-buffer win (get-buffer "*ansi-term*"))
      (select-window win)
      (setq my/term-state (plist-put my/term-state :active t)))))

(defun my/term-hide ()
  (interactive)
  (if (plist-get my/term-state :active)
      (progn
	(when-let ((win (get-buffer-window (get-buffer "*ansi-term*"))))
	  (delete-window win))
	(setq my/term-state (plist-put my/term-state :active nil)))))

(defun my/term-buffer-killed-hook ()
  "Reset my/term-state if *ansi-term* buffer is killed."
  (when (and (boundp 'my/term-state)
             (string= (buffer-name) "*ansi-term*"))
    (setq my/term-state '(:win nil :active nil :below t))))

(add-hook 'kill-buffer-hook #'my/term-buffer-killed-hook)

(defun my/term-send (cmd)
  (interactive)
  (my/term-show)
  (let* ((buf  (get-buffer "*ansi-term*"))
	 (proc (and buf (get-buffer-process buf))))
    (unless proc
      (error "No running ansi-term process"))
    (process-send-string proc (concat cmd "\n"))))

(defun my/term-toggle ()
  (interactive)

  (if (plist-get my/term-state :active)
      (my/term-hide)
    (my/term-show)))

(defun my/term-toggle-move ()
  (interactive)

  (setq my/term-state
	(plist-put my/term-state :below
		   (not (plist-get my/term-state :below))))
  (if (plist-get my/term-state :active)
      (progn
	(my/term-hide)
	(my/term-show))
    (my/term-show)))

(global-set-key (kbd "C-`") #'my/term-toggle)
(global-set-key (kbd "C-M-`") #'my/term-toggle-move)



;; ────────────────────────────────────────────────────────────────────────────────────────────────────
;; Run Org Code Blocks in Terminal
;; ────────────────────────────────────────────────────────────────────────────────────────────────────

(defun my/create-file (file content)
  (interactive)
  (let ((dir (file-name-directory file)))
    (when dir
      (make-directory dir t))
    (write-region content nil file)))


(defun my/string-to-plist (s)
  (when s
    (let* ((tokens (split-string s "[ \t\n]+" t))
	   (plist '()))
      (while tokens
	(let ((key (intern (pop tokens)))
	      (val (pop tokens)))
	  (setq plist (plist-put plist key val))))
      plist)))


(defun my/extract-src-block ()
  (interactive)
  (let ((element (org-element-context)))
    (unless (eq (org-element-type element) 'src-block)
      (error "Point is not inside an Org src block"))

    (let* ((lang    (org-element-property :language element))
	   (headers (org-element-property :parameters element))
	   (hplist  (my/string-to-plist headers))
	   (body    (org-element-property :value element))
	   (result  (list :lang lang :headers hplist :body body)))
      (message "%s" result)
      result)))

(defun my/run-org-src-block ()
  (interactive)
  (let* ((data    (my/extract-src-block))
	 (lang    (plist-get data :lang))
	 (headers (plist-get data :headers))
	 (path    (expand-file-name (plist-get headers :path)))
	 (body    (plist-get data :body)))
   
    (cond
     ((string= lang "shell")
      (my/term-send body))
     
     ((string= lang "makefile")
      (unless path
	(error "makefile requires :path"))
      (my/create-file path body)
      (my/term-send (format "make -f %s; rm -f %s" path path)))

     (t
      (message "Source (%s) written to %s lang path")))))



(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-<return>")
	      #'my/run-org-src-block))

