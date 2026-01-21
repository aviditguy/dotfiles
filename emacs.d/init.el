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
(global-auto-revert-mode 1)  ;; if file changes on disk reload its buffer

;; Never let point get closer than 10 lines to top/bottom of window
(setq scroll-margin 10)
(setq scroll-conservatively 101)   ;; smooth scrolling, no recentering
(setq scroll-step 1)               ;; scroll by 1 line when needed

;; Handle temporary files
(setq auto-save-default nil)   ;; Disable auto-saving
(setq make-backup-files nil)   ;; Disable backup~ files
(setq create-lockfiles nil)    ;; Disable .#lock files

;; Something Performace Wise via ChatGPT
(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 2 1000 1000))))

;; Set Font
(set-face-attribute 'default nil :font "Monospace" :height 120)




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

(require 'org-tempo)
(setq org-structure-template-alist
      '(("c"      . "src c")
        ("cpp"    . "src cpp")
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









;; ────────────────────────────────────────────────────────────────────────────────────────────────────
;; CONFIG
;; ────────────────────────────────────────────────────────────────────────────────────────────────────

(defvar my/config
  '(:term-name "*my-terminal*"
	       :term-below t
	       :c-cmd "gcc %f -o /tmp/a.out -lm && /tmp/a.out"
	       :python-cmd "python3 %f"
	       :lisp-cmd "sbcl --script %f"
	       :c-fmt "clang-format -i %f"
	       :python-fmt "black %f"))


(defun my/helper--get-lang-extension (lang)
  (cond
   ((string= lang "c")      ".c")
   ((string= lang "python") ".py")
   (t                       ".txt")))


(defun my/helper--build-lang-command (file lang libs)
  (let ((cmd (plist-get my/config
			(intern (format ":%s-cmd" lang)))))
    (when cmd
      (let ((cmd1 (replace-regexp-in-string "%f" file cmd t t)))
	(if libs
	    (replace-regexp-in-string "&&" (concat libs " &&") cmd1 t t)
	  cmd1)))))


(defun my/helper--create-file (path lang content)
  (let* ((ext (my/helper--get-lang-extension lang))
	 (file (if path path (make-temp-file "my-" nil ext))))
    (when (and path (file-name-directory path))
      (make-directory (file-name-directory path) t))
    (with-temp-file file (insert content))
    file))

;; ────────────────────────────────────────────────────────────────────────────────────────────────────
;; TERMINAL TOGGLE SYSTEM
;; ────────────────────────────────────────────────────────────────────────────────────────────────────

(defun my/helper--create-terminal ()
  (let ((term-name (plist-get my/config :term-name)))
    (unless (buffer-live-p (get-buffer term-name))
      (save-window-excursion
	(let ((buf (ansi-term (getenv "SHELL"))))
	  (with-current-buffer buf
	    (rename-buffer term-name)))))))


(defun my/helper--terminal-visible-p ()
  (let ((win (get-buffer-window (plist-get my/config :term-name) t)))
    (and win (window-live-p win))))


(defun my/helper--show-terminal (&optional switch)
  (my/helper--create-terminal)

  (unless (my/helper--terminal-visible-p)
    (let ((win
	   (if (plist-get my/config :term-below)
	       (split-window nil -15 'below)
	     (split-window nil -100 'right))))
      (set-window-buffer win (get-buffer (plist-get my/config :term-name)))
      (when switch (select-window win)))))


(defun my/helper--hide-terminal ()
  (if (my/helper--terminal-visible-p)
      (delete-window
       (get-buffer-window (plist-get my/config :term-name)))))


(defun my/send-raw-string-terminal (cmd)
  (interactive)
  (my/helper--show-terminal)
  (with-current-buffer (get-buffer (plist-get my/config :term-name))
    (goto-char (point-max))
    (term-send-raw-string (concat cmd "\n"))))


(defun my/toggle-terminal ()
  (interactive)
  (if (my/helper--terminal-visible-p)
      (my/helper--hide-terminal)
    (my/helper--show-terminal t)))


(defun my/move-terminal ()
  (interactive)
  (setf my/config (plist-put my/config :term-below
			     (not (plist-get my/config :term-below))))
  (when (my/helper--terminal-visible-p)
    (my/helper--hide-terminal))
  (my/helper--show-terminal t))
      


;; ────────────────────────────────────────────────────────────────────────────────────────────────────
;; RUN ORG SRC BLOCK IN TERMINAL
;; ────────────────────────────────────────────────────────────────────────────────────────────────────

(defun my/helper--org-src-block-p ()
  (when (derived-mode-p 'org-mode)
    (let ((el (org-element-context)))
      (when (eq (org-element-type el) 'src-block)
	el))))


(defun my/helper--extract-org-src-block ()
  (when-let ((el (my/helper--org-src-block-p)))
    (let ((header (org-babel-parse-header-arguments
		   (org-element-property :parameters el))))
      (list
       :lang   (org-element-property :language el)
       :body   (org-element-property :value el)
       :path   (alist-get :path header)
       :run    (alist-get :run header)
       :single (alist-get :single header)
       :libs   (alist-get :libs header)))))


(defun my/run-org-src-block ()
  (interactive)
  (when-let* ((data (my/helper--extract-org-src-block))
	      (run  (plist-get data :run)))
    
    (let ((lang    (plist-get data :lang))
	  (path    (plist-get data :path))
	  (single  (plist-get data :single))
	  (libs    (plist-get data :libs))
	  (content (plist-get data :body))
	  (done    nil))

      (save-excursion
	(while (and (not done) (not single))
	  (condition-case nil
	      (progn
		(org-babel-previous-src-block)
		(let ((data (my/helper--extract-org-src-block)))
		  (when (and (eq nil (plist-get data :run))
			     (string= lang (plist-get data :lang)))
		    (setf content (concat (plist-get data :body) "\n" content)))))
	    (error
	     (setf done t)))))

      (let* ((file (my/helper--create-file path lang content))
	     (cmd  (my/helper--build-lang-command file lang libs)))

	(unless cmd
	  (error "No command for language %s" lang))

	(my/send-raw-string-terminal cmd)))))



;; ────────────────────────────────────────────────────────────────────────────────────────────────────
;; KEYBINDINGS
;; ────────────────────────────────────────────────────────────────────────────────────────────────────

(global-set-key (kbd "C-`") #'my/toggle-terminal)
(global-set-key (kbd "C-M-`") #'my/move-terminal)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-<return>")
	      #'my/run-org-src-block))

