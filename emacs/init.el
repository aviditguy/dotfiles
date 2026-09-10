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


;; ============================================================
;; ORG MODE
;; ============================================================

(require 'org)
(require 'org-tempo)


;; PACKAGES
(use-package visual-fill-column)

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :custom
  ;; Headings & Lists
  (org-modern-star '("◉" "○" "◆" "◇" "▶" "▷"))
  (org-modern-list '((?- . "•")
                     (?+ . "‣")
                     (?* . "⁃")))

  ;; Checkboxes
  (org-modern-checkbox
   '((?X . "✓")))

  ;; Tables & Blocks
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 1)
  (org-modern-block-fringe 4)
  (org-modern-block-name t)
  (org-modern-block-border t)

  ;; TODO & Tags
  (org-modern-todo t)
  (org-modern-tag t))


(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))


;; BASIC ORG SETTINGS
(setq org-hide-emphasis-markers t)
;; (setq org-startup-folded 'overview)
(setq org-confirm-babel-evaluate nil)

(setq org-src-window-setup 'current-window)
(setq org-src-preserve-indentation t)
(setq org-edit-src-content-indentation 0)

(setq org-ellipsis " ▼ ")
(setq org-indent-indentation-per-level 3)


;; VISUAL FILL / WRAPPING
(defun my/org-visual-setup ()
  (setf visual-fill-column-width 110
	visual-fill-column-center-text t)
  
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(add-hook 'org-mode-hook #'my/org-visual-setup)
(add-hook 'org-mode-hook #'org-indent-mode)


;; SOURCE BLOCK TEMPLATES
(setq org-structure-template-alist
      '(("c"      . "src c")
        ("py"     . "src python")
        ("sh"     . "src shell")
	("awk"    . "src awk")
        ("el"     . "src emacs-lisp")
	("lisp"   . "src lisp")))


;; ORG FACES & FILE ASSOCIATIONS
(with-eval-after-load 'org

  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5
                  org-level-6
                  org-level-7
                  org-level-8))

    (set-face-attribute face nil
                        :weight 'bold
                        :height 1.1))

  ;; Images
  (add-to-list 'org-file-apps '("\\.png\\'"  . "feh %s"))
  (add-to-list 'org-file-apps '("\\.jpg\\'"  . "feh %s"))
  (add-to-list 'org-file-apps '("\\.jpeg\\'" . "feh %s"))
  (add-to-list 'org-file-apps '("\\.gif\\'"  . "feh %s"))
  (add-to-list 'org-file-apps '("\\.webp\\'" . "feh %s"))
  (add-to-list 'org-file-apps '("\\.svg\\'"  . "feh %s"))

  ;; Video
  (add-to-list 'org-file-apps '("\\.mp4\\'" . "mpv %s")))


;; LATEX / TIKZ PREVIEW
;; Set dvisvgm as the default process for LaTeX previews
(setq org-preview-latex-default-process 'dvisvgm)

;; Ensure the TikZ package is included in the preview preamble
(add-to-list 'org-latex-packages-alist
	     '("" "tikz" t))

;; Enable the tikzpicture environment for previewing
(eval-after-load "preview"
  '(add-to-list
    'preview-default-preamble
    "\\PreviewEnvironment{tikzpicture}"
    t))

(setq org-format-latex-options
      (plist-put
       (plist-put org-format-latex-options
                  :scale 1)
       :latex-header
       "\\usepackage{cancel}
\\usepacage{multirow}"))

;; (setq org-format-latex-options
;;       (plist-put org-format-latex-options
;; 		 :scale 1.1))

;; (set
;; 		 :latex-header
;; 		 "\\usepackage{cancel}"))

(setq org-preview-latex-image-directory
      "~/.ltximg/")


;; IMAGES
(setq org-image-align 'center)
(setq org-image-actual-width 500)



;; ============================================================
;; TERMINAL SYSTEM
;; ============================================================

(use-package vterm)


(defvar my--vterm-name "*vterm-default*")
(defvar my--vterm-below-p t)
(defvar my--vterm-height 15)
(defvar my--vterm-width 80)


(defun my-vterm-is-focused ()
  (let ((win (get-buffer-window my--vterm-name)))
    (if (eq win (selected-window))
	win
      nil)))

(defun my-vterm-update-height ()
  (when my--vterm-below-p
    (setf my--vterm-height
	  (window-height (get-buffer-window
			  my--vterm-name)))))

(defun my-vterm-update-width ()
  (unless my--vterm-below-p
    (setf my--vterm-width
	  (window-width (get-buffer-window
			 my--vterm-name)))))


(defun my-vterm-show (&optional switch)
  (let ((buf (get-buffer my--vterm-name)))
    (unless buf
      (save-window-excursion
	(vterm my--vterm-name)))

    (let ((win (get-buffer-window my--vterm-name)))
      (unless win
	(setf win
	      (if my--vterm-below-p
		  (split-window nil (- my--vterm-height) 'below)
		(split-window nil (- my--vterm-width) 'right)))
	(set-window-buffer win my--vterm-name))

      (when switch (select-window win)))))


(defun my-vterm-hide ()
  (when-let* ((win (my-vterm-is-focused)))
    (my-vterm-update-height)
    (my-vterm-update-width)
    (delete-window win)))


(defun my-vterm-toggle ()
  (interactive)

  (if (my-vterm-is-focused)
      (my-vterm-hide)
    (my-vterm-show t)))


(defun my-vterm-move ()
  (interactive)

  (my-vterm-show t)
  (my-vterm-hide)

  (setf my--vterm-below-p
	(not my--vterm-below-p))

  (my-vterm-show t))


(global-set-key (kbd "C-`") #'my-vterm-toggle)
(global-set-key (kbd "C-M-`") #'my-vterm-move)


(defun my-vterm-send (command)
  (my-vterm-show)
  (with-current-buffer my--vterm-name
    (goto-char (point-max))
    (vterm-send-string command t)
    (vterm-send-return)))
