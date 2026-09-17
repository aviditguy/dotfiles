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
;; ORG CAROUSEL
;; ============================================================

(defvar my--carousel-overlay nil)
(defvar my--carousel-images  nil)
(defvar my--carousel-index   0)


(defun my-org-src-get (key)
  (and (derived-mode-p 'org-mode)
       (let ((el (org-element-context)))
	 (when (eq (org-element-type el) 'src-block)
	   (org-element-property key el)))))


(defun my-carousel-src-p ()
  (string=
   (my-org-src-get :language)
   "carousel"))


(defun my-carousel-set-image ()
  (when (overlayp my--carousel-overlay)
    
    (let* ((img-path (nth my--carousel-index my--carousel-images))
	   (img-name (file-name-nondirectory (expand-file-name img-path)))
	   (total    (length my--carousel-images))
	   (count    (1+ my--carousel-index)))

      (overlay-put
       my--carousel-overlay
       'display
       (create-image img-path nil nil :width 400))

      (overlay-put
       my--carousel-overlay
       'after-string
       (format "\n%s  (%d/%d)\n\n" img-name count total)))))


(defun my-carousel-create ()
  (interactive)
  
  (when (my-carousel-src-p)

    (my-carousel-remove)

    (let ((beg  (my-org-src-get :begin))
	  (end  (my-org-src-get :end)))

      (setf my--carousel-images
	    (split-string
	     (my-org-src-get :value)
	     "\n" t))

      (setf my--carousel-index 0)

      (setf my--carousel-overlay
	    (make-overlay beg end))

      (my-carousel-set-image))))


(defun my-carousel-remove ()
  (interactive)

  (when (overlayp my--carousel-overlay)
    (delete-overlay my--carousel-overlay)
    (setf my--carousel-overlay nil)))


(defun my-carousel-next ()
  (interactive)

  (when (and (overlayp my--carousel-overlay)
	     (my-carousel-src-p))
    (setf my--carousel-index
	  (mod
	   (1+ my--carousel-index)
	   (length my--carousel-images)))

    (my-carousel-set-image)))

;; (define-key org-mode-map (kbd "<right>") #'my-carousel-next)

(defun my-carousel-previous ()
  (interactive)

  (when (overlayp my--carousel-overlay)
    (setf my--carousel-index
          (mod
           (1- my--carousel-index)
           (length my--carousel-images)))

    (my-carousel-set-image)))


(defun my-carousel-toggle ()
  (interactive)
  (if (overlayp my--carousel-overlay)
      (my-carousel-remove)
    (my-carousel-create)))

(defvar my--carousel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'my-carousel-toggle)
    (define-key map (kbd "<right>")     #'my-carousel-next)
    (define-key map (kbd "<left>")      #'my-carousel-previous)
    map))

(define-minor-mode my-carousel-mode
  "Keymap for navigating carousel blocks."
  :lighter nil
  :keymap my--carousel-mode-map)

(defun my-carousel-activate-map ()
  (my-carousel-mode
   (if (my-carousel-src-p) 1 -1)))


(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'post-command-hook
                      #'my-carousel-activate-map
                      nil
                      t)))






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


(defun my-org-src-key (key)
  (when (org-in-src-block-p)
    
    (let* ((el   (org-element-context))
	   (res  (org-element-property key el))
	   (args (org-babel-parse-header-arguments
		  (org-element-property :parameters el))))

      (or res
	  (cdr (assoc key args))))))


;; (defun my-org-src-key (key)
;;   (when (org-in-src-block-p)
    
;;     (let* ((el   (org-element-context))
;; 	   (res  (org-element-property key el))
;; 	   (args (org-babel-parse-header-arguments
;; 		  (org-element-property :parameters el))))

;;       (or res
;; 	  (cdr (assoc key args))))))


;; (defun my-extract-c-flags (file)
;;   (let ((ext (file-name-extension file)))

;;     (when (member ext '("c" "cpp"))
;;       (with-temp-buffer
;; 	(insert-file-contents file nil 0 300)
;; 	(goto-char (point-min))

;; 	(when (looking-at "^//[ \t]*\\(.*\\)$")
;; 	  (match-string 1))))))


(defun my-org-src-extension ()
  (let ((lang (my-org-src-key :language)))
    (cond
     ((string= lang "c")   ".c")
     ((string= lang "cpp") ".cpp")
     ((string= lang "py")  ".py")
     ((string= lang "js")  ".js")
     ((string= lang "emacs-lisp") ".el"))))


(defun my-org-src-name ()
  (when (org-in-src-block-p)
    (concat my--org-src-directory
	    (file-name-sans-extension (buffer-name))
	    (my-org-src-extension))))


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


(load "/home/ashura/Workspace/dotfiles/emacs/templates.el")
(load "/home/ashura/Workspace/dotfiles/emacs/terminal.el")
(load "/home/ashura/Workspace/dotfiles/emacs/eval_buffer.el")
