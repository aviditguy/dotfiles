(require 'package)

;; Add archives
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

;; Initialize package system
(package-initialize)

;; Ensure package list is available
(unless package-archive-contents (package-refresh-contents))

;; Install use-package if needed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)  ;; auto-install packages


;; Doom themes
(use-package doom-themes)


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

(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")   ;; start SBCL as backend
  ;; company integration
  (add-hook 'sly-mode-hook #'company-mode)
  (add-hook 'sly-mrepl-mode-hook #'company-mode))

(with-eval-after-load 'sly
  (define-key sly-mode-map (kbd "C-c C-k") #'sly-compile-and-load-file) ; whole file
  (define-key sly-mode-map (kbd "C-c C-l") #'sly-eval-defun))          ; current defun


(provide 'packages)
