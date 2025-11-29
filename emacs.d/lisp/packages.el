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
        company-idle-delay 0.0))  ;; popup instantly (or set 0.2 if too aggressive)

(use-package eglot
  :ensure t
  :hook (python-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  ;; Handy keybindings
  (define-key eglot-mode-map (kbd "C-c r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c a") #'eglot-code-actions))

;; Eldoc-box: automatic hover docs
(use-package eldoc-box
  :ensure t
  :after eglot
  :config
  (define-key eglot-mode-map (kbd "C-c h") #'eldoc-box-help-at-point))


(provide 'packages)
