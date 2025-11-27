(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq ring-bell-function 'ignore)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(ido-mode 1)
(ido-everywhere 1)
(show-paren-mode 1)
(global-auto-revert-mode 1)

(save-place-mode 1)  ;; reopen file at last cursor position

(recentf-mode 1)
(add-hook 'emacs-startup-hook #'recentf-open-files)

(add-hook 'org-mode-hook #'org-display-inline-images)  ;; display inline-images org

(setq auto-save-default nil)   ;; Disable auto-saving
(setq make-backup-files nil)   ;; Disable backup~ files
(setq create-lockfiles nil)    ;; Disable .#lock files

(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 2 1000 1000))))

(set-face-attribute 'default nil :font "SauceCodePro NF ExtraLight" :height 140)

(provide 'core)
