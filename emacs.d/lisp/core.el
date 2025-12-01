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

(recentf-mode 1)
(add-hook 'emacs-startup-hook #'recentf-open-files)

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
(set-face-attribute 'default nil :font "SauceCodePro NF ExtraLight" :height 140)

(provide 'core)
;;; core.el ends here
