;;; keybindings.el --- Global keybindings -*- lexical-binding: t; -*-

;; Theme cycling (theme.el defines my/cycle-theme)
(global-set-key (kbd "<f5>") #'my/cycle-theme)

;; Buffer / file helpers (functions.el)
(global-set-key (kbd "C-c oe") #'my/open-init-file)
(global-set-key (kbd "C-c od") #'my/open-dotfiles)
(global-set-key (kbd "C-c or") #'my/reload-config)


(global-set-key (kbd "M-<up>") #'my/move-line-up)

(global-set-key (kbd "C-a") #'my/move-bie-of-line)

(global-set-key (kbd "M-g M-g") #'my/goto-line)

;; ─────────────────────────────────────────────
;; Terminal Keybindings
;; ─────────────────────────────────────────────
(global-set-key (kbd "C-`") #'my/toggle-terminal)
(global-set-key (kbd "C-c C-`") #'my/toggle-terminal-move)

;; ─────────────────────────────────────────────
;; Window resizing with C-x + C-arrow keys
;; ─────────────────────────────────────────────
(global-set-key (kbd "C-x C-<up>")    (lambda () (interactive) (enlarge-window 2)))
(global-set-key (kbd "C-x C-<down>")  (lambda () (interactive) (shrink-window 2)))
(global-set-key (kbd "C-x C-<left>")  (lambda () (interactive) (shrink-window-horizontally 2)))
(global-set-key (kbd "C-x C-<right>") (lambda () (interactive) (enlarge-window-horizontally 2)))

(provide 'keybindings)
;;; keybindings.el ends here
