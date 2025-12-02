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

(global-set-key (kbd "C-`") #'my/toggle-terminal)

(provide 'keybindings)
;;; keybindings.el ends here
