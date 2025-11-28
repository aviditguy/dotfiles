;;; functions.el --- General Functions -*- lexical-binding: t; -*-

;; Dotfiles / config helpers
(defun my/open-init-file ()
  "Open my main Emacs init file."
  (interactive)
  (find-file user-init-file))

(defun my/open-dotfiles ()
  "Open my dotfiles directory in Dired."
  (interactive)
  (dired "~/Workspace/dotfiles"))

(defun my/reload-config ()
  "Reload init.el."
  (interactive)
  (load-file user-init-file)
  (message "Config reloaded!"))

(defun my/move-line-up ()
  "Move current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))


(provide 'functions)
;;; functions.el ends here
