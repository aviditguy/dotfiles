;;; theme.el --- Theme cycling with persistence -*- lexical-binding: t; -*-

;; Where to store the current theme index
(defvar my/theme-index-file
  (locate-user-emacs-file "theme-index.el")
  "File where the current theme index is stored.")

;; List of themes to cycle through
(setq my/theme-list
      '(doom-material-dark
        doom-1337
        doom-acario-dark
        doom-ir-black))

(defvar my/theme-index 0
  "Current theme index in `my/theme-list`.")

(defun my/save-theme-index ()
  "Save `my/theme-index` to `my/theme-index-file`."
  (with-temp-file my/theme-index-file
    (insert (format "%S" my/theme-index))))

(defun my/load-theme-index ()
  "Load `my/theme-index` from `my/theme-index-file`, default to 0."
  (setq my/theme-index
        (if (file-exists-p my/theme-index-file)
            (with-temp-buffer
              (insert-file-contents my/theme-index-file)
              (read (buffer-string)))
          0)))

(defun my/load-theme-by-index (index)
  "Disable current themes and load theme at INDEX in `my/theme-list`."
  (let* ((len (length my/theme-list))
         (idx (mod index len))     ; wrap around
         (theme (nth idx my/theme-list)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)
    (setq my/theme-index idx)
    (my/save-theme-index)
    (message "Theme: %s" theme)))

(defun my/cycle-theme ()
  "Cycle through themes in `my/theme-list`."
  (interactive)
  (my/load-theme-by-index (1+ my/theme-index)))

;; ── Startup: restore last theme ──────────────────────────────────────────────
(my/load-theme-index)
(my/load-theme-by-index my/theme-index)

(provide 'theme)
;;; theme.el ends here
