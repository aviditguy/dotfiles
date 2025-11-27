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

(provide 'packages)
