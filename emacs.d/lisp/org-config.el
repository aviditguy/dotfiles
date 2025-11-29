(setq org-hide-emphasis-markers t)
(setq org-startup-folded 'overview)
(setq org-confirm-babel-evaluate nil)

(setq org-src-window-setup 'current-window)
(setq org-src-preserve-indentation t)
(setq org-edit-src-content-indentation 0)
(setq org-ellipsis " ▼ ")

(setq org-indent-indentation-per-level 3)
(add-hook 'org-mode-hook 'org-indent-mode)

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :custom
  ;; Headings & Lists
  (org-modern-star '("◉" "○" "◆" "◇" "▶" "▷"))
  (org-modern-list '((?- . "•") (?+ . "‣") (?* . "⁃")))
  ;; Checkboxes
  (org-modern-checkbox
   '((?X . "🟩") (?- . "▢") (?\s . "⬜")))
  ;; Tables & blocks
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 1)
  (org-modern-block-fringe 4)
  (org-modern-block-name t)
  (org-modern-block-border t)
  ;; Tags & todo keywords
  (org-modern-todo t)
  (org-modern-tag t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit default :weight normal :height 1.5))))
 '(org-level-2 ((t (:inherit default :height 1.4))))
 '(org-level-3 ((t (:inherit default :height 1.3))))
 '(org-level-4 ((t (:inherit default :height 1.2))))
 '(org-level-5 ((t (:inherit default :height 1.1))))
 '(org-level-6 ((t (:inherit default :height 1.1))))
 '(org-level-7 ((t (:inherit default :height 1.1))))
 '(org-level-8 ((t (:inherit default :height 1.1)))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell      . t)
   (C          . t)
   (js         . nodejs)
   (python     . t)))

(require 'org-tempo)
(setq org-structure-template-alist
      '(("C"      . "src C")
        ("cpp"    . "src C++")
        ("py"     . "src python")
        ("sh"     . "src shell")
	("script" . "src shell-script")
        ("js"     . "src js")
        ("el"     . "src emacs-lisp")))

(with-eval-after-load 'org
  (add-to-list 'org-file-apps '("\\.png\\'" . "feh %s"))
  (add-to-list 'org-file-apps '("\\.jpg\\'" . "feh %s"))
  (add-to-list 'org-file-apps '("\\.jpeg\\'" . "feh %s"))
  (add-to-list 'org-file-apps '("\\.gif\\'" . "feh %s"))
  (add-to-list 'org-file-apps '("\\.webp\\'" . "feh %s"))
  (add-to-list 'org-file-apps '("\\.svg\\'" . "feh %s")))


(provide 'org-config)
