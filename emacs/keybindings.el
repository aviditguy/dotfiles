;; -*- lexical-binding: t; -*-

(provide 'keybindings)

(require 'terminal)
(require 'carousel)
(require 'eval-last-exp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TERMINAL
(global-set-key (kbd "C-`") #'my-vterm-toggle)
(global-set-key (kbd "C-M-`") #'my-vterm-move)

;; EVAL LAST EXPRESSION
(global-set-key (kbd "C-M-e") #'my-eval-last-exp)

;; CAROUSEL
(defvar my--carousel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'my-carousel-toggle)
    (define-key map (kbd "<right>") #'my-carousel-next)
    (define-key map (kbd "<left>")  #'my-carousel-previous)
    map))

(define-minor-mode my-carousel-mode
  "Keymap for navigating carousel blocks."
  :lighter nil
  :keymap my--carousel-mode-map)

(defun my-carousel-activate-map ()
  (my-carousel-mode
   (if (my-org-carousel-p) 1 -1)))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'post-command-hook
                      #'my-carousel-activate-map
                      nil
                      t)))

