;; -*- lexical-binding: t; -*-

(provide 'terminal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VTERM SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; install package
(use-package vterm)


;; vterm setup variables
(defvar my--vterm-name "*vterm-default*")
(defvar my--vterm-below-p t)
(defvar my--vterm-height 15)
(defvar my--vterm-width 80)


;; checks if vterm window is selected or not
(defun my-vterm-is-focused ()
  (let ((win (get-buffer-window my--vterm-name)))
    (if (eq win (selected-window))
	win
      nil)))


;; updates `my--vterm-height` variable
(defun my-vterm-update-height ()
  (when my--vterm-below-p
    (setf my--vterm-height
	  (window-height (get-buffer-window
			  my--vterm-name)))))


;; updates `my--vterm-width` variable
(defun my-vterm-update-width ()
  (unless my--vterm-below-p
    (setf my--vterm-width
	  (window-width (get-buffer-window
			 my--vterm-name)))))


;; shows terminal in split-window below ⬄ right
(defun my-vterm-show (&optional switch)
  (let ((buf (get-buffer my--vterm-name)))
    (unless buf
      (save-window-excursion
	(vterm my--vterm-name)))

    (let ((win (get-buffer-window my--vterm-name)))
      (unless win
	(setf win
	      (if my--vterm-below-p
		  (split-window nil (- my--vterm-height) 'below)
		(split-window nil (- my--vterm-width) 'right)))
	(set-window-buffer win my--vterm-name))

      (when switch (select-window win)))))


;; closes terminal window & updates width and height
(defun my-vterm-hide ()
  (when-let* ((win (my-vterm-is-focused)))
    (my-vterm-update-height)
    (my-vterm-update-width)
    (delete-window win)))


;; terminal toggle function -- keybinding C-`
(defun my-vterm-toggle ()
  (interactive)

  (if (my-vterm-is-focused)
      (my-vterm-hide)
    (my-vterm-show t)))


;; moves terminal bottom ⬄ right -- keybinding C-M-`
(defun my-vterm-move ()
  (interactive)

  (my-vterm-show t)
  (my-vterm-hide)

  (setf my--vterm-below-p
	(not my--vterm-below-p))

  (my-vterm-show t))


;; (global-set-key (kbd "C-`") #'my-vterm-toggle)
;; (global-set-key (kbd "C-M-`") #'my-vterm-move)


;; useful function to send command to terminal without switching to it
(defun my-vterm-send (command)
  (my-vterm-show)
  (with-current-buffer my--vterm-name
    (goto-char (point-max))
    (vterm-send-string command t)
    (vterm-send-return)))

