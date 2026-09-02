;;; -*- lexical-binding: t; -*-

(use-package vterm)

(defvar my-vterm-default "*vterm-default*")
(defvar my-vterm-below-p t)
(defvar my-vterm-below-height 15)
(defvar my-vterm-right-width 80)

(defun my-vterm-show (&optional switch)
  (save-window-excursion
    (unless (get-buffer my-vterm-default)
      (vterm)
      (with-current-buffer "*vterm*"
	(rename-buffer my-vterm-default))))

    (let ((win (get-buffer-window my-vterm-default)))
      (unless win
	(setf win
	      (if my-vterm-below-p
		  (split-window nil (- my-vterm-below-height) 'below)
		(split-window nil (- my-vterm-right-width) 'right)))

	(set-window-buffer win my-vterm-default))

      (when switch (select-window win))))


(defun my-vterm-update-height ()
  (when my-vterm-below-p
    (setf my-vterm-below-height
	  (window-height (get-buffer-window
			  my-vterm-default)))))


(defun my-vterm-update-width ()
  (unless my-vterm-below-p
    (setf my-vterm-right-width
	  (window-width (get-buffer-window
			 my-vterm-default)))))


(defun my-vterm-is-focused ()
  (let ((win (get-buffer-window
	      my-vterm-default)))
    (if (eq win (selected-window))
	win
      nil)))


(defun my-vterm-hide ()
  (let ((win (my-vterm-is-focused)))

    (when win    
      (my-vterm-update-height)
      (my-vterm-update-width)
      (delete-window win))))


(defun my-vterm-toggle ()
  (interactive)

  (if (my-vterm-is-focused)
      (my-vterm-hide)
    (my-vterm-show t)))


(defun my-vterm-move ()
  (interactive)

  (let ((win (get-buffer-window my-vterm-default)))

    (when win
      (my-vterm-update-height)
      (my-vterm-update-width)
      (delete-window win))

    (setf my-vterm-below-p
	  (not my-vterm-below-p))

    (my-vterm-show t)))


(global-set-key (kbd "C-`") #'my-vterm-toggle)
(global-set-key (kbd "C-M-`") #'my-vterm-move)


(defun my-vterm-send (command)
  (my-vterm-show)
  (with-current-buffer my-vterm-default
    (goto-char (point-max))
    (vterm-send-string command t)
    (vterm-send-return)))


(provide 'terminal)
