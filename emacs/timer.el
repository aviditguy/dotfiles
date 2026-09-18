;; -*- lexical-binding: t; -*-

(provide 'timer)

;; ============================================================
;; CORE
;; ============================================================

(defvar my--clock-buffer "*My Clock*")
(defvar my--clock-timer nil)
(defvar my--clock-marker nil)


(defun my-clock-start ()
  (interactive)

  (my-clock-stop)
  
  (let ((buffer (get-buffer-create my--clock-buffer)))
    (with-current-buffer buffer
      (erase-buffer)

      (setq my--clock-marker
	    (point-marker))

      (insert "00:00:00"))

    (display-buffer buffer)

    (setq my--clock-timer
	  (run-at-time t 1 #'my-clock-update))))

(defun my-clock-update ()
  (when (buffer-live-p
	 (get-buffer my--clock-buffer))
    
    (with-current-buffer my--clock-buffer
      (save-excursion
	(goto-char my--clock-marker)

	(delete-region (point) (+ (point) 8))

	(insert (format-time-string "%H:%M:%S"))

	(add-text-properties
	 (point-min)
	 (point-max)
	 '(face (:height 2.0 :weight bold)))))))


(defun my-clock-stop ()
  (interactive)

  (when (timerp my--clock-timer)
    (cancel-timer my--clock-timer)
    (setq my--clock-timer nil)))


