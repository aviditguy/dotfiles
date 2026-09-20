;; -*- lexical-binding: t; -*-

(provide 'carousel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CAROUSEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my--carousel-overlays '())
(defvar my--carousel-images '())
(defvar my--carousel-index 0)


(defun my-org-carousel-p  ()
  (string=
   (my-org-src-get :language)
   "carousel"))


(defun my-carousel-create ()
  (interactive)

  (when (my-org-carousel-p)

    (my-carousel-remove)
    
    (let ((beg (my-org-src-get :begin))
	  (end (my-org-src-get :end)))

      (setq my--carousel-images
	    (butlast (split-string
		      (my-org-src-get :value)
		      "\n")))

      (message "%s" my--carousel-images)
      (setq my--carousel-index 0)
      
      (dolist (image my--carousel-images)
	(setq my--carousel-overlays
	      (append my--carousel-overlays
		      (list (make-overlay beg end)))))
      
      (my-carousel-set-image my--carousel-index nil))))


(defun my-carousel-remove ()
  (interactive)

  (when my--carousel-overlays
    (dolist (overlay my--carousel-overlays)
      (when (overlayp overlay)
	(delete-overlay overlay)))
    
    (setq my--carousel-overlays '())))


(defun my-carousel-set-image (idx pidx)
  (when my--carousel-overlays
    (let* ((overlay  (nth idx my--carousel-overlays))
	   (img-path (nth idx my--carousel-images))
	   (img-name (file-name-nondirectory img-path))
	   (total    (length my--carousel-images))
	   (count    (1+ idx)))

	(overlay-put overlay 'display
		     (create-image img-path nil nil :width 400))

	(when pidx
	  (overlay-put
	   (nth pidx my--carousel-overlays)
	   'display nil)

	  (overlay-put
	   (nth pidx my--carousel-overlays)
	   'after-string nil))

	(overlay-put
	 overlay
	 'after-string
	 (format "\n%s  (%d/%d)\n\n"
		 img-name count total)))))


(defun my-carousel-next ()
  (interactive)

  (when (and my--carousel-overlays
	     (my-org-carousel-p))
    
    (let ((pidx my--carousel-index))

      (setf my--carousel-index
	    (mod
	     (1+ my--carousel-index)
	     (length my--carousel-images)))

      (when
	  (not (= my--carousel-index pidx))
	(my-carousel-set-image my--carousel-index pidx)))))


(defun my-carousel-previous ()
  (interactive)

  (when (and my--carousel-overlays
	     (my-org-carousel-p))
    
    (let ((pidx my--carousel-index))

      (setf my--carousel-index
	    (mod
	     (1- my--carousel-index)
	     (length my--carousel-images)))

      (when
	  (not (= my--carousel-index pidx))
	(my-carousel-set-image my--carousel-index pidx)))))



(defun my-carousel-toggle ()
  (interactive)
  (if my--carousel-overlays
      (my-carousel-remove)
    (my-carousel-create)))

;; (defvar my--carousel-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "C-c C-c") #'my-carousel-toggle)
;;     (define-key map (kbd "<right>") #'my-carousel-next)
;;     (define-key map (kbd "<left>")  #'my-carousel-previous)
;;     map))

;; (define-minor-mode my-carousel-mode
;;   "Keymap for navigating carousel blocks."
;;   :lighter nil
;;   :keymap my--carousel-mode-map)

;; (defun my-carousel-activate-map ()
;;   (my-carousel-mode
;;    (if (my-org-carousel-p) 1 -1)))


;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (add-hook 'post-command-hook
;;                       #'my-carousel-activate-map
;;                       nil
;;                       t)))

