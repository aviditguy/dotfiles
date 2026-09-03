;;; -*- lexical-binding: t; -*-

(defvar my--carousel-overlay nil)
(defvar my--carousel-images  nil)
(defvar my--carousel-index   0)


(defun my-org-src-block-get (key)
  (let ((el (org-element-context)))
    (when (eq (org-element-type el) 'src-block)
      (org-element-property key el))))


(defun my-carousel-set-image ()
  (when (overlayp my--carousel-overlay)
    (let* ((img-path (nth my--carousel-index my--carousel-images))
	   (img-name (file-name-nondirectory (expand-file-name img-path)))
	   (total    (length my--carousel-images))
	   (count    (1+ my--carousel-index)))

      (overlay-put
       my--carousel-overlay
       'display
       (create-image img-path nil nil :width 400))

      (overlay-put
       my--carousel-overlay
       'after-string
       (format "\n%s  (%d/%d)\n\n" img-name count total)))))


(defun my-carousel-create ()
  (interactive)
  
  (when
      (string=
       (my-org-src-block-get :language)
       "carousel")

    (my-carousel-remove)

    (let ((beg  (my-org-src-block-get :begin))
	  (end  (my-org-src-block-get :end)))

      (setf my--carousel-images
	    (split-string
	     (my-org-src-block-get :value)
	     "\n" t))

      (setf my--carousel-index 0)

      (setf my--carousel-overlay
	    (make-overlay beg end))

      (my-carousel-set-image))))
      
	 
(defun my-carousel-remove ()
  (interactive)

  (when (overlayp my--carousel-overlay)
    (delete-overlay my--carousel-overlay)
    (setf my--carousel-overlay nil)))


(defun my-carousel-next ()
  (interactive)

  (when (overlayp my--carousel-overlay)
    (setf my--carousel-index
	  (mod
	   (1+ my--carousel-index)
	   (length my--carousel-images)))

    (my-carousel-set-image)))


(defun my-carousel-previous ()
  (interactive)

  (when (overlayp my--carousel-overlay)
    (setf my--carousel-index
          (mod
           (1- my--carousel-index)
           (length my--carousel-images)))

    (my-carousel-set-image)))


(define-key org-mode-map (kbd "C-M-c") #'my-carousel-create)
(define-key org-mode-map (kbd "C-M-r") #'my-carousel-remove)
(define-key org-mode-map (kbd "C-M-n") #'my-carousel-next)
(define-key org-mode-map (kbd "C-M-p") #'my-carousel-previous)

(provide 'carousel)
