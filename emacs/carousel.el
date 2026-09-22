;; ;; -*- lexical-binding: t; -*-

;; (provide 'carousel)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; CAROUSEL
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar my--carousel-overlays '())
;; (defvar my--carousel-images '())
;; (defvar my--carousel-index 0)


;; (defun my-org-carousel-p  ()
;;   (string=
;;    (my-org-src-get :language)
;;    "carousel"))


;; (defun my-carousel-create ()
;;   (interactive)

;;   (when (my-org-carousel-p)

;;     (my-carousel-remove)
    
;;     (let ((beg (my-org-src-get :begin))
;; 	  (end (1+ (my-org-src-get :end))))

;;       (setq my--carousel-images
;; 	    (split-string
;; 	     (my-org-src-get :value)
;; 	     "\n"))

;;       (message "%s" my--carousel-images)
;;       (setq my--carousel-index 0)
      
;;       (dolist (image my--carousel-images)
;; 	(setq my--carousel-overlays
;; 	      (append my--carousel-overlays
;; 		      (list (make-overlay beg end)))))
      
;;       (my-carousel-set-image my--carousel-index nil))))


;; (defun my-carousel-remove ()
;;   (interactive)

;;   (when my--carousel-overlays
;;     (dolist (overlay my--carousel-overlays)
;;       (when (overlayp overlay)
;; 	(delete-overlay overlay)))
    
;;     (setq my--carousel-overlays '())))


;; (defun my-carousel-set-image (idx pidx)
;;   (when my--carousel-overlays
;;     (let* ((overlay  (nth idx my--carousel-overlays))
;; 	   (img-path (nth idx my--carousel-images))
;; 	   (img-name (file-name-nondirectory img-path))
;; 	   (total    (length my--carousel-images))
;; 	   (count    (1+ idx))
;; 	   (is-svg   (string-match-p "\\.svg\\'" img-path)))

;;       (overlay-put overlay 'display
;; 		   (if is-svg
;; 		       (create-image img-path nil nil)
;; 		     (create-image img-path nil nil :width 400)))

;;       (when pidx
;; 	(overlay-put
;; 	 (nth pidx my--carousel-overlays)
;; 	 'display nil)

;; 	(overlay-put
;; 	 (nth pidx my--carousel-overlays)
;; 	 'after-string nil))

;;       (overlay-put
;;        overlay
;;        'after-string
;;        (format "\n%s  (%d/%d)\n\n"
;; 	       img-name count total)))))


;; (defun my-carousel-next ()
;;   (interactive)

;;   (when (and my--carousel-overlays
;; 	     (my-org-carousel-p))
    
;;     (let ((pidx my--carousel-index))

;;       (setf my--carousel-index
;; 	    (mod
;; 	     (1+ my--carousel-index)
;; 	     (length my--carousel-images)))

;;       (when
;; 	  (not (= my--carousel-index pidx))
;; 	(my-carousel-set-image my--carousel-index pidx)))))


;; (defun my-carousel-previous ()
;;   (interactive)

;;   (when (and my--carousel-overlays
;; 	     (my-org-carousel-p))
    
;;     (let ((pidx my--carousel-index))

;;       (setf my--carousel-index
;; 	    (mod
;; 	     (1- my--carousel-index)
;; 	     (length my--carousel-images)))

;;       (when
;; 	  (not (= my--carousel-index pidx))
;; 	(my-carousel-set-image my--carousel-index pidx)))))



;; (defun my-carousel-toggle ()
;;   (interactive)
;;   (if my--carousel-overlays
;;       (my-carousel-remove)
;;     (my-carousel-create)))

;; ;; (defvar my--carousel-mode-map
;; ;;   (let ((map (make-sparse-keymap)))
;; ;;     (define-key map (kbd "C-c C-c") #'my-carousel-toggle)
;; ;;     (define-key map (kbd "<right>") #'my-carousel-next)
;; ;;     (define-key map (kbd "<left>")  #'my-carousel-previous)
;; ;;     map))

;; ;; (define-minor-mode my-carousel-mode
;; ;;   "Keymap for navigating carousel blocks."
;; ;;   :lighter nil
;; ;;   :keymap my--carousel-mode-map)

;; ;; (defun my-carousel-activate-map ()
;; ;;   (my-carousel-mode
;; ;;    (if (my-org-carousel-p) 1 -1)))


;; ;; (add-hook 'org-mode-hook
;; ;;           (lambda ()
;; ;;             (add-hook 'post-command-hook
;; ;;                       #'my-carousel-activate-map
;; ;;                       nil
;; ;;                       t)))



;; -*- lexical-binding: t; -*-

;; (provide 'carousel)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; SHARED HELPER
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun my/create-image-smart (file-path &optional width)
;;   "FILE-PATH ki image banao, theme background se match karte hue.
;; SVG ho toh natural size (WIDTH ignore), warna WIDTH (default 400)."
;;   (let ((bg (face-background 'default nil t)))
;;     (if (string-match-p "\\.svg\\'" file-path)
;; 	(create-image file-path 'svg nil :background bg)
;;       (create-image file-path nil nil :width (or width 400) :background bg))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; CAROUSEL
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar my--carousel-overlays '())
;; (defvar my--carousel-images '())
;; (defvar my--carousel-index 0)


;; (defun my-org-carousel-p  ()
;;   (string=
;;    (my-org-src-get :language)
;;    "carousel"))


;; (defun my-carousel-create ()
;;   (interactive)

;;   (when (my-org-carousel-p)

;;     (my-carousel-remove)

;;     (let ((beg (my-org-src-get :begin))
;; 	  (end (1+ (my-org-src-get :end))))

;;       (setq my--carousel-images
;; 	    (split-string
;; 	     (my-org-src-get :value)
;; 	     "\n"))

;;       (setq my--carousel-index 0)

;;       (dolist (image my--carousel-images)
;; 	(setq my--carousel-overlays
;; 	      (append my--carousel-overlays
;; 		      (list (make-overlay beg end)))))

;;       (my-carousel-set-image my--carousel-index nil))))


;; (defun my-carousel-remove ()
;;   (interactive)

;;   (when my--carousel-overlays
;;     (dolist (overlay my--carousel-overlays)
;;       (when (overlayp overlay)
;; 	(delete-overlay overlay)))

;;     (setq my--carousel-overlays '())))


;; (defun my-carousel-set-image (idx pidx)
;;   (when my--carousel-overlays
;;     (let* ((overlay  (nth idx my--carousel-overlays))
;; 	   (img-path (nth idx my--carousel-images))
;; 	   (img-name (file-name-nondirectory img-path))
;; 	   (total    (length my--carousel-images))
;; 	   (count    (1+ idx)))

;;       (overlay-put overlay 'display
;; 		   (my/create-image-smart img-path 400))

;;       (when pidx
;; 	(overlay-put
;; 	 (nth pidx my--carousel-overlays)
;; 	 'display nil)

;; 	(overlay-put
;; 	 (nth pidx my--carousel-overlays)
;; 	 'after-string nil))

;;       (overlay-put
;;        overlay
;;        'after-string
;;        (format "\n%s  (%d/%d)\n\n"
;; 	       img-name count total)))))


;; (defun my-carousel-next ()
;;   (interactive)

;;   (when (and my--carousel-overlays
;; 	     (my-org-carousel-p))

;;     (let ((pidx my--carousel-index))

;;       (setf my--carousel-index
;; 	    (mod
;; 	     (1+ my--carousel-index)
;; 	     (length my--carousel-images)))

;;       (when
;; 	  (not (= my--carousel-index pidx))
;; 	(my-carousel-set-image my--carousel-index pidx)))))


;; (defun my-carousel-previous ()
;;   (interactive)

;;   (when (and my--carousel-overlays
;; 	     (my-org-carousel-p))

;;     (let ((pidx my--carousel-index))

;;       (setf my--carousel-index
;; 	    (mod
;; 	     (1- my--carousel-index)
;; 	     (length my--carousel-images)))

;;       (when
;; 	  (not (= my--carousel-index pidx))
;; 	(my-carousel-set-image my--carousel-index pidx)))))


;; (defun my-carousel-toggle ()
;;   (interactive)
;;   (if my--carousel-overlays
;;       (my-carousel-remove)
;;     (my-carousel-create)))


;; (defun my-carousel-toggle-hook ()
;;   "`C-c C-c' se carousel block pe toggle karne ke liye hook."
;;   (when (my-org-carousel-p)
;;     (my-carousel-toggle)
;;     t))

;; (add-hook 'org-ctrl-c-ctrl-c-hook #'my-carousel-toggle-hook)


;; -*- lexical-binding: t; -*-

(provide 'carousel)


(defun my/create-image-smart (file-path &optional width)
  "FILE-PATH ki image banao, theme background se match karte hue.
SVG ho toh natural size (WIDTH ignore), warna WIDTH (default 400)."
  (let ((bg (face-background 'default nil t)))
    (if (string-match-p "\\.svg\\'" file-path)
	(create-image file-path 'svg nil :background bg)
      (create-image file-path nil nil :width (or width 400) :background bg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CAROUSEL (multi-instance, ek buffer me multiple blocks independently)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my--carousel-instances '()
  "Alist: (block-begin-pos . plist(:overlays :images :index)).")

(defface my-carousel-label-face
  '((t :height 0.8 :inherit shadow))
  "Carousel image label (filename aur count) ke liye chhoti font.")

(defun my-org-carousel-p ()
  (string=
   (my-org-src-get :language)
   "carousel"))


(defun my--carousel-instance-at (beg)
  "BEG (block start) ke liye stored instance data lao."
  (cdr (assoc beg my--carousel-instances)))


(defun my--carousel-set-instance (beg data)
  "BEG ke liye instance DATA store/update karo."
  (let ((cell (assoc beg my--carousel-instances)))
    (if cell
	(setcdr cell data)
      (push (cons beg data) my--carousel-instances))))


(defun my--carousel-remove-instance (beg)
  "BEG ka instance hata do alist se."
  (setq my--carousel-instances
	(assoc-delete-all beg my--carousel-instances)))


(defun my-carousel-create ()
  (interactive)

  (when (my-org-carousel-p)

    (let* ((beg (my-org-src-get :begin))
	   (end (1+ (my-org-src-get :end))))

      ;; agar is block ka carousel already khula hai, pehle usse band karo
      (when (my--carousel-instance-at beg)
	(my-carousel-remove beg))

      (let* ((images (split-string (my-org-src-get :value) "\n"))
	     (overlays (mapcar (lambda (_) (make-overlay beg end)) images))
	     (data (list :overlays overlays :images images :index 0)))

	(my--carousel-set-instance beg data)
	(my-carousel-set-image beg 0 nil)))))


(defun my-carousel-remove (&optional beg)
  (interactive)

  (let* ((beg (or beg (my-org-src-get :begin)))
	 (data (my--carousel-instance-at beg)))

    (when data
      (dolist (overlay (plist-get data :overlays))
	(when (overlayp overlay)
	  (delete-overlay overlay)))

      (my--carousel-remove-instance beg))))


(defun my-carousel-set-image (beg idx pidx)
  (let ((data (my--carousel-instance-at beg)))
    (when data
      (let* ((overlays (plist-get data :overlays))
	     (images   (plist-get data :images))
	     (overlay  (nth idx overlays))
	     (img-path (nth idx images))
	     (img-name (file-name-nondirectory img-path))
	     (total    (length images))
	     (count    (1+ idx)))

	(overlay-put overlay 'display
		     (my/create-image-smart img-path 400))

	(when pidx
	  (overlay-put (nth pidx overlays) 'display nil)
	  (overlay-put (nth pidx overlays) 'after-string nil))

	(overlay-put
	 overlay
	 'after-string
	 (propertize
	  (format "\n%s  (%d/%d)\n\n"
		  img-name count total)
	  'face 'my-carousel-label-face))

	(plist-put data :index idx)
	(my--carousel-set-instance beg data)))))


(defun my-carousel-next ()
  (interactive)

  (when (my-org-carousel-p)
    (let* ((beg (my-org-src-get :begin))
	   (data (my--carousel-instance-at beg)))

      (when data
	(let* ((pidx (plist-get data :index))
	       (total (length (plist-get data :images)))
	       (idx (mod (1+ pidx) total)))

	  (when (not (= idx pidx))
	    (my-carousel-set-image beg idx pidx)))))))


(defun my-carousel-previous ()
  (interactive)

  (when (my-org-carousel-p)
    (let* ((beg (my-org-src-get :begin))
	   (data (my--carousel-instance-at beg)))

      (when data
	(let* ((pidx (plist-get data :index))
	       (total (length (plist-get data :images)))
	       (idx (mod (1- pidx) total)))

	  (when (not (= idx pidx))
	    (my-carousel-set-image beg idx pidx)))))))


(defun my-carousel-toggle ()
  (interactive)

  (when (my-org-carousel-p)
    (let ((beg (my-org-src-get :begin)))
      (if (my--carousel-instance-at beg)
	  (my-carousel-remove beg)
	(my-carousel-create)))))


(defun my-carousel-toggle-hook ()
  "`C-c C-c' se carousel block pe toggle karne ke liye hook."
  (when (my-org-carousel-p)
    (my-carousel-toggle)
    t))

(add-hook 'org-ctrl-c-ctrl-c-hook #'my-carousel-toggle-hook)
