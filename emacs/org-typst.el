;; -*- lexical-binding: t; -*-

(provide 'org-typst)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TYPST SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun my-typst-fg-color ()
;;   (apply #'format "#%02x%02x%02x"
;;          (mapcar (lambda (c) (/ c 257))
;;                  (color-values (face-foreground 'default)))))

;; (defvar my--typst-directory "~/.typstimg/")

;; (defvar my--typst-header
;;   "#set page(
;;   width: auto,
;;   height: auto,
;;   margin: 0pt,
;;   fill: none
;; )
;; #set text(size: %spt, fill: rgb(\"%s\"), top-edge: \"ascender\", bottom-edge: \"descender\")

;; %s
;; ")


;; (defvar my--typst-cetz-header
;;   "#set page(
;;   width: auto,
;;   height: auto,
;;   margin: 0pt,
;;   fill: none
;; )
;; #set text(size: %spt, fill: rgb(\"%s\"), top-edge: \"ascender\", bottom-edge: \"descender\")

;; #import \"@preview/cetz:0.5.1\"
;; #cetz.canvas({
;;   import cetz.draw: *
;;   %s
;; })
;; ")


;; (defvar my--typst-font-size 16)

;; (defun my-org-in-typst-block-p ()
;;   (and (org-in-src-block-p)
;;        (member (my-org-src-get :language) '("typst" "cetz"))))


;; (defun my-id (data &optional limit)
;;   (setq limit (or limit 40))
;;   (substring (secure-hash 'sha256 data) 0 limit))

;; (defun my-typst-create-svg ()
;;   (let ((lang (my-org-src-get :language)))

;;     (when (my-org-in-typst-block-p)
;;       (make-directory my--typst-directory t)

;;       (let* ((header (if (string= lang "typst")
;; 			 my--typst-header
;; 		       my--typst-cetz-header))
;; 	     (data (format header
;; 			   my--typst-font-size
;; 			   (my-typst-fg-color)
;; 			   (my-org-src-get :value)))
;; 	     (output (expand-file-name
;; 		      (concat my--typst-directory
;; 			      "org-typstimg_"
;; 			      (my-id data)
;; 			    ".svg"))))

;; 	(unless (file-exists-p output)
;; 	  (with-temp-buffer
;; 	    (insert data)

;; 	    (call-process-region
;; 	     (point-min)
;; 	     (point-max)
;; 	     "typst"
;; 	     nil
;; 	     "*typst-output*"
;; 	     nil
;; 	     "compile"
;; 	     "--format"
;; 	     "svg"
;; 	     "-"
;; 	     output)))

;; 	output))))


;; (defun my-org-src-get-end ()
;;   (when (org-in-src-block-p)
;;     (save-excursion
;;       (beginning-of-line)
;;       (re-search-forward "^[ \t]*#\\+end_src"
;; 			 nil t)
;;       (point))))


;; (defun my-org-typst-preview ()
;;   (interactive)

;;   (when (my-org-in-typst-block-p)
;;     (let* ((beg (my-org-src-get :begin))
;; 	   (end (1+ (my-org-src-get-end)))
;; 	   (svg (my-typst-create-svg))
;; 	   (ov  (make-overlay beg end)))

;;       (overlay-put ov 'face 'default)
;;       (overlay-put ov 'display (create-image svg nil nil))
;;       (overlay-put ov 'after-string "\n"))))


;; (defun my-org-typst-toggle ()
;;   (interactive)
;;   (when (my-org-in-typst-block-p)
;;     (let ((ov (seq-find (lambda (o) (overlay-get o 'my-typst))
;; 			(overlays-at (point)))))
;;       (if ov
;; 	  (delete-overlay ov)
;; 	(let* ((beg (my-org-src-get :begin))
;; 	       (end (1+ (my-org-src-get-end)))
;; 	       (svg (my-typst-create-svg))
;; 	       (ov  (make-overlay beg end)))
;; 	  (overlay-put ov 'my-typst t)
;; 	  (overlay-put ov 'face 'default)
;; 	  (overlay-put ov 'display (create-image svg nil nil))
;; 	  (overlay-put ov 'after-string "\n"))))))


(defvar my--typst-directory "~/.typstimg/")

(defvar my--typst-header
  "#set page(
  width: auto,
  height: auto,
  margin: 0pt,
  fill: none
)
#set text(size: %spt, fill: rgb(\"%s\"), top-edge: \"ascender\", bottom-edge: \"descender\")

%s
")


(defvar my--typst-cetz-header
  "#set page(
  width: auto,
  height: auto,
  margin: 0pt,
  fill: none
)
#set text(size: %spt, fill: rgb(\"%s\"), top-edge: \"ascender\", bottom-edge: \"descender\")

#import \"@preview/cetz:0.5.1\"
#cetz.canvas({
  import cetz.draw: *
  %s
})
")

(defvar my--typst-font-size 16)


(defun my-typst-fg-color ()
  (apply #'format "#%02x%02x%02x"
         (mapcar (lambda (c) (/ c 257))
                 (color-values (face-foreground 'default)))))


(defun my-typst-compile (data path)
  (unless (file-exists-p path)
    (with-temp-buffer
      (insert data)
      
      (call-process-region
       (point-min)
       (point-max)
       "typst"
       nil
       "*typst-output*"
       nil
       "compile"
       "--format"
       "svg"
       "-"
       path))))
  
(defun my-org-typst-preview ()
  (interactive)

  (when (my-org-in-src-block-p '("typst" "cetz"))
    (let* ((header (if (string= (my-org-src-get :language) "typst")
		       my--typst-header
		     my--typst-cetz-header))
	   (data (string-trim
		  (format header
			  my--typst-font-size
			  (my-typst-fg-color)
			  (my-org-src-get :value))))
	   (svgpath (expand-file-name
		     (concat my--typst-directory
			     "org-typstimg_"
			     (substring (secure-hash 'sha256 data) 0 40)
			     ".svg"))))

      (unless (string= (string-trim (my-org-src-get :value)) "")
	
	(make-directory my--typst-directory t)
	(my-typst-compile data svgpath)

	(let ((ov (seq-find (lambda (o) (overlay-get o 'my-typst))
			    (overlays-at (point)))))
	  (if ov
	      (delete-overlay ov)
	    (let* ((beg (my-org-src-get :begin))
		   (end (1+ (my-org-src-get :end)))
		   (ov  (make-overlay beg end)))
	      (overlay-put ov 'my-typst t)
	      (overlay-put ov 'face 'default)
	      (overlay-put ov 'display (create-image svgpath nil nil))
	      (overlay-put ov 'after-string "\n"))))))))

(defun my-org-typst-preview-toggle ()
  (when (my-org-in-src-block-p '("typst" "cetz"))
    (my-org-typst-preview)
    t))


(add-hook 'org-ctrl-c-ctrl-c-hook #'my-org-typst-preview-toggle)
