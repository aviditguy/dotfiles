;; -*- lexical-binding: t; -*-

(provide 'org-typst)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TYPST SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-typst-fg-color ()
  (apply #'format "#%02x%02x%02x"
         (mapcar (lambda (c) (/ c 257))
                 (color-values (face-foreground 'default)))))

(defvar my--typst-directory "~/.typstimg/")

(defvar my--typst-header
  "#set page(
  width: auto,
  height: auto,
  margin: 0pt,
  fill: none
)
#set text(size: %spt, fill: rgb(\"%s\"), top-edge: \"ascender\", bottom-edge: \"descender\")
")

(defvar my--typst-font-size 16)


(defun my-id (data &optional limit)
  (setq limit (or limit 40))
  (substring (secure-hash 'sha256 data) 0 limit))

(defun my-typst-create-svg ()
  (when (and (org-in-src-block-p)
	     (string= (my-org-src-get :language)
		      "typst"))

    (make-directory my--typst-directory t)
    
    (let* ((data (concat (format my--typst-header
				 my--typst-font-size
				 (my-typst-fg-color))
			 (my-org-src-get :value)))
	   (output (expand-file-name
		    (concat my--typst-directory
			    "org-typstimg_"
			    (my-id data)
			    ".svg"))))

      (unless (file-exists-p output)
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
	   output)))

      output)))


(defun my-org-src-get-end ()
  (when (org-in-src-block-p)
    (save-excursion
      (beginning-of-line)
      (re-search-forward "^[ \t]*#\\+end_src"
			 nil t)
      (point))))


(defun my-org-typst-preview ()
  (interactive)

  (when (and (org-in-src-block-p)
	     (string= (my-org-src-get :language)
		      "typst"))

   

    (let* ((beg (my-org-src-get :begin))
	   (end (1+ (my-org-src-get-end)))
	   (svg (my-typst-create-svg))
	   (ov  (make-overlay beg end)))

      (overlay-put ov 'face 'default)
      (overlay-put ov 'display (create-image svg nil nil))
      (overlay-put ov 'after-string "\n"))))


(defun my-org-typst-toggle ()
  (interactive)
  (when (and (org-in-src-block-p)
	     (string= (my-org-src-get :language) "typst"))
    (let ((ov (seq-find (lambda (o) (overlay-get o 'my-typst))
			(overlays-at (point)))))
      (if ov
	  (delete-overlay ov)
	(let* ((beg (my-org-src-get :begin))
	       (end (1+ (my-org-src-get-end)))
	       (svg (my-typst-create-svg))
	       (ov  (make-overlay beg end)))
	  (overlay-put ov 'my-typst t)
	  (overlay-put ov 'face 'default)
	  (overlay-put ov 'display (create-image svg nil nil))
	  (overlay-put ov 'after-string "\n"))))))
