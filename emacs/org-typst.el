;; -*- lexical-binding: t; -*-

(provide 'org-typst)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TYPST SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my--typst-header
  "#set page(
  width: auto,
  height: auto,
  margin: 6pt,
  fill: none
)

#import \"@preview/cetz:0.5.1\"
#import cetz.draw: circle, line, rect, content, on-layer, set-style

#set text(size: %spt, fill: rgb(\"%s\"), top-edge: \"ascender\", bottom-edge: \"descender\")
#set math.mat(column-gap: 1em)

%s
")


(defvar my--typst-cetz-header
  "#set page(
  width: auto,
  height: auto,
  margin: 6pt,
  fill: none
)
#set text(size: %spt, fill: rgb(\"%s\"), top-edge: \"ascender\", bottom-edge: \"descender\")
#set math.mat(column-gap: 1em)

#import \"@preview/cetz:0.5.1\"
#cetz.canvas({
  import cetz.draw: *
  set-style(
    stroke: rgb(\"%s\"),
  )

  %s
})
")

(defvar my--typst-directory "~/.typstimg/")
(defvar my--typst-preview-buffer "*my-typst-preview*")
(defvar my--typst-font-size 16)

(defun my-typst-fg-color ()
  (apply #'format "#%02x%02x%02x"
         (mapcar (lambda (c) (/ c 257))
                 (color-values (face-foreground 'default)))))

(defun my-typst-get-svg-name ()
  (let ((data (my-typst-format-data)))
    (expand-file-name
     (concat my--typst-directory
	     "org-typstimg_"
	     (substring (secure-hash 'sha256 data) 0 40)
	     ".svg"))))

(defun my-typst-format-data ()
  (if (string= (my-org-src-get :language) "typst")
      (format my--typst-header
	      my--typst-font-size
	      (my-typst-fg-color)
	      (my-org-src-get :value))
    (format my--typst-cetz-header
	    my--typst-font-size
	    (my-typst-fg-color)
	    (my-typst-fg-color)
	    (my-org-src-get :value))))
	 

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


(defun my-org-typst-preview (&optional display-in-buffer)
  (interactive)

  (when (and (my-org-in-src-block-p '("typst" "cetz"))
	     (not (string= (my-org-src-get :value) "")))

    (let ((svg  (my-typst-get-svg-name))
	  (data (my-typst-format-data)))
	  
      (make-directory my--typst-directory t)
      (my-typst-compile data svg)

      (if display-in-buffer
	  (let ((buffer (get-buffer-create my--typst-preview-buffer)))
	    (with-current-buffer buffer
	      (erase-buffer)
	      (insert "\n\n")
	      (insert "    ")
	      (insert-image (create-image svg 'svg))
	      (goto-char (point-max)))
	    (display-buffer buffer))
	
	(let ((ov (seq-find (lambda (o) (overlay-get o 'my-typst))
			    (overlays-at (point)))))
	  (if ov
	      (delete-overlay ov)
	    (let ((ov (make-overlay (my-org-src-get :begin)
				    (1+ (my-org-src-get :end)))))
	      (overlay-put ov 'my-typst t)
	      (overlay-put ov 'face 'default)
	      (overlay-put ov 'display (create-image svg nil nil))
	      (overlay-put ov 'after-string "\n"))))))))

(defun my-org-typst-preview-toggle ()
  (when (my-org-in-src-block-p '("typst" "cetz"))
    (my-org-typst-preview)
    t))


(add-hook 'org-ctrl-c-ctrl-c-hook #'my-org-typst-preview-toggle)

(define-key org-mode-map
	    (kbd "C-c C-p")
	    (lambda ()
	      (interactive)
	      (my-org-typst-preview t)))
