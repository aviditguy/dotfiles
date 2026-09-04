;;; -*- lexical-binding: t; -*-


(defun my-org-todo-toggle ()
  (interactive)

  (save-excursion

    ;; jump to heading
    (unless (org-at-heading-p)
      (re-search-backward "^[ \t]*\\*+ " nil t))

    (let ((state (org-get-todo-state)))
	
      (cond

       ;; TODO -> DONE
       ((string= state "TODO")
	(org-todo "DONE")
	(re-search-forward "^[ \t]*:CLOSED:" nil t)
	(insert (format " [%s]"
			(format-time-string "%Y-%m-%d %a %H:%M"))))

       ;; Remove TODO [Change normal heading to TODO]
       ((string= state "DONE")
	(org-todo "")
	(re-search-forward "^[ \t]*:PROPERTIES:" nil t)
	(beginning-of-line)
	(kill-line 4))

       ;; Create TODO [Change normal heading to TODO]
       (t
	(org-todo "TODO")
	(end-of-line)
	(insert "\n :PROPERTIES:\n")
	(insert (format " :CREATED: [%s]\n"
			(format-time-string "%Y-%m-%d %a %H:%M")))
	(insert (format " :CLOSED:\n"))
	(insert  " :END:"))))))


(with-eval-after-load 'org
  (define-key org-mode-map
	      (kbd "C-c C-t")
	      #'my-org-todo-toggle)

  (define-key org-mode-map
	      (kbd "S-<right>")
	      #'my-org-todo-toggle))


(provide 'org_todo)
