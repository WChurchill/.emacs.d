;;; org.el
(require 'org)

(add-hook 'org-mode-hook 'bind-org-mode-keys)

;;; Throw error when editing invisible section
(setq org-catch-invisible-edits 'show-and-error)

;;; Make LaTeX previews a larger font
(setq org-format-latex-options
	  (plist-put org-format-latex-options :scale 1.7))

;;; Custom todo keywords
(setq org-todo-keywords
	  '((sequence "NEXT(n)" "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "|")
		(sequence "|" "DONE(d)" "CANCELLED(c)")))

;;; Custom tags
(setq org-tag-persistent-alist
	  '(("finances" . ?f)
		("school" . ?s)
		(:startgroup . nil)
		("EXAM" . ?x)
		("homework" .?h)
		("quiz" . ?z)
		(:endgroup . nil)
		(:startgroup . nil)
		("@house" . ?o)
		("@apartment" . ?p)
		(:endgroup . nil)))

;;; Set agenda files to view all todo entries
(setq org-agenda-files (directory-files "~/org" t ".\.org\$" t))

(setq org-agenda-custom-commands
	  '(("c" "Custom agenda"
		 ((tags "PRIORITY=\"A\""
				((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
				 (org-agenda-overriding-header "High Priority Unfinished Tasks")))
		  (agenda "" ((org-agenda-span 14)
					  (org-deadline-warning-days 0)))))
		("u" "Unscheduled Todos"
		 ((alltodo ""
				   ((org-agenda-skip-function
					 '(org-agenda-skip-entry-if
					   'scheduled
					   'deadline
					   'todo '("SOMEDAY" "WAITING")))
					(org-agenda-overriding-header "Unscheduled tasks:")))))))

(defun bind-org-mode-keys ()
  (local-set-key (kbd "C-c a") 'org-agenda)
  ;;; Don't use arrow keys to move headings around
  (local-set-key (kbd "M-J") 'org-metaleft)
  (local-set-key (kbd "M-:") 'org-metaright)
  (local-set-key (kbd "M-K") 'org-metadown)
  (local-set-key (kbd "M-L") 'org-metaup)
  ;;; Easy promote and demote subtrees
  (local-set-key (kbd "C-c M-J") 'org-promote-subtree)
  (local-set-key (kbd "C-c M-:") 'org-demote-subtree)
  ;; C-' is used for avy-mode
  (local-unset-key (kbd "C-'")))

