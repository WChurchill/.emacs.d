;;; org.el

(defun bind-org-mode-keys ()
 (local-set-key (kbd "C-c a") 'org-agenda)
 (local-set-key (kbd "C-M-f") 'org-forward-heading-same-level)
 (local-set-key (kbd "C-M-b") 'org-backward-heading-same-level)
 (local-set-key (kbd "C-c C-f") 'org-down-element)
 (local-set-key (kbd "C-c C-b") 'org-up-element)

;;; Don't use arrow keys to move headings around
 (local-set-key (kbd "M-J") 'org-metaleft)
 (local-set-key (kbd "M-:") 'org-metaright)
 (local-set-key (kbd "M-K") 'org-metadown)
 (local-set-key (kbd "M-L") 'org-metaup))

(add-hook 'org-mode-hook 'bind-org-mode-keys)

;;; Custom todo keywords
(setq org-todo-keywords
	  '((sequence "NEXT(n)"  "|" "DONE(d)")
		(sequence "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "|")
		(sequence "|" "CANCELLED(c)")))

;;; Custom tags
(setq org-tag-persistent-alist
	  '(("finances" . ?f)
		("exam" . ?x)
		("homework" .?h)
		("quiz" . ?z)
		(:startgroup . nil)
		("@home" . ?o)
		("@apartment" . ?p)
		(:endgroup . nil)))

;;; Set agenda files to view all todo entries
(setq org-agenda-files
	  '("~/org/main.org"
		"~/school/school.org"))

