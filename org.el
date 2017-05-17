;;; org.el
(require 'org)

;;; Custom clock reporting
(defun my-clock-report ()
  (interactive)
  (set-buffer (generate-new-buffer "*my-clock-report*"))
  (insert
   (org-clock-get-clocktable
	:scope 'agenda-with-archives
	:maxlevel 2
	:tstart "<-5w>"
	:tend "<+1w>"
	:wstart 6
	:step 'week
	:fileskip0 t))
  (switch-to-buffer-other-window "*my-clock-report*"))

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
		("@computer" . ?c)
		("@house" . ?o)
		("@apartment" . ?p)
		("@location" . ?l)
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


(define-key org-mode-map (kbd "C-c a") 'org-agenda)
  ;;; Don't use arrow keys to move headings around
(define-key org-mode-map (kbd "M-J") 'org-metaleft)
(define-key org-mode-map (kbd "M-:") 'org-metaright)
(define-key org-mode-map (kbd "M-K") 'org-metadown)
(define-key org-mode-map (kbd "M-L") 'org-metaup)
  ;;; Easy promote and demote subtrees
(define-key org-mode-map (kbd "C-c M-J") 'org-promote-subtree)
(define-key org-mode-map (kbd "C-c M-:") 'org-demote-subtree)
;; C-' is used for avy-mode
(define-key org-mode-map (kbd "C-'") 'avy-goto-line)

