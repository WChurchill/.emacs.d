;;; org.el
(require 'org)

;;; Set agenda files to view all todo entries
(setq org-agenda-files (directory-files "~/org" t ".\.org\$" t))

;;; Org refile config
(setq org-refile-targets '((org-agenda-files . (:level . 1))))

;;; Org-Capture config
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(define-key global-map (kbd "C-c c") 'org-capture)

(setq org-capture-templates
	  '(("n" "Next" entry (file+headline "~/org/inbox.org" "Tasks")
		 "* NEXT %?\n %i")))

;;; Custom clock reporting
(defun make-report-buffer (buffer-name clocktable-args)
  (set-buffer (generate-new-buffer buffer-name))
  (insert
   `(org-clock-get-clocktable
	 ,clocktable-args))
  (switch-to-buffer-other-window buffer-name)
  (read-only-mode 1)
  (local-set-key (kbd "M-n") 'forward-paragraph)
  (local-set-key (kbd "M-p") 'backward-paragraph))

(defun my-weekly-report ()
  (interactive)
  (let ((buffer-name "*weekly-review*"))
	(when (get-buffer buffer-name)
	  (kill-buffer buffer-name))
	(set-buffer (generate-new-buffer buffer-name))
	(insert
	 (org-clock-get-clocktable
	  :scope 'agenda-with-archives
	  :maxlevel 3
	  :tstart "<-8w>"
	  :tend "<+1w>"
	  :wstart 0
	  :step 'week
	  :fileskip0 t))
	(switch-to-buffer-other-window buffer-name))
  (let ((buffer-name "*daily-review*"))
	(when (get-buffer buffer-name)
	  (kill-buffer buffer-name))
	(set-buffer (generate-new-buffer buffer-name))
	(insert
	 (org-clock-get-clocktable
	  :scope 'agenda-with-archives
	  :maxlevel 3
	  :tstart "<-14d>"
	  :tend "<+1d>"
	  :step 'day
	  :fileskip0 t))
	(switch-to-buffer-other-window buffer-name)))

;;; Show characters as UTF-8
(setq org-pretty-entities t)

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

