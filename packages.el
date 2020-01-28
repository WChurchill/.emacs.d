;;;; packages.el
;;;;
;;;; Collection of package configurations too small to deserve their
;;;; own file

(message "loading packages.el")
;; Easier setup of lisp workspace
(defun lisp-dir ()
  (interactive)
  (find-file "~/lisp"))

(defun l-proj ()
  (interactive)
  (find-file "~/lisp/quicklisp/local-projects"))

;; Find C++ directory easily
(defun c-dir ()
  (interactive)
  (find-file "~/C++"))

(defun select-site ()
  (helm :sources (helm-build-sync-source "sites"
		   :candidates '(("github" . "github.com/search?q=")
				 ("google" . "google.com/search#q=")
				 ("stackoverflow" . "stackoverflow.com/search?q=")
				 ("javadocs" . "google.com/search?q=javadocs+8+")
				 ("cppreference" . "cppreference.com/search.do?q=" )
				 ("python" . "docs.python.org/3/search.html?q=")
				 ("arch-packages" . "archlinux.org/packages?/=")
				 ("arch-wiki" . "wiki.archlinux.org/index.php?title=Special%3ASearch&search=")
				 ("wikipedia" . "en.wikipedia.org/wiki/Special:Search?search="))
		   :fuzzy-match t)
	:buffer "*select website*"))

(defun search-site (search-string)
  (interactive "sSearch: ")
  (browse-url (concat (select-site) search-string)))

(global-set-key (kbd "C-c C-d h") 'search-site)

(defun helm-switch-to-file-buffer ()
  (interactive)
  ())

(defun helm-switch-to-process-buffer ()
  (interactive)
  (helm-occur-mode)
  (helm-buffer-list))

;;; PAREDIT
(require 'paredit)
(defun wrap-progn ()
  (paredit-forward)
  (paredit-backward)
  (paredit-wrap-sexp 1)
  (insert "progn")
  (newline 2)
  (indent-sexp)
  (previous-line 1)
  (indent-for-tab-command 1))

(defun duplicate-sexp (n)
  "Copies the sexp immediately after point and duplicates it n times."
  (interactive "*p")
  (paredit-forward) (paredit-backward) ; bring point immediately before sexp
  (kill-sexp)
  (yank)
  (dotimes (iter n)
    (newline-and-indent)
    (yank)))

(defun duplicate-sexp-inline (n)
  "Copies the sexp immediately after point and duplicates it n times in the same line."
  (interactive "*p")
  (paredit-forward) (paredit-backward) ; bring point immediately before sexp
  (kill-sexp)
  (dotimes (iter n)
    (yank)
    (insert " "))
  (yank))

(defun duplicate-last-sexp (n)
  "Copies the sexp immediately before point and duplicates it n times."
  (interactive "*p")
  (paredit-backward)
  (kill-sexp)
  (yank)
    (dotimes (iter n)
	(newline-and-indent)
	(yank))
  (yank))

(defun duplicate-last-sexp-inline (n)
  "Copies the sexp immediately before point and duplicates it n times in the same line."
  (interactive "*p")
  (paredit-backward)
  (kill-sexp)
  (yank)
  (dotimes (iter n)
    (insert " ")
    (yank)))


(defun bind-paredit-keys ()
  (define-key paredit-mode-map (kbd "C-c C-d d") 'duplicate-sexp)
  (define-key paredit-mode-map (kbd "C-c C-d C-d") 'duplicate-sexp-inline)
  (define-key paredit-mode-map (kbd "C-c C-d D") 'duplicate-last-sexp)
  (define-key paredit-mode-map (kbd "C-c C-d C-S-d") 'duplicate-last-sexp-inline)
  (define-key paredit-mode-map (kbd "C-M-z") 'paredit-wrap-sexp)
  (define-key paredit-mode-map (kbd "C-c C-d p") 'wrap-progn)
  (define-key paredit-mode-map (kbd "M-r") 'paredit-raise-sexp))

(add-hook 'paredit-mode-hook 'bind-paredit-keys)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook       'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)


;; MULTIPLE-CURSORS
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-lines)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c m r") 'mc/mark-all-in-region)
(global-set-key (kbd "C-c m n") 'mc/mark-next-lines)


;;; PYTHON-MODE
(require 'elpy)
(elpy-enable)
(setq python-shell-interpreter "ipython"
	  python-shell-interpreter-args "--simple-prompt -i")
(setq elpy-rpc-virtualenv-path 'current)

(add-hook 'python-mode-hook 'electric-pair-mode)

(defun bind-elpy-keys ()
  (define-key elpy-mode-map (kbd "M-J") 'elpy-nav-indent-shift-left)
  (define-key elpy-mode-map (kbd "M-K") 'elpy-nav-move-line-or-region-down)
  (define-key elpy-mode-map (kbd "M-L") 'elpy-nav-move-line-or-region-up)
  (define-key elpy-mode-map (kbd "M-:") 'elpy-nav-indent-shift-right)
  (define-key elpy-mode-map (kbd "M-n") 'elpy-flymake-next-error)
  (define-key elpy-mode-map (kbd "M-p") 'elpy-flymake-previous-error)
  (define-key elpy-mode-map (kbd "C-c M-.") 'elpy-goto-definition-other-window)
  (define-key elpy-mode-map (kbd "C-S-f") 'elpy-format-code))
(add-hook 'elpy-mode-hook 'bind-elpy-keys)
(remove-hook 'inferior-python-mode-hook 'electric-pair-mode)

;;; MAGIT-MODE
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-x g") 'magit-status)


;;; ORG-MODE
(message "loading my-org")
(load-file "~/.emacs.d/my-org.el")

;;; YASNIPPET
(add-to-list 'load-path
			 "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
(global-set-key (kbd "C-S-y") 'yas-insert-snippet)


;;; WHICH-KEY
(require 'which-key)
(which-key-mode)
(global-set-key (kbd "C-h B") 'which-key-show-major-mode)
(global-set-key (kbd "C-h b") 'which-key-show-minor-mode-keymap)


;;; packages.el ends here
