(load "/home/winston/.emacs.d/packages.el")

;;; JS2-MODE
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;;; MULTI-WEB-MODE
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags 
  '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
    (js2-mode  "<script[^>]*>" "</script>")
    (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;;; MAGIT-MODE
(setq magit-last-seen-setup-instructions "1.4.0")

;;; AUCTEX-MODE
(setq reftex-plug-into-AUCTeX t)

;;; SLIME-MODE
(defun init-slime ()
  (interactive)
  (load "/home/winston/.emacs.d/slime.el")
  (slime))

;;; ORG-MODE
(add-hook 'org-mode-hook
	  (load "org.el"))

;;; MULTI-TERM
(require 'multi-term)
(global-set-key (kbd "C-c M") 'multi-term)
(add-hook 'multi-term-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c m") 'multi-term-next)
	    (local-set-key (kbd "C-c n") 'multi-term-prev)))

;;; ACE-JUMP
(require 'ace-jump-mode)
(global-set-key (kbd "C-;") 'ace-jump-mode)
(global-set-key (kbd "C-M-;") 'ace-jump-char-mode)
(global-set-key (kbd "C-M-'") 'ace-jump-line-mode)

;;; ACE-WINDOW
(global-set-key (kbd "C-:") 'ace-window)

;;; PAREDIT
(defun wrap-progn ()
  (interactive)
  (paredit-forward)
  (paredit-backward)
  (paredit-wrap-sexp 1)
  (insert "progn")
  (newline 2)
  (indent-sexp)
  (previous-line 1)
  (indent-for-tab-command 1))

(add-hook 'paredit-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-M-z") 'paredit-wrap-sexp)
	    (local-set-key (kbd "C-c C-d C-s") 'wrap-progn)))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook       #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;;; ACTIVATE HELM-MODE
(helm-mode)
