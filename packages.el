;;;; packages.el

(load "~/.emacs.d/loadpackages.el")

;;; ACTIVATE HELM-MODE
(helm-mode)
(setq helm-mode-line-string "")

;;; AVY
(require 'avy)
(global-set-key (kbd "C-;") 'avy-goto-word-1)
(global-set-key (kbd "C-\"") 'avy-goto-char)
(global-set-key (kbd "M-g f") 'avy-goto-line)

(setq avy-styles-list '((avy-goto-char . at)))

;;; ACE-WINDOW
(global-set-key (kbd "C-:") 'ace-window)

;;; HIGHLIGHT-NUMBERS-MODE
(highlight-numbers-mode)
;;; HIGHLIGHT-QUOTED-MODE
(highlight-quoted-mode)

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

(defun duplicate-sexp (n)
  "Copies the sexp immediately after point and duplicates it n times."
  (interactive "*p")
  (paredit-forward) (paredit-backward)
  (kill-sexp)
  (dotimes (iter n)
    (yank)
    (newline-and-indent))
  (yank))

(defun duplicate-sexp-inline (n)
  "Copies the sexp immediately after point and duplicates it n times in the same line."
  (interactive "*p")
  (paredit-forward) (paredit-backward)
  (kill-sexp)
  (dotimes (iter (1+ n))
    (yank)
    (insert " ")))

(add-hook 'paredit-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-d d") 'duplicate-sexp)
	    (local-set-key (kbd "C-c C-d C-d") 'duplicate-sexp-inline)
	    (local-set-key (kbd "C-M-z") 'paredit-wrap-sexp)
	    (local-set-key (kbd "C-c C-d p") 'wrap-progn)))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook       #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;;; EMACS-ECLIM
(require 'eclim)
(require 'eclimd)
(global-eclim-mode)
(require 'company)


(global-company-mode t)
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
(add-hook 'eclim-mode-hook
	  (lambda ()
	    (require 'company-emacs-eclim)
	    (company-emacs-eclim-setup)
	    (local-set-key (kbd "C-c b") 'eclim-project-build)
	    (local-set-key (kbd "C-c r") 'eclim-run-class)
	    (local-set-key (kbd "C-c l") 'eclim-problems)))

;;; MULTIPLE-CURSORS
(require 'multiple-cursors)
;(global-set-key (kbd "M-<space>") 'mc-)
(global-set-key (kbd "M-s-c M-s-c") 'mc/edit-lines)

;;; PYTHON-MODE
(package-initialize)
(elpy-enable)
(elpy-use-ipython)

;;; C++-Mode
;(require 'company-clang)
(require 'company-cmake)

(defun interactive-compile ()
  (interactive)
  (setq-local compilation-read-command nil)
  (call-interactively 'compile))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode)
	      (local-set-key
	       (kbd "<f5>")
	       'interactive-compile))))

;;; GGTAGS-MODE
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
	      (ggtags-mode))))

(add-hook 'ggtags-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c g s") 'ggtags-find-other-symbol)
	    (local-set-key (kbd "C-c g h") 'ggtags-view-tag-history)
	    (local-set-key (kbd "C-c g r") 'ggtags-find-reference)
	    (local-set-key (kbd "C-c g f") 'ggtags-find-file)
	    (local-set-key (kbd "C-c g c") 'ggtags-create-tags)
	    (local-set-key (kbd "C-c g u") 'ggtags-update-tags)
	    
	    (local-set-key (kbd "M-,") 'pop-tag-mark)))


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
(defun mgs ();;magit-status shortcut
  (interactive)
  (magit-status))

;;; AUCTEX-MODE
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; ;;not sure what this one does
;; (setq reftex-plug-into-AUCTeX t)

(add-hook 'latex-mode-hook
	  (lambda ()
	    (electric-pair-mode)
	    (local-set-key (kbd "C-c b") 'latex-insert-block)))

;;; SLIME-MODE
(defun init-slime ()
  (interactive)
  (load "~/.emacs.d/slime.el")
  (slime))

;;; ORG-MODE
(require 'org)
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c a") 'org-agenda)
	    (local-set-key (kbd "C-M-f") 'org-forward-heading-same-level)
	    (local-set-key (kbd "C-M-b") 'org-backward-heading-same-level)
	    (local-set-key (kbd "C-c C-f") 'org-down-element)
	    (local-set-key (kbd "C-c C-b") 'org-up-element)))
;(setq org-agenda-files (quote "~/org/cal.org"))

;;; MULTI-TERM
(require 'multi-term)
(global-set-key (kbd "C-c M") 'multi-term)
(setq multi-term-program "/bin/bash")
(add-hook 'multi-term-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c m") 'multi-term-next)
	    (local-set-key (kbd "C-c n") 'multi-term-prev)))


;;; Smart-Mode-Line
;(setq sml/theme 'powerline)
                                        ;(sml/setup)


;;; File extensions
(load "~/.emacs.d/filemode.el")

