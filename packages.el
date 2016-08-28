;;;; packages.el
;(setq debug-on-error t)
(load "~/.emacs.d/loadpackages.el")

;;; ACTIVATE HELM-MODE
(require 'helm)
(setq helm-mode-line-string "")
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files) 
(helm-mode 1)

(defun select-site ()
  (helm :sources (helm-build-sync-source "sites"
		   :candidates '(("github" . "github.com/search?q=")
				 ("google" . "google.com/search#q=")
				 ("stackoverflow" . "stackoverflow.com/search?q=")
				 ("javadocs" . "google.com/search?q=javadocs+8+")
				 ("c++" . "cplusplus.com/search.do?q=" )
				 ("python" . "docs.python.org/3/search.html?q=")
				 ("arch-packages" . "archlinux.org/packages?/=")
				 ("arch-wiki" . "wiki.archlinux.org/index.php?title=Special%3ASearch&search=")
				 ("wikipedia" . "en.wikipedia.org/wiki/Special:Search?search="))
		   :fuzzy-match t)
	:buffer "*select website*"))

(defun search-site (search-string)
  (interactive "MSearch: ")
  (browse-url (concatenate 'string (select-site) search-string)))

(global-set-key (kbd "C-c C-d h") 'search-site)

(defun helm-switch-to-file-buffer ()
  (interactive)
  ())

(defun helm-switch-to-process-buffer ()
  (interactive)
  (helm-moccur-mode)
  (helm-buffer-list))



;;; AVY
(require 'avy)
(global-set-key (kbd "C-;") 'avy-goto-word-1)
(global-set-key (kbd "C-\"") 'avy-goto-char)
;;(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "C-'") 'avy-goto-line)

(setq avy-styles-list '((avy-goto-char . at)))

;; enable avy-select in isearch mode
(eval-after-load "isearch"
  (define-key isearch-mode-map (kbd "C-;") 'avy-isearch))


;;; ACE-WINDOW
(global-set-key (kbd "C-:") 'ace-window)


;;; HIGHLIGHT-NUMBERS-MODE
(highlight-numbers-mode 1) 
;;; HIGHLIGHT-QUOTED-MODE
(highlight-quoted-mode 1)


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

(defun bind-paredit-keys ()
  (local-set-key (kbd "C-c C-d d") 'duplicate-sexp)
  (local-set-key (kbd "C-c C-d C-d") 'duplicate-sexp-inline)
  (local-set-key (kbd "C-M-z") 'paredit-wrap-sexp)
  (local-set-key (kbd "C-c C-d p") 'wrap-progn)
  (local-set-key (kbd "M-r") 'paredit-raise-sexp))

(add-hook 'paredit-mode-hook 'bind-paredit-keys)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook       #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)


;;; COMPANY
(require 'company)
(global-company-mode t)
(global-set-key (kbd "C-<tab>") 'company-complete)


;;; JAVA-MODE
;(require 'flymake)
;(add-hook 'java-mode-hook 'flymake-mode-on)
(defun javac-all ()
  (interactive)
  (setq-local compilation-read-command nil)
  (call-interactively (shell-command "javac *.java")))

(defun bind-javac-all ()
  (local-set-key (kbd "<f5>") 'javac-all))

(add-hook 'java-mode-hook 'bind-javac-all)


;;; EMACS-ECLIM
(require 'eclim)
(require 'eclimd)
(require 'company-emacs-eclim)

(global-eclim-mode)
(company-emacs-eclim-setup)
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(defun bind-eclim-keys ()
  (local-set-key (kbd "C-c b") 'eclim-project-build)
  (local-set-key (kbd "C-c r") 'eclim-run-class)
  (local-set-key (kbd "C-c l") 'eclim-problems))

(add-hook 'eclim-mode-hook 'bind-eclim-keys)

(defun compile-ctf ()
  (interactive)
  (let (b (get-buffer-create "*CTF compilation*"))
    (set-buffer b)
    (shell-command
     "cd ~/school/artificial_intelligence/project; javac -cp . ctf/agent/*.java"))

  (defun bind-compile-ctf ()
    (local-set-key (kbd "C-c b") 'compile-ctf)))

(add-hook 'java-mode-hook 'bind-compile-ctf)


;;; MULTIPLE-CURSORS
(require 'multiple-cursors)
;(global-set-key (kbd "M-<space>") 'mc-)
(global-set-key (kbd "M-s-c M-s-c") 'mc/edit-lines)


;;; PYTHON-MODE
(package-initialize)
(elpy-enable)
(setq python-shell-interpreter "ipython"
    python-shell-interpreter-args "--simple-prompt -i")
;(elpy-use-ipython)
(defun bind-python-keys ()
  )

(add-hook 'python-mode-hook 'bind-python-keys)
(add-hook 'python-mode-hook 'electric-pair-mode)


;;; C++-Mode
;(require 'company-clang)
(require 'company-cmake)

(defun interactive-compile ()
  (interactive)
  (setq-local compilation-read-command nil)
  (call-interactively 'compile))

(defun bind-interactive-compile ()
  (when (derived-mode-p 'c-mode 'c++-mode)
    (local-set-key
     (kbd "<f5>")
     'interactive-compile)))

(add-hook 'c-mode-common-hook 'bind-interactive-compile)

(setq
 ;; Use gdb-many-windows
 gdb-many-windows t
 ;; Start off with the file that has main
 gdb-show-main t)


;;; HELM-GTAG
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t)

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(defun save-and-update-gtags ()
  (interactive)
  (save-buffer)
  (helm-gtags-update-tags))

(defun bind-helm-gtags-keys ()
  (local-set-key (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (local-set-key (kbd "C-x C-s") 'save-and-update-gtags)
  (local-set-key (kbd "C-j") 'helm-gtags-select)
  (local-set-key (kbd "M-.") 'helm-gtags-dwim)
  (local-set-key (kbd "M-,") 'helm-gtags-pop-stack)
  (local-set-key (kbd "C-c <") 'helm-gtags-previous-history)
  (local-set-key (kbd "C-c >") 'helm-gtags-next-history))

(add-hook 'helm-gtags-mode-hook 'bind-helm-gtags-keys)


;;; JS2-MODE
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
;; My own janky java compile keybinding


;;; MULTI-WEB-MODE
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
		  ;; (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
		  (js-mode "<script>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
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
(defun save-and-compile-latex ()
  (interactive)
  (save-buffer)
  (TeX-command-master))

(defvar LaTeX-electric-pairs '((\$ . \$)) "Electric pairs for LaTeX-mode.")
(defun LaTeX-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs LaTeX-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(defun bind-LaTeX-keys ()
  (electric-pair-mode)
  (LaTeX-add-electric-pairs)
  ;;(define-key 'LaTeX-mode-map "\$" 'electric-pair)
  (local-set-key (kbd "C-c b") 'latex-insert-block)
  (local-set-key (kbd "C-c C-c") 'save-and-compile-latex)
  (local-set-key (kbd "$") 'self-insert-command)) ; Add an extra "$" when I type $

(add-hook 'LaTeX-mode-hook 'bind-LaTeX-keys)


;;; SLIME-MODE
(defun init-slime ()
  (interactive)
  (load "~/.emacs.d/slime.el")
  (slime))


;;; ORG-MODE
(require 'org)

(defun bind-org-mode-keys ()
  (local-set-key (kbd "C-c a") 'org-agenda)
  (local-set-key (kbd "C-M-f") 'org-forward-heading-same-level)
  (local-set-key (kbd "C-M-b") 'org-backward-heading-same-level)
  (local-set-key (kbd "C-c C-f") 'org-down-element)
  (local-set-key (kbd "C-c C-b") 'org-up-element))

(add-hook 'org-mode-hook
	  'bind-org-mode-keys)
;(setq org-agenda-files (quote "~/org/cal.org"))


;;; MULTI-TERM
(require 'multi-term)
(global-set-key (kbd "C-c M") 'multi-term)
(setq multi-term-program "/bin/bash")

(defun bind-multi-term-keys ()
  (local-set-key (kbd "C-c m") 'multi-term-next)
  (local-set-key (kbd "C-c n") 'multi-term-prev))

(add-hook 'multi-term-mode-hook 'bind-multi-term-keys)

;;; Smart-Mode-Line
;(setq sml/theme 'powerline)
;(sml/setup)


;;; File extensions
(load "~/.emacs.d/filemode.el")


;;; YASNIPPET
(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
(global-set-key (kbd "C-c y") 'yas-insert-snippet)


;;; PROJECTILE
(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)

;;; HELM-PROJECTILE
(require 'helm-projectile)
(helm-projectile-on)
