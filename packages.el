;;;; packages.el
;;;;
;;;; Collection of package configurations too small to deserve their
;;;; own file
(setq debug-on-error t)

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

;;; HIGHLIGHT-NUMBERS-MODE
(highlight-numbers-mode 1)
;;; HIGHLIGHT-QUOTED-MODE
(highlight-quoted-mode 1)

;;; PAREDIT
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


;;; COMPANY
(require 'company)
(setq company-dabbrev-downcase nil)
(add-hook 'after-init-hook 'global-company-mode)
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
;; (require 'eclim)
;; (require 'eclimd)
;; (require 'company-eclim)

;; (add-hook 'after-init-hook 'global-eclim-mode)
;; ;;(company-emacs-eclim-setup)
;; (setq help-at-pt-display-when-idle t)
;; (setq help-at-pt-timer-delay 0.1)
;; (help-at-pt-set-timer)

;; (defun bind-eclim-keys ()
;;   (local-set-key (kbd "C-c b") 'eclim-project-build)
;;   (local-set-key (kbd "C-c r") 'eclim-run-class)
;;   (local-set-key (kbd "C-c l") 'eclim-problems))

;; (add-hook 'eclim-mode-hook 'bind-eclim-keys)

(defun compile-ctf ()
  (interactive)
  (switch-to-buffer
   (get-buffer-create "*CTF compilation*"))
  (shell-command
   "cd ~/school/16/spring/artificial_intelligence/project; javac -cp . ctf/agent/*.java"))

(defun bind-compile-ctf ()
  (local-set-key (kbd "C-c b") 'compile-ctf))

(add-hook 'java-mode-hook 'bind-compile-ctf)


;;; MULTIPLE-CURSORS
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-lines)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c m r") 'mc/mark-all-in-region)
(global-set-key (kbd "C-c m n") 'mc/mark-next-lines)


;;; PYTHON-MODE
(elpy-enable)
(setq python-shell-interpreter "ipython"
	  python-shell-interpreter-args "--simple-prompt -i"
	  elpy-rpc-backend "rope")

(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(elpy-use-ipython)
(require 'py-autopep8)
;(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(require 'py-yapf)
;(add-hook 'python-mode-hook 'py-yapf-enable-on-save)
(add-hook 'python-mode-hook 'electric-pair-mode)
(defun format-then-save ()
  (interactive)
  (elpy-format-code)
  (save-buffer))
(defun bind-elpy-keys ()
  (define-key elpy-mode-map (kbd "M-J") 'elpy-nav-indent-shift-left)
  (define-key elpy-mode-map (kbd "M-K") 'elpy-nav-move-line-or-region-down)
  (define-key elpy-mode-map (kbd "M-L") 'elpy-nav-move-line-or-region-up)
  (define-key elpy-mode-map (kbd "M-:") 'elpy-nav-indent-shift-right)
  (define-key elpy-mode-map (kbd "M-n") 'compilation-next-error)
  (define-key elpy-mode-map (kbd "M-p") 'compilation-previous-error)
  (define-key elpy-mode-map (kbd "C-c M-.") 'elpy-goto-definition-other-window)
  (define-key elpy-mode-map (kbd "C-c f") 'elpy-format-code)
  (define-key elpy-mode-map (kbd "C-x C-s") 'format-then-save))
(add-hook 'elpy-mode-hook 'bind-elpy-keys)
(remove-hook 'inferior-python-mode-hook 'electric-pair-mode)

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
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'c-mode-common-hook 'c-toggle-electric-state)
(add-hook 'c-mode-common-hook 'c-toggle-auto-newline)

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
;;(add-hook 'elpy-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(defun save-and-update-gtags ()
  (interactive)
  (save-buffer)
  (helm-gtags-update-tags))

(defun bind-helm-gtags-keys ()
  (define-key helm-gtags-mode-map (kbd "C-t") 'transpose-chars)
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


;;; MULTI-WEB-MODE
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
		  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
		  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)


;;; MAGIT-MODE
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-x g") 'magit-status)
;; required for ssh to work
(require 'exec-path-from-shell)
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")



;;; AUCTEX-MODE
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'linum-mode)
;; ;;not sure what this one does
(setq reftex-plug-into-AUCTeX t)
(setq reftex-cite-format; Get ReTeX with biblatex, see http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992
           '((?t . "\\textcite[]{%l}")
             (?a . "\\autocite[]{%l}")
             (?c . "\\cite[]{%l}")
             (?s . "\\smartcite[]{%l}")
             (?f . "\\footcite[]{%l}")
             (?n . "\\nocite{%l}")
             (?b . "\\blockcquote[]{%l}{}")))
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
  ;;(LaTeX-add-electric-pairs)
  ;;(define-key 'LaTeX-mode-map "\$" 'electric-pair)
  (local-set-key (kbd "C-c b") 'latex-insert-block)
  (local-set-key (kbd "C-c C-c") 'save-and-compile-latex)
  (local-set-key (kbd "$") 'self-insert-command)) ; Add an extra "$" when I type $

(add-hook 'LaTeX-mode-hook 'bind-LaTeX-keys)

;;; REFTEX
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;;; SLIME-MODE
(defun init-slime ()
  (interactive)
  (load-file "~/.emacs.d/slime.el")
  (slime))


;;; ORG-MODE
(load-file "~/.emacs.d/my-org.el")


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


;;; MATLAB
;; custom workspace configuration for project
(add-hook 'matlab-mode-hook 'linum-mode)


;;; SECRETARIA



;;; SMERGE
(setq smerge-command-prefix (kbd "C-c v"))
