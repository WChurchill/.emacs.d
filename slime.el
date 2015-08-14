;;;; slime.el

;;; Set Inferior Lisp
(load (expand-file-name "~/lisp/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;;; Local Copy of the CL Hyperspec
(setq common-lisp-hyperspec-root "file:/home/winston/lisp/CLHS7/HyperSpec/")

;;; Custom Keybindings
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil)
  (define-key slime-repl-mode-map
    (kbd "M-DEL") nil))

(add-hook 'slime-repl-mode-hook
	  (lambda ()
	    (enable-paredit-mode)
	    (override-slime-repl-bindings-with-paredit)
	    (local-set-key (kbd "C-M-z") 'paredit-wrap-sexp)
	    (local-set-key (kbd "C-c q") 'load-slime-package-files)))

(add-hook 'lisp-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-,")       'slime-selector)
	    (local-set-key (kbd "<tab>")     'slime-indent-and-complete-symbol)
	    (local-set-key (kbd "C-c C-v 0") 'slime-remove-balanced-comments)
	    (local-set-key (kbd "C-c C-v 9") 'slime-insert-balanced-comments)
	    (local-set-key (kbd "C-.")       'slime-compile-and-load-file)
	    (local-set-key (kbd "C-'")       'slime-compile-file)
	    (local-set-key (kbd "C-c C-d l") 'slime-show-compilation-log)))


