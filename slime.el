;;;; slime.el

;;; Set Inferior Lisp
(load (expand-file-name "~/lisp/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy slime-banner))

;;; Local Copy of the CL Hyperspec
(setq common-lisp-hyperspec-root "file:/home/winston/lisp/CLHS7/HyperSpec/")

;;; Custom Keybindings
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil)
  (define-key slime-repl-mode-map
    (kbd "M-DEL") nil))

(defun bind-repl-keys ()
  (enable-paredit-mode)
  (override-slime-repl-bindings-with-paredit)
  ;;(local-set-key (kbd "C-M-z") 'paredit-wrap-sexp)
  (local-set-key (kbd "M-DEL") 'paredit-backward-kill-word)
  (local-set-key (kbd "C-c q") 'load-slime-package-files))

(add-hook 'slime-repl-mode-hook 'bind-repl-keys)

(define-key lisp-mode-map (kbd "C-,")       'slime-selector)
(define-key lisp-mode-map (kbd "<tab>")     'slime-indent-and-complete-symbol)
(define-key lisp-mode-map (kbd "C-c C-;") 'slime-insert-balanced-comments)
(define-key lisp-mode-map (kbd "C-c C-:") 'slime-remove-balanced-comments)
(define-key lisp-mode-map (kbd "C-c C-d 9") 'comment-region)
(define-key lisp-mode-map (kbd "C-c C-d 0") 'uncomment-region)
(define-key lisp-mode-map (kbd "C-c l")       'slime-compile-and-load-file)
(define-key lisp-mode-map (kbd "C-.")       'slime-compile-file)
(define-key lisp-mode-map (kbd "C-c C-d l") 'slime-show-compilation-log)

