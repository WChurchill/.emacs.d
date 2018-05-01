;;;; init.el

;; Don't run regular expressions for every .el and .elc file
(let ((file-name-handler-alist nil)
      (default-cons-threshold gc-cons-threshold)
	  (temp-cons-threshold 100000000))
  ;; Increase number of bytes before garbage collection
  (setq gc-cons-threshold temp-cons-threshold)

  ;; must call before package configuration
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  
  (load "/home/winston/.emacs.d/noexternals.el")
  (load "/home/winston/.emacs.d/keybindings.el")
  (load "/home/winston/.emacs.d/essentials.el")
  (load "/home/winston/.emacs.d/themes/my-euphoria-theme.el")
  (load "/home/winston/.emacs.d/packages.el")
  
  (setq gc-cons-threshold default-cons-threshold))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("6b3da91c8d9b6921eeb7d87da7e6e642b76db9753fb730bf95470209336cdc4a" default)))
 '(org-export-backends (quote (ascii beamer html icalendar latex md odt)))
 '(package-selected-packages
   (quote
	(magit which-key flycheck-ledger ledger-mode pdf-tools interleave ecb jedi helm-company elpy helm-gtags helm-bibtex scad-mode helm-cscope py-autopep8 py-yapf helm-flycheck electric-spacing php-mode csharp-mode typescript-mode exec-path-from-shell cmake-mode swiper smart-mode-line-powerline-theme python-mode paredit multi-web-mode multiple-cursors multi-term matlab-mode lua-mode highlight-quoted highlight-numbers helm-projectile ggtags emacs-eclim company-cmake company-c-headers company-auctex cmake-font-lock autopair ace-window ac-js2))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
