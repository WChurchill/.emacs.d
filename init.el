;;;; init.el

;; Don't run regular expressions for every .el and .elc file
(let ((file-name-handler-alist nil))
  ;; Increase number of bytes before garbage collection
  (setq gc-cons-threshold 100000000)
  
  (load "/home/winston/.emacs.d/themes/euphoria-theme.el")
  (load "/home/winston/.emacs.d/noexternals.el")
  (load "/home/winston/.emacs.d/keybindings.el")
  (load "/home/winston/.emacs.d/packages.el")

  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(org-agenda-files (quote ("~/org/cal.org"))))
  (setq gc-cons-threshold 800000))
