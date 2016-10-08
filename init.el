;;;; init.el


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(org-agenda-files (quote ("~/org/main.org"))))


;; Don't run regular expressions for every .el and .elc file
(let ((file-name-handler-alist nil))
  ;; Increase number of bytes before garbage collection
  (setq gc-cons-threshold 100000000)
  
  (load "/home/winston/.emacs.d/themes/my-euphoria-theme.el")
  (load "/home/winston/.emacs.d/noexternals.el")
  (load "/home/winston/.emacs.d/keybindings.el")
  (load "/home/winston/.emacs.d/packages.el")

  
  (setq gc-cons-threshold 800000))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
