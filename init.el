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
	("6b3da91c8d9b6921eeb7d87da7e6e642b76db9753fb730bf95470209336cdc4a" default)))
 '(org-agenda-files
   (quote
	("/home/winston/org/jarvis.org" "/home/winston/org/main.org" "/home/winston/org/senior_design.org" "/home/winston/org/school.org" "/home/winston/org/personal.org" "/home/winston/org/fitbeat.org" "/home/winston/org/chatbot_workshop.org" "/home/winston/org/edds.org")))
 '(package-selected-packages
   (quote
	(helm-flycheck electric-spacing php-mode csharp-mode typescript-mode exec-path-from-shell cmake-mode swiper smart-mode-line-powerline-theme python-mode paredit multi-web-mode multi-term matlab-mode magit lua-mode jedi highlight-quoted highlight-numbers helm-projectile ggtags emacs-eclim elpy company-cmake company-c-headers company-auctex cmake-font-lock autopair arduino-mode android-mode ace-window ac-js2))))

;; Don't run regular expressions for every .el and .elc file
(let ((file-name-handler-alist nil)
      (default-cons-threshold gc-cons-threshold)
	  (temp-cons-threshold 100000000))
  ;; Increase number of bytes before garbage collection
  (setq gc-cons-threshold temp-cons-threshold)

  
  (load "/home/winston/.emacs.d/noexternals.el")
  (load "/home/winston/.emacs.d/keybindings.el")
  (load "/home/winston/.emacs.d/themes/my-euphoria-theme.el")
  (load "/home/winston/.emacs.d/packages.el")
  
  
  ;;(setq gc-cons-threshold 800000)
  (setq gc-cons-threshold default-cons-threshold))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
