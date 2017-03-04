;;; filemode.el

;; open PKGBUILD files in shell-script-mode
(add-to-list 'auto-mode-alist '("PKGBUILD" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.install\\'" . shell-script-mode))

;; set .m file extension to invoke matlab-mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))

;; file extensions for the Gazebo robot simulator, which uses xml format 
(add-to-list 'auto-mode-alist '("\\.world\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.launch\\'" . nxml-mode))

;; Open cuda files in c++ mode
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
;; Set mode for header files to c++-mode rather than c-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; I use prolog more often than perl
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
;; open s(ASP) files in prolog mode
(add-to-list 'auto-mode-alist '("\\.lp\\'" . prolog-mode))
