;;; filemode.el

;; set .m file extension to invoke matlab-mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))

;; file extensions for the Gazebo robot simulator, which uses xml format 
(add-to-list 'auto-mode-alist '("\\.world\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.launch\\'" . nxml-mode))

;; Open ".cu" cuda files in c++ mode
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
