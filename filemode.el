;;; filemode.el

;; set .m file extension to invoke octave-mode
;;(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; file extensions for the Gazebo robot simulator, which uses xml format 
(add-to-list 'auto-mode-alist '("\\.world\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.launch\\'" . nxml-mode))
