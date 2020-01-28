;;; init --- Load all other emacs configuration

;;; Commentary:
;;; Load Emacs configuration as fast and as reliably as possible.

;;; Code:

(message "init!")
;; Don't run regular expressions for every .el and .elc file
(let ((file-name-handler-alist nil)
      (default-cons-threshold gc-cons-threshold)
	  (temp-cons-threshold 100000000))
  (message "increasing cons threshold")
  ;; Increase number of bytes before garbage collection
  (setq gc-cons-threshold temp-cons-threshold)

  (message "initializing packages")
  ;; must call before package configuration
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)

  (message "loading custom files")
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
 '(org-agenda-files
   (quote
	("/home/winston/org/inbox.org" "/home/winston/org/main.org" "/home/winston/org/finance.org" "/home/winston/org/expenses.org" "/home/winston/org/school.org" "/home/winston/org/moving.org" "/home/winston/org/personal.org" "/home/winston/org/papers.org" "/home/winston/org/chatbot_workshop.org" "/home/winston/org/offer.org" "/home/winston/org/gradschool.org" "/home/winston/org/muse.org" "/home/winston/org/edds.org")))
 '(org-export-backends (quote (ascii beamer html icalendar latex md odt)))
 '(package-selected-packages
   (quote
	(which-key multiple-cursors yasnippet-snippets magit yasnippet elpy helm ace-window))))

;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
