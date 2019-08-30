;;;; essentials.el

;;; This file is for initializing packages that are extremely
;;; convenient and would aid in debugging emacs quicker

;;; ACTIVATE HELM-MODE
(require 'helm)
(setq helm-mode-line-string "")
(helm-autoresize-mode t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h r") 'helm-register)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(helm-mode 1)

(defun emd (&optional arg)
  "Convenience function to easily edit things in .emacs.d/"
  (interactive "P")
  (find-file (helm-read-file-name "Find files or url" :initial-input "~/.emacs.d/")))

;;; AVY
(require 'avy)
(global-set-key (kbd "C-;") 'avy-goto-word-1)
(global-set-key (kbd "C-\"") 'avy-goto-char)
;;(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "C-'") 'avy-goto-line)

(setq avy-styles-alist '((avy-goto-char . at)))

;; enable avy-select in isearch mode
(eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "C-;") 'avy-isearch))

;;; I3-EMACS
(load-file "/home/winston/.emacs.d/i3-emacs/i3.el")
(load-file "/home/winston/.emacs.d/i3-emacs/i3-integration.el")
(require 'i3-integration)

(i3-one-window-per-frame-mode-on)

;;; ACE-WINDOW
(global-set-key (kbd "C-:") 'ace-window)
