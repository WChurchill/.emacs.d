;; Joins Lines into one (from the bottom up)
(global-set-key (kbd "M-j") '(lambda () (interactive) (join-line -1)))
;;from top down
(global-set-key (kbd "M-6") 'join-line)

;; Kill line from the left
(global-set-key (kbd "<s-backspace>") '(lambda () (interactive) (kill-line 0)))

;; Backward-kill-sexp
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)

;; Replace List-buffers with Ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;Quicker Keybinding Writing
(defun new-keybinding ()
  "Inserts the string \"(global-set-key (kbd\"\") ') and places point in between the quotes."
  (interactive)
  (insert "(global-set-key (kbd \"\") ')")
  (backward-char 5))

(add-hook 'emacs-lisp-mode-hook
	  (lambda () (local-set-key (kbd "C-c C-k") 'new-keybinding)))

;; Copy and Kill region
(global-set-key (kbd "C-w") 'copy-region-as-kill)
(global-set-key (kbd "C-x w") 'kill-region)

;; Better Window management
(global-set-key (kbd "M-s-b") 'shrink-window-horizontally)
(global-set-key (kbd "M-s-f") 'enlarge-window-horizontally)
(global-set-key (kbd "M-s-n") 'shrink-window)
(global-set-key (kbd "M-s-p") 'enlarge-window)

(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-horizontally)
(global-set-key (kbd "M-3") 'split-window-vertically)
(global-set-key (kbd "M-4") 'kill-buffer-and-window)
(global-set-key (kbd "M-=") 'balance-windows)

;; Killing buffers and Windows
(defun kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (buffer-name)))

(global-set-key (kbd "M--") 'kill-current-buffer)

;; Horizontal Scrolling
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(global-set-key (kbd "C-<") 'scroll-left)
(global-set-key (kbd "C->") 'scroll-right)
