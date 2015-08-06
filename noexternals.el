;; Startup workspace
(defun find-my-files ()
  (find-file "/home/winston/main.org")
  (split-window-horizontally)
  (find-file "/home/winston/lisp/quicklisp/local-projects/machine-learning/linreg.lisp"))

(find-my-files)

;; Font
(setq line-spacing 0)
(set-face-font 'default "-*-terminus-medium-r-*-*-*-140-75-75-*-*-iso8859-15")

;; Line width and word wrapping
(auto-fill-mode)
(setq fill-column 80)
(setq-default truncate-lines t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Don't tell me what the scratch buffer is for.
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

;; Don’t defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; Joins Lines into one (from the bottom up)
(global-set-key (kbd "M-j") '(lambda () (interactive) (join-line -1)))
;;from top down
(global-set-key (kbd "M-6") 'join-line)

;; Kill line from the left
(global-set-key (kbd "<s-backspace>") '(lambda () (interactive) (kill-line 0)))

;; Start show-paren-mode
(show-paren-mode 1)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Don't feel like typing whole words
(defalias 'yes-or-no-p 'y-or-n-p)

;; When on tab, make cursor tab length
(setq-default x-stretch-cursor t)

;; Sudo-Edit
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Start fullscreen
(toggle-frame-maximized)
;(toggle-frame-fullscreen)

;; Cursor Blinking
(setq blink-cursor-blinks 9999)

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

;; Remove scrollbars, menubars, startup screen, and toolbar
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-screen t)

;; Wind-move
;(global-set-key (kbd "C-c C-b") 'windmove-left)
;(global-set-key (kbd "C-c C-n") 'windmove-down)
;(global-set-key (kbd "C-c C-p") 'windmove-up)
;(global-set-key (kbd "C-c C-f") 'windmove-right)

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
