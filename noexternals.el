;;;; noexternals.el

;; Startup workspace
(defun find-my-files ()
  (let ((orgfile  "~/org/main.org")
	(lispfile "~/lisp/quicklisp/local-projects" ))
  (if (file-exists-p orgfile)
      (find-file orgfile))
  (when (file-exists-p lispfile)
    (split-window-horizontally)
    (find-file lispfile))))

(find-my-files)

;; Easier editing of .emacs.d/
(defun em-dir ()
  (interactive)
  (find-file "~/.emacs.d"))

;; Font
(setq line-spacing 0)
;(set-face-font 'default "-*-terminus-medium-r-*-*-*-140-75-75-*-*-iso8859-15")

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

