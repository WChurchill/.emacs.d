;;;; keybindings.el

;; Smart open-line and open-line-above
(defun smart-open-line (n)
  "Insert a newline and leave point before it.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
	 (do-left-margin (and (bolp) (> (current-left-margin) 0)))
	 (loc (point-marker))
	 ;; Don't expand an abbrev before point.
	 (abbrev-mode nil))
    (dotimes (iter n)
      (newline)
      (indent-according-to-mode))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
	     (if do-left-margin (indent-to (current-left-margin)))
	     (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)))

(defun smart-open-line-above (n)
  "Insert a newline above point and move point to end of current line.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
	 (do-left-margin (and (bolp) (> (current-left-margin) 0)))
	 (loc (point-marker))
	 ;;Don't expand an abbrev before point.
	 (abbrev-mode nil))
    (back-to-indentation)
    (dotimes (iter n)
      (newline-and-indent))
    (forward-line (- n))
    (indent-according-to-mode)
    (while (> n 0)
      (cond ((bolp)
	     (if do-left-margin (indent-to (current-left-margin)))
	     (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    ;(goto-char loc)
    (end-of-line)))

(global-set-key (kbd "C-o") 'smart-open-line)
(global-set-key (kbd "C-S-o") 'smart-open-line-above)

;; Joins Lines into one (from the bottom up)
(defun join-line-down (&optional n)
  (interactive)
  (if n
	  (join-line (- n))
	(join-line -1)))
(global-set-key (kbd "M-j") 'join-line-down)
;;from top down
(global-set-key (kbd "M-6") 'join-line)

;; Kill line from the left
(defun backward-kill-line ()
  (interactive)
  (kill-line 0))

(global-set-key (kbd "<s-backspace>") 'backward-kill-line)

;; Backward-kill-sexp
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)

;; Replace List-buffers with Ibuffer
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b b") 'ibuffer)

;;Quicker Keybinding Writing
(defun new-global-key ()
  "Inserts the string \"(global-set-key (kbd\"\") ') and places point in between the quotes."
  (interactive)
  (insert "(global-set-key (kbd \"\") ')")
  (backward-char 5))
(defun new-local-key ()
  "Inserts the string \"(local-set-key (kbd\"\") ') and places point in between the quotes."
  (interactive)
  (insert "(local-set-key (kbd \"\") ')")
  (backward-char 5))

(defun bind-my-elisp-keys ()
  (local-set-key (kbd "C-c C-k") 'new-global-key)
  (local-set-key (kbd "C-c C-l") 'new-local-key)
  (local-set-key (kbd "C-c e") 'eval-buffer))

(add-hook 'emacs-lisp-mode-hook 'bind-my-elisp-keys)

;; replace-regexp
(global-set-key (kbd "C-x C-q") 'replace-regexp)

;; Better Window management
(global-set-key (kbd "M-B") 'shrink-window-horizontally)
(global-set-key (kbd "M-F") 'enlarge-window-horizontally)
(global-set-key (kbd "M-N") 'shrink-window)
(global-set-key (kbd "M-P") 'enlarge-window)

(defun save-all-delete-frame ()
  (interactive)
  (save-some-buffers)
  (delete-frame))

(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-horizontally)
(global-set-key (kbd "M-3") 'split-window-vertically)
(global-set-key (kbd "M-4") 'kill-buffer-and-window)
(global-set-key (kbd "M-8") 'save-all-delete-frame)
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
(global-set-key (kbd "C-<") 'scroll-right)
(global-set-key (kbd "C->") 'scroll-left)

;; Toggle Menu Bar
(global-set-key (kbd "C-c C-x t") 'toggle-menu-bar-mode-from-frame)

;; Comment/Uncomment region
(defun bind-comment-keys ()
  (local-set-key (kbd "C-c /") 'comment-region)
  (local-set-key (kbd "C-c ?") 'uncomment-region))

(add-hook 'prog-mode-hook 'bind-comment-keys)


;; Revert buffer
(global-set-key (kbd "C-c C-x r") 'revert-buffer)
(defun enable-auto-revert-mode ()
  "Enables auto-revert mode for the current buffer"
  (interactive)
  (auto-revert-mode 1))
(global-set-key (kbd "C-c C-x C-r") 'enable-auto-revert-mode)


;; Dired-up-directory alias
(require 'dired)
(define-key dired-mode-map (kbd "b") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-x C-q") 'dired-toggle-read-only)
