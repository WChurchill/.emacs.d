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
(global-set-key (kbd "M-J") 'join-line)

(defun backward-kill-line ()
  "Kill text on line before point."
  (interactive)
  (kill-line 0))

(global-set-key (kbd "<C-backspace>") 'backward-kill-line)

(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;; Backward-kill-sexp
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)

;; Replace List-buffers with Ibuffer
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b b") 'ibuffer)

;; Switch buffers in only one key stroke
(global-unset-key (kbd "C-x b"))
(global-set-key (kbd "C-S-b") 'switch-to-buffer)

;;Quicker Keybinding Writing
(defun new-global-key ()
  "Insert the string \"(global-set-key (kbd\"\") ') and places point in between the quotes."
  (interactive)
  (insert "(global-set-key (kbd \"\") ')")
  (backward-char 5))
(defun new-local-key ()
  "Insert the string \"(local-set-key (kbd\"\") ') and places point in between the quotes."
  (interactive)
  (insert "(local-set-key (kbd \"\") ')")
  (backward-char 5))

(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'new-global-key)
(define-key emacs-lisp-mode-map (kbd "C-c C-l") 'new-local-key)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-buffer)

;; replace-regexp
(global-set-key (kbd "C-x q") 'replace-regexp)
;; (global-set-key (kbd "C-x C-q") 'read-only-mode)

(defun save-all-delete-frame ()
  (interactive)
  (save-some-buffers)
  (delete-frame))

;; Single stroke window management
(global-unset-key (kbd "C-x 0"))
(global-unset-key (kbd "C-x 1"))
(global-unset-key (kbd "C-x 2"))
(global-unset-key (kbd "C-x 3"))

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
(global-set-key (kbd "C-x <") 'scroll-right)
(global-set-key (kbd "C-x >") 'scroll-left)

;; Toggle Menu Bar
(global-set-key (kbd "C-c C-x t") 'toggle-menu-bar-mode-from-frame)

;; Comment/Uncomment region
(define-key prog-mode-map (kbd "C-c /") 'comment-region)
(define-key prog-mode-map (kbd "C-c ?") 'uncomment-region)


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


;; Easily recenter to the bottom
(defun recenter-bottom ()
  (interactive)
  (recenter (- -1 (min (max 0 scroll-margin)
					   (truncate (/ (window-body-height) 4.0))))))
(global-set-key (kbd "C-S-l") 'recenter-bottom)

;; Don't suspend the frame if I accidentaly try to undo with C-z
(global-unset-key (kbd "C-z"))

(global-set-key (kbd "C-S-r") #'raise-sexp)

;; Eval and replace
(defun eval-and-replace ()
  "Replace the preceding sexp with the result of its evaluation."
  (interactive)
  (let ((value (eval (preceding-sexp))))
	(kill-sexp -1)
	(insert (format "%S" value))))
(global-set-key (kbd "C-c e") #'eval-and-replace)


