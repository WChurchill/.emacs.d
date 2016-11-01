;;;; noexternals.el

;; Startup workspace
(defun find-my-files ()
  (let ((firstfile  "~/school/school.org")
	(secondfile "~/org/main.org"))
  (if (file-exists-p firstfile)
      (find-file firstfile))
  (when (file-exists-p secondfile)
    (split-window-horizontally)
    (find-file secondfile))))

;;(find-file "~/org/main.org")
;;(find-my-files)

;; Security Patches
;; taken from https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(setq tls-checktrust t)
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

(if (condition-case e
		(progn
		  (message "Checkpoint")
		  (url-retrieve "https://wrong.host.badssl.com/"
						(lambda (retrieved) t))
		  (url-retrieve "https://self-signed.badssl.com/"
						(lambda (retrieved) t))
		  t)
	  (error nil))
	(error "tls misconfigured")
  (url-retrieve "https://badssl.com"
				(lambda (retrieved) t)))

;; Easier editing of .emacs.d/
(defun em-dir ()
  (interactive)
  (find-file "~/.emacs.d"))

;; Easier setup of lisp workspace
(defun lisp-dir ()
  (interactive)
  (find-file "~/lisp"))

(defun l-proj ()
  (interactive)
  (find-file "~/lisp/quicklisp/local-projects"))

;; Find C++ directory easily
(defun c-dir ()
  (interactive)
  (find-file "~/C++"))

;; Auto-update changed files
(global-auto-revert-mode)

;; Font
(setq line-spacing 0)
;;(set-face-attribute 'default nil :height 108)
(set-face-font 'default "-*-Inconsolata-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")
;;(set-face-font 'default "-*-inconsolata-r-*-*-*-140-75-75-*-*-iso8859-15")

;; Line width and word wrapping
(auto-fill-mode 1)
(setq auto-fill-column 100)
;(setq-default truncate-lines t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Custom Tabs
(setq-default c-default-style "linux")
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
;(setq indent-line-function 'insert-tab)

;; Make return key also indent
(electric-indent-mode 1)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Don't tell me what the scratch buffer is for.
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

;; Don’t defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; enable electric-pair mode for C-like languages (e.g. Java, C++, etc.)
(add-hook 'c-mode-common-hook 'electric-pair-mode)

;; also for shell-script-mode
(add-hook 'sh-mode-hook 'electric-pair-mode)

;; Start show-paren-mode
(show-paren-mode 1)

;; highlight line mode
(global-hl-line-mode t)

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
                         (helm-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Start fullscreen
(toggle-frame-maximized)
;(toggle-frame-fullscreen)

;; Cursor Blinking
;; enable cursor blinking mode
(blink-cursor-mode 1) ; enable haxxing mode
;; number of blinks before using solid cursor
;; if arg is 0 or negative, never stop blinking
(setq blink-cursor-blinks 0) ; blink forever!

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

;; Set default browser
;; this doesn't work for some reason
;(setq browse-url-default-browser "/usr/bin/chromium")
; this works though
(setq browse-url-browser-function 'browse-url-chromium)

;; Default dired shell commands
(setq dired-guess-shell-alist-user
	  '(("\\.\(pdf\)"
		 "evince > /dev/null")
		("\\.\(odt\|docx?\|pptx?\)"
		 "libreoffice > /dev/null")
		("*\.\(jpe?g\|png\|gif\|bmp\)"
		 "eog > /dev/null")
		("\\.\(mp4\|webm\|mov\)"
		 "vlc > /dev/null")))
