;;;; noexternals.el

;; Startup workspace
(defun find-my-files ()
  (let ((firstfile  "~/school")
	(secondfile "~/lisp/quicklisp/local-projects")
        )
  (if (file-exists-p firstfile)
      (find-file firstfile))
  (when (file-exists-p secondfile)
    (split-window-horizontally)
    (find-file secondfile))))

;(find-file "~/lisp/quicklisp/local-projects")

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
                 (if (eq window-system 'w32) ".exe" "") trustfile))))
;; (if (condition-case e
;;         (progn
;;           (url-retrieve "https://wrong.host.badssl.com/"
;;                         (lambda (retrieved) t))
;;           (url-retrieve "https://self-signed.badssl.com/"
;;                         (lambda (retrieved) t))
;;           t)
;;       ('error nil))
;;     (error "tls misconfigured")
;;   (url-retrieve "https://badssl.com"
;;                 (lambda (retrieved) t)))

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

;; Font
(setq line-spacing 0)
;(set-face-font 'default "-*-terminus-medium-r-*-*-*-140-75-75-*-*-iso8859-15")

;; Line width and word wrapping
(auto-fill-mode)
(setq fill-column 100)
(setq-default truncate-lines t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Custom Tabs
;; (setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)
;; (setq indent-line-function 'insert-tab)

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

