;;;; loadpackages.el

(require 'cl)
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(defvar required-packages
  '(ac-js2
    ace-jump-mode
    ace-window
    android-mode
    arduino-mode
    auctex
    company
    css-mode
    elpy
    emacs-eclim
    ;color-identifiers-mode
    helm
    highlight-numbers
    highlight-quoted
    jedi
    js2-mode
    ;highlight-symbol
    ;hl-defined
    ;hl-sexp
    magit
    multi-term
    multi-web-mode
    multiple-cursors
    org
    paredit
    php-mode
    python-mode
    ;slime
    yasnippet) 
  "A list of packages to insure are installed at launch.")

; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))


