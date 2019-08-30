;;; my-ledger.el --- My custom configurations for ledger-mode

;;; Commentary:
;; Set basic options and add helm support to ledger-magic-tab

;;; Code:

(require 'ledger-mode)

(add-to-list 'ledger-reports '("net-worth" "%(binary) -f %(ledger-file) bal ^assets ^liabilities"))
(add-to-list 'ledger-reports '("monthly" "%(binary) -M -r -f %(ledger-file) bal expenses"))

(eval-after-load 'flycheck
  '(require 'flycheck-ledger))

(defun ledger-align-and-newline ()
  "Indent current line and make new a line."
  (interactive)
  (if (save-excursion
		(beginning-of-line)
		(looking-at "\s-*$"))
	  (newline)
	(progn
	  (ledger-post-align-xact (point))
	  (newline)
	  (indent-to ledger-post-account-alignment-column))))

(defun my-ledger-tab ()
  "Indent and do not attempt to autocomplete."
  (interactive)
  (ledger-post-align-xact (point)))

(define-key ledger-mode-map (kbd "<tab>") 'my-ledger-tab)
(define-key ledger-mode-map (kbd "<return>") 'ledger-align-and-newline)

(load "~/.emacs.d/helm-ledger/helm-ledger.el")
(require 'helm-ledger)

(define-key ledger-mode-map (kbd "C-<tab>") 'helm-ledger)

(provide 'my-ledger)

;;; my-ledger.el ends here
