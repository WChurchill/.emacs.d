;;;; my-auctex.el
;;;;
;;;; Auctex-specific configuration

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'linum-mode)
;; ;;not sure what this one does
(setq reftex-plug-into-AUCTeX t)
(setq reftex-cite-format; Get ReTeX with biblatex, see http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992
           '((?t . "\\textcite[]{%l}")
             (?a . "\\autocite[]{%l}")
             (?c . "\\cite[]{%l}")
             (?s . "\\smartcite[]{%l}")
             (?f . "\\footcite[]{%l}")
             (?n . "\\nocite{%l}")
             (?b . "\\blockcquote[]{%l}{}")))
(defun save-and-compile-latex ()
  (interactive)
  (save-buffer)
  (TeX-command-master))

(defvar LaTeX-electric-pairs '((\$ . \$)) "Electric pairs for LaTeX-mode.")
(defun LaTeX-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs LaTeX-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(defun bind-LaTeX-keys ()
  (electric-pair-mode)
  ;;(LaTeX-add-electric-pairs)
  ;;(define-key 'LaTeX-mode-map "\$" 'electric-pair)
  (local-set-key (kbd "C-c b") 'latex-insert-block)
  (local-set-key (kbd "C-c C-c") 'save-and-compile-latex)
  (local-set-key (kbd "$") 'self-insert-command)) ; Add an extra "$" when I type $

(add-hook 'LaTeX-mode-hook 'bind-LaTeX-keys)

;;; REFTEX
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
