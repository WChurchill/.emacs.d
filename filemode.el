;; OCTAVE MODE

;; (setq auto-mode-alist
;;       (cons
;;        '("\\.m$" . octave-mode)
;;        auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.world\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.launch\\'" . nxml-mode))
