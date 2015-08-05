;; OCTAVE MODE
;(autoload 'octave-mode "octave-mode" nil t)
(setq auto-mode-alist
      (cons
       '("\\.m$" . octave-mode)
       auto-mode-alist))
