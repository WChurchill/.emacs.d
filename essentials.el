;;;; essentials.el

;;; This file is for initializing packages that are extremely
;;; convenient and would aid in debugging emacs quicker

;;; ACTIVATE HELM-MODE
(require 'helm)
(setq helm-mode-line-string "")
(helm-autoresize-mode t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h r") 'helm-register)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(helm-mode 1)

;; Easier editing of .emacs.d/
(defun em-dir ()
  "Preconfigured `helm' for helm implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `helm-find-files-1' instead.
This is the starting point for nearly all actions you can do on files."
  (interactive "P")
  (let* ((smart-input     (helm-find-files-initial-input))
         (default-input   (expand-file-name "~/.emacs.d"))
         (input           (cond (helm-find-file-ignore-thing-at-point
                                 default-input)
                                ((and (eq major-mode 'org-agenda-mode)
                                      org-directory
                                      (not smart-input))
                                 (expand-file-name org-directory))
                                ((and (eq major-mode 'dired-mode) smart-input)
                                 (file-name-directory smart-input))
                                ((and (not (string= smart-input ""))
                                      smart-input))
                                (t default-input)))
         (input-as-presel (null (nth 0 (file-attributes input))))
         (presel          (helm-aif (or (and input-as-presel input)
                                        (buffer-file-name (current-buffer))
                                        (and (eq major-mode 'dired-mode)
                                             smart-input))
                              (if helm-ff-transformer-show-only-basename
                                  (helm-basename it) it))))
    (set-text-properties 0 (length input) nil input)
    (helm-find-files-1 input (and presel (null helm-ff-no-preselect)
                                  (concat "^" (regexp-quote presel))))))

;;; AVY
(require 'avy)
(global-set-key (kbd "C-;") 'avy-goto-word-1)
(global-set-key (kbd "C-\"") 'avy-goto-char)
;;(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "C-'") 'avy-goto-line)

(setq avy-styles-list '((avy-goto-char . at)))

;; enable avy-select in isearch mode
(eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "C-;") 'avy-isearch))


;;; ACE-WINDOW
(global-set-key (kbd "C-:") 'ace-window)
