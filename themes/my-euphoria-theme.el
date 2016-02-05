;;; my-euphoria-theme.el --- euphoria theme

;; Copyright (C) 2000 by oGLOWo
;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/emacs-jp/replace-colorthemes
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Port of euphoria theme from `color-themes'

;;; Code:

(deftheme my-euphoria
  "my euphoria theme")

(custom-theme-set-faces
 'my-euphoria

 '(default ((t (:background "black" :foreground "green"))))
 '(mouse ((t (:foregound "white"))))
 '(cursor ((t (:foregound "white"))))
 '(border ((t (:foregound "black"))))

 '(help-highlight-face ((t (:underline t))))
 '(list-matching-lines-face ((t (:bold t :weight bold))))
 '(widget-mouse-face ((t (:background "darkolivegreen"))))

 '(bold ((t (:bold t :weight bold))))
 '(bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
 '(border ((t (:background "black"))))
 '(comint-highlight-input ((t (:bold t :weight bold))))
 '(comint-highlight-prompt ((t (:foreground "cyan"))))
 '(cursor ((t (:background "lightgray"))))
 '(fixed-pitch ((t (:family "courier"))))
 '(font-lock-builtin-face ((t (:foreground "chartreuse2" :bold t))))
 '(font-lock-comment-face ((t (:foreground "gray45"))))
 '(font-lock-constant-face ((t (:foreground "gray"))))
 '(font-lock-doc-face ((t (:foreground "cyan"))))
 '(font-lock-doc-string-face ((t (:foreground "cyan3"))))
 '(font-lock-function-name-face ((t (:foreground "magenta" :bold nil))))
 '(font-lock-keyword-face ((t (:foreground "blue" :bold t))))
 '(font-lock-preprocessor-face ((t (:foreground  "mediumspringgreen" ))))
 '(font-lock-reference-face ((t (:foreground "white"))))
 '(font-lock-string-face ((t (:foreground "yellow3"))))
 '(font-lock-type-face ((t (:foreground "#409b40" :bold nil ))))
 '(font-lock-variable-name-face ((t (:foreground "royalblue2"))))
 '(font-lock-warning-face ((t (:bold t :foreground "red" :weight bold))))
 '(fringe ((t (:background "gray10" :foreground "#00ff00"))))
 '(header-line ((t (:box (:line-width -1 :style released-button) :background "grey20" :foreground "grey90" :box nil))))
 '(highlight ((t (:background "darkolivegreen"))))
 '(horizontal-divider ((t (:background "gray16" :foreground "#00ff00"))))
 '(isearch ((t (:background "green" :foreground "black"))))
 '(isearch-lazy-highlight-face ((t (:background "DarkSlateGray"))))
 '(italic ((t (:italic t :slant italic))))
 '(menu ((t (:background "gray16" :foreground "green"))))
 '(modeline ((t (:background "gray16" :foreground "#00ff00"
                 :box (:line-width -1 :style released-button)))))
 '(modeline-buffer-id ((t (:background "gray10" :foreground "#00ff00"))))
 '(modeline-mousable ((t (:background "gray16" :foreground "#00ff00"))))
 '(modeline-mousable-minor-mode ((t (:background "gray16" :foreground "#00ff00"))))
 '(mouse ((t (:background "yellow"))))
 '(primary-selection ((t (:background "#00ff00" :foreground "black"))))
 '(region ((t (:background "green3" :foreground "black"))))
 '(scroll-bar ((t (:background "gray16" :foreground "#00ff00"))))
 '(secondary-selection ((t (:background "#00ff00" :foreground "black"))))
 '(show-paren-match-face ((t (:background "green" :foreground "black"))))
 '(show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))
 '(speedbar-button-face ((t (:foreground "#00ff00"))))
 '(speedbar-directory-face ((t (:foreground "#00ff00"))))
 '(speedbar-file-face ((t (:foreground "red"))))
 '(speedbar-highlight-face ((t (:background "#00ff00" :foreground "purple"))))
 '(speedbar-selected-face ((t (:foreground "deeppink" :underline t))))
 '(speedbar-tag-face ((t (:foreground "yellow"))))
 '(tool-bar ((t (:background "green" :foreground "black"
                 :box (:line-width 1 :style released-button)))))
 '(tooltip ((t (:background "gray16" :foreground "#00ff00"))))
 '(trailing-whitespace ((t (:background "blue"))))
 '(underline ((t (:underline t))))
 '(variable-pitch ((t (:family "helv"))))
 '(vertical-divider ((t (:background "gray16" :foreground "#00ff00"))))
 '(widget-button-face ((t (:bold t :weight bold))))
 '(widget-button-pressed-face ((t (:foreground "red"))))
 '(widget-documentation-face ((t (:foreground "lime green"))))
 '(widget-field-face ((t (:background "dim gray"))))
 '(widget-inactive-face ((t (:foreground "light gray"))))
 '(widget-single-line-field-face ((t (:background "dim gray"))))
 '(zmacs-region ((t (:background "steelblue" :foreground "white")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'my-euphoria)

;;; my-euphoria-theme.el ends here
