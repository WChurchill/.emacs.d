;;;; upgrade-packages.el
;;;; This script automatically upgrades all outdated packages
;;;; TODO: handle missing internet connection 
;;;; TODO: desktop notifications

(require 'package)
(with-temp-buffer
  (package-menu-mode)
  (package-menu--generate nil t)
  (package-menu-mark-upgrades)
  (package-menu-execute t)
  (package-autoremove))
