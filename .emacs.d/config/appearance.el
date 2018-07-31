;;; package -- summary
;;; Commentary:

;;; Code:
;; Set font
(set-frame-font "Monaco 13" nil t)

;; Turn off the blinking cursor
(blink-cursor-mode -1)

(use-package leuven-theme)
(use-package github-theme)
(use-package dracula-theme)
(use-package atom-one-dark-theme)
(use-package apropospriate-theme
  :init (load-theme 'apropospriate-light t))

(provide 'appearance)
;;; appearance.el ends here
