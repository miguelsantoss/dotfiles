;;; package -- summary
;;; Commentary:

;;; Code:
;; Turn off the blinking cursor
(blink-cursor-mode -1)

;; View hex colours in the following modes
(dolist (mode '(
                css-mode
                scss-mode
                emacs-lisp-mode
                haskell-mode
                lisp-interaction-mode
                lisp-mode
                          ))
  (font-lock-add-keywords mode
                          '((fontify-hex-colors))))

(use-package leuven-theme)
(use-package github-theme :config (load-theme 'github t))
(use-package dracula-theme)
(use-package atom-one-dark-theme)

(provide 'appearance)
;;; appearance.el ends here
