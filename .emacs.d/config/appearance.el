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
(use-package github-theme)
(use-package dracula-theme)
(use-package atom-one-dark-theme
  :config (load-theme 'atom-one-dark t))

(provide 'appearance)
