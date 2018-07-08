;; Turn off the blinking cursor
(blink-cursor-mode -1)

(setq-default evil-motion-state-cursor 'box)
(setq-default evil-visual-state-cursor 'box)
(setq-default evil-normal-state-cursor 'box)
(setq-default evil-insert-state-cursor 'box)
(setq-default evil-emacs-state-cursor  'box)

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

(use-package leuven-theme :ensure t)
(use-package github-theme :ensure t)
(use-package dracula-theme :ensure t)
(use-package atom-one-dark-theme
  :ensure t
  :config (load-theme 'atom-one-dark t))

(provide 'appearance)
