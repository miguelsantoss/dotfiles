;;; package -- summary
;;; Commentary:

;;; Code:
(use-package evil
  :custom
  (evil-motion-state-cursor 'box)
  (evil-visual-state-cursor 'box)
  (evil-normal-state-cursor 'box)
  (evil-insert-state-cursor 'box)
  (evil-emacs-state-cursor  'box)

  :config
  (evil-mode t)

  (setq-default evil-want-C-d-scroll t)
  (setq-default evil-want-C-u-scroll t)

  ;; Centre screen around a search
  (defadvice
      evil-search-forward
      (after evil-search-forward-recenter activate)
    (recenter))
  (ad-activate 'evil-search-forward)

    (defadvice
      evil-search-next
      (after evil-search-next-recenter activate)
    (recenter))
  (ad-activate 'evil-search-next)

  (defadvice
      evil-search-previous
      (after evil-search-previous-recenter activate)
    (recenter))
  (ad-activate 'evil-search-previous)

  (use-package evil-escape
    :config
    (evil-escape-mode t)
    (setq-default evil-escape-key-sequence "fd")
    (setq-default evil-escape-delay 0.2))

  (use-package evil-surround
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject))

(provide 'setup-evil)
;;; setup-evil.el ends here
