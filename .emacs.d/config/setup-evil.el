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

  (define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)

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

  (use-package evil-leader
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "e" 'find-file
      "bb" 'switch-to-buffer
      "bd" 'kill-buffer-and-window
      "by" 'copy-whole-buffer
      "cy" 'clipboard-kill-ring-save
      "cp" 'clipboard-yank
      "fs" 'save-buffer
      "gs" 'magit-status
      "hs" 'split-window-horizontally
      "iu" 'insert-char
      "lf" 'load-file
      "ne" 'flycheck-next-error
      "pe" 'flycheck-previous-error
      "rm" 'notmuch
      "sm" 'message-send-and-exit
      "si" 'whitespace-mode
      "tn" 'linum-mode
      "w1" 'delete-other-windows
      "wk" 'windmove-left
      "wj" 'windmove-right
      "qq" 'save-buffers-kill-emacs
      "zp" 'zeal-at-point
      )
    )

  (use-package evil-surround
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject))

(provide 'setup-evil)
;;; setup-evil.el ends here
