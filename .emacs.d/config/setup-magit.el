;;; package --- Summary
;;; Commentary:

;;; Code:
(use-package magit
  :bind ("C-c g" . magit-status)
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :config
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
  
  ;;This setting is needed to use ivy completion:
  
  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(use-package magithub
  :after magit
  :config (magithub-feature-autoinject t))

(use-package diff-hl
  :hook
  (dired-mode . diff-hl-dired-mode)
  :config
  (vc-git-diff-switches '("--histogram"))
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh t))

(use-package git-messenger
  :custom
  (git-messenger:show-detail t))

(use-package git-gutter+
  :diminish
  :hook (prog-mode . git-gutter+-mode))

(use-package git-gutter-fringe+
  :after git-gutter+
  :diminish
  :config
  (git-gutter-fr+-minimal))

(provide 'setup-magit)
;;; setup-magit.el ends here
