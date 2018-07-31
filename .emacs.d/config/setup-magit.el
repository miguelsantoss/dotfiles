;;; package --- Summary
;;; Commentary:

;;; Code:
(use-package magit
  :bind (("C-c g" . magit-status)
         ("C-c b" . magit-blame))
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :config
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)


  (defun icn-commit-message-template (&rest discard)
    "Insert Card ID from branch"
    (interactive)
    (insert (magit-get-current-branch))
    (beginning-of-line)
    (kill-word 1)
    (delete-char 1)
    (forward-word)
    (kill-line)
    (beginning-of-line)
    (insert "[")
    (upcase-word 1)
    (insert "] ")
    (end-of-line)
    (evil-append))

  ;; (add-hook 'git-commit-mode-hook 'icn-commit-message-template)
  ;; (add-hook 'with-editor-mode-hook 'evil-normal-state)

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

;; (use-package magithub
  ;; :after magit
  ;; :config (magithub-feature-autoinject t))

(use-package diff-hl
  :hook
  (dired-mode . diff-hl-dired-mode)
  :config
  ;; (vc-git-diff-switches '("--histogram"))
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
