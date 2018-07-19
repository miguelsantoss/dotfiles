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

(use-package git-gutter+
  :diminish git-gutter+-mode
  :bind (("C-M-z C-M-s" . git-gutter+-stage-hunks)
         ("C-M-z C-M-c" . git-gutter+-stage-and-commit))
  :hook (prog-mode . git-gutter+-mode))

(use-package git-gutter-fringe+
  :after git-gutter+
  :config
  (git-gutter-fr+-minimal))

(provide 'setup-magit)
;;; setup-magit.el ends here
