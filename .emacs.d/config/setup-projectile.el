;;; package -- summary
;;; Commentary:

;;; Code:
(use-package projectile
  :custom
  (projectile-completion-system 'ivy)
  (projectile-require-project-root nil)
  ;; (projectile-switch-project-action 'projectile-vc)
  (projectile-mode-line
   '(:eval
     (format " Pr[%s]"
             (projectile-project-name))))
  (projectile-enable-caching t)
  (projectile-keymap-prefix (kbd "C-c p"))
  :config
  (projectile-mode))

(provide 'setup-projectile)
;;; setup-projectile.el ends here
