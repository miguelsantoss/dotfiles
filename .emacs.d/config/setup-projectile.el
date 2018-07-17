(use-package projectile
  :custom
  (projectile-switch-project-action 'projectile-vc)
  (projectile-mode-line
   '(:eval
     (format " Pr[%s]"
             (projectile-project-name))))
  (projectile-enable-caching t)
  :config
  (projectile-global-mode t))

(provide 'setup-projectile)
