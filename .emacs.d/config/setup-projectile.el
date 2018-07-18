;;; package -- summary
;;; Commentary:

;;; Code:
(use-package projectile
  :custom
  (projectile-switch-project-action 'projectile-vc)
  (projectile-mode-line
   '(:eval
     (format " Pr[%s]"
             (projectile-project-name))))
  (projectile-enable-caching t)
  :config
  (projectile-mode t))

(provide 'setup-projectile)
;;; setup-projectile.el ends here
