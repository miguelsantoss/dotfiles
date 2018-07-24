;;; package -- summary
;;; Commentary:

;;; Code:
(use-package projectile
  :custom
  (projectile-completion-system 'ivy)
  (projectile-require-project-root nil)
  (projectile-switch-project-action 'projectile-vc)
  (projectile-mode-line
   '(:eval
     (format " Pr[%s]"
             (projectile-project-name))))
  (projectile-enable-caching t)
  :bind (("C-c p p" . projectile-switch-project)
        ("C-c p f" . projectile-find-file))
  :config
  (projectile-mode))

(provide 'setup-projectile)
;;; setup-projectile.el ends here
