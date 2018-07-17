(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere t)
  (ido-ubiquitous-mode t))

(use-package ido-vertical-mode
  :after ido
  :config 
  (ido-vertical-mode t))

(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))


(use-package company-quickhelp
  :after company
  :diminish
  :hook (company-mode . company-quickhelp-mode))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(provide 'setup-completion)
