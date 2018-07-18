;;; package -- summary
;;; Commentary:

;;; Code:
(use-package ido
  :custom
  (ido-enable-prefix nil)
  (ido-enable-flex-matching t)
  (ido-case-fold nil)
  (ido-auto-merge-work-directories-length -1)
  (ido-create-new-buffer 'always)
  (ido-use-filename-at-point nil)
  (ido-max-prospects 10)
  (ido-use-faces nil)
  (ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  (ido-everywhere t)
  :config
  (ido-mode t))

(use-package flx-ido
  :after ido
  :config
  (flx-ido-mode t))

(use-package ido-vertical-mode
  :after ido
  :config
  (ido-vertical-mode t))

(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode t))

(use-package smex
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

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
;;; setup-completion.el ends here
