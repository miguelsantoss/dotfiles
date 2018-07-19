;;; package -- summary
;;; Commentary:

;;; Code:
(use-package ivy
  :custom
  (ivy-initial-inputs-alist nil)
  (ivy-extra-directories nil)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-format-function #'ivy-format-function-arrow)
  (ivy-display-style 'fancy)
  (ivy-use-selectable-prompt t)
  :bind ("C-c C-r" . ivy-resume)
  :init
  (ivy-mode))

(use-package ivy-historian
  :after ivy
  :config
  (ivy-historian-mode))

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-abbreviate-paths t)
  (ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))
(use-package prescient
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :after (ivy prescient)
  (ivy-prescient-mode))

(use-package swiper
  :custom
  (swiper-include-line-number-in-search t))

(use-package counsel
  :bind
  (([remap execute-extended-command] . counsel-M-x)
   ([remap find-file] . counsel-find-file)
  ("C-s" . counsel-grep-or-swiper)
  ("C-h v" . counsel-describe-variable)
  ("C-h f" . counsel-describe-function)))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode))

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

(use-package company-prescient
  :after (company prescient)
  :config
  (company-prescient-mode))

(provide 'setup-completion)
;;; setup-completion.el ends here
