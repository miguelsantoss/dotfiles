;;; private/ms/config.el -*- lexical-binding: t; -*-

(def-package! feature-mode
  :mode "\\.feature$")

(map! "C-s" #'counsel-grep-or-swiper
      (:leader  :prefix "p" :n "s" #'+ivy/project-search)
      (:leader  :prefix "f" :n "d" #'dired-jump))

(def-package! lsp-mode
  :commands (lsp-mode lsp-define-stdio-client))

(def-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (set! :lookup 'lsp-ui-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t))

(def-package! company-lsp
  :after lsp-mode
  :config
  (set-company-backend! 'lsp-mode '(company-lsp))
  (setq company-lsp-enable-recompletion t))

(def-package! lsp-typescript
  :hook ((js2-mode typescript-mode) . lsp-typescript-enable))

(def-package! lsp-ruby)
