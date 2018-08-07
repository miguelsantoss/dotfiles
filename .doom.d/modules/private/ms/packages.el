;; -*- no-byte-compile: t; -*-
;;; private/ms/packages.el

(package! feature-mode)

(package! lsp-mode)
(package! lsp-ui)
(package! lsp-typescript)

(package! lsp-ruby
  :recipe (:fetcher
           github
           :repo "emacs-lsp/lsp-ruby"
           :files ("lsp-ruby.el")))

(package! company-lsp)
