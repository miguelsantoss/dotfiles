;;; package

(setq
  ms/enable-osx-p (eq system-type 'darwin)
  ms/enable-linux-p (eq system-type 'gnu/linux)
  ms/enable-home-p (string= (system-name) "redstar")
  ms/enable-work-p ms/enable-osx-p)

(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)
(delete-selection-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style -1)
(tooltip-mode -1)

(use-package evil
  :config
  (evil-mode t))

(ido-mode t)

(use-package restart-emacs)

(use-package exec-path-from-shell
  :if ms/enable-osx-p
  :config
  (exec-path-from-shell-initialize))

;; Ruby / rails stuff
(use-package enh-ruby-mode
  :mode "\\.rb\\'"
  :mode "\\.rake\\'"
  :mode "\\.gemspec\\'"
  :mode "\\.\\(?:pry\\|irb\\)rc\\'"
  :mode "/\\(?:Gem\\|Cap\\|Vagrant\\|Rake\\|Pod\\|Puppet\\|Berks\\)file\\'"
  :interpreter "ruby")

(use-package inf-ruby
  :hook ((enh-ruby-mode . inf-ruby-minor-mode)
	 (compilation-filter-hook . inf-ruby-auto-enter)))

(use-package yard-mode
  :hook enh-ruby-mode)

(use-package rubocop
  :hook (enh-ruby-mode . rubocop-mode))

(use-package robe
  :hook (enh-ruby-mode . robe-mode))

(use-package rspec-mode)
(use-package robe)

(use-package rbenv
  :config
  (global-rbenv-mode t)
  )
