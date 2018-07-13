;;; package --- Summary
;; UTF-8 please
(set-language-environment "UTF-8") ;; Set input
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; unbind
(setq ns-right-alternate-modifier nil)
(package-initialize)
(unless package--initialized (package-initialize t))
;; initalize all ELPA packages
(require 'package)
(setq package-enable-at-startup nil
      package-archives
      '(("melpa"           . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/"))
      package-user-dir "~/.emacs.d/elpa/")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))
(use-package diminish)

(menu-bar-mode -1)		   ;; Remove menu bar
(tool-bar-mode -1)		   ;; Remove toolbar
(scroll-bar-mode -1)		   ;; Remove scrollbar
(tooltip-mode -1)		   ;; Remove scrollbar
(setq initial-scratch-message nil) ;; Disable text in scratch
(setq inhibit-startup-message t)   ;; Disable startup message

(setq ring-bell-function 'ignore) ;; Disable raping your ears with error ring tone
(setq gc-cons-threshold (* 10 1024 1024)) ;; Reduce the frequency of garbage collection (default is 0.76MB, this sets it to 10MB)

;; These functions are useful. Activate them.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Indent with spaces
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)

;; Don't count two spaces after a period as the end of a sentence.
;; Just one space is needed.
(setq sentence-end-double-space nil)

;; delete the region when typing, just like as we expect nowadays.
(delete-selection-mode t)

(column-number-mode t)
;; unprettify symbol when at right edge
(setq prettify-symbols-unprettify-at-point 'right-edge)
(setq uniquify-buffer-name-style 'forward)

;; Don't create backups
(setq make-backup-files nil)
(set-fringe-mode '(6 . 0))

;; Undo windows
(winner-mode 1)
(global-prettify-symbols-mode +1) ;; Make symbols pretty e.g. lamba
(global-linum-mode t)

;; set titlebar - show path to file and project name if available
(defun frame-title-format ()
  "Return frame title with current project name, where applicable."
  (concat
   "emacs - "
   (when (and (bound-and-true-p projectile-mode)
              (projectile-project-p))
     (format "[%s] - " (projectile-project-name)))
   (let ((file buffer-file-name))
     (if file
          (abbreviate-file-name file)
       "%b"))))

(setq-default frame-title-format '((:eval (frame-title-format))))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; Turn off the blinking cursor
(blink-cursor-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

(set-frame-font "Monaco 12" nil t)	   ;; Set font
;; Don't load old elc files when el is newer
(setq load-prefer-newer t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(use-package restart-emacs)
(use-package which-key
  :defer 2
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

(use-package smooth-scrolling
  :hook (after-init . smooth-scrolling-mode)
  :config
  (setq smooth-scroll-margin 5))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :defer 1
  :commands (exec-path-from-shell-initialize
             exec-path-from-shell-copy-env)
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package color-identifiers-mode)

(use-package evil
  :init
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-u-scroll t)

  :config
  (evil-mode 1)

  (define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)

  (setq-default evil-motion-state-cursor 'box)
  (setq-default evil-visual-state-cursor 'box)
  (setq-default evil-normal-state-cursor 'box)
  (setq-default evil-insert-state-cursor 'box)
  (setq-default evil-emacs-state-cursor  'box)

  ;; Centre screen around a search
  (defadvice evil-search-forward
    (after evil-search-forward-recenter activate)
    (recenter))
  (ad-activate 'evil-search-forward)

  (defadvice evil-search-next
    (after evil-search-next-recenter activate)
    (recenter))
  (ad-activate 'evil-search-next)

  (defadvice evil-search-previous
    (after evil-search-previous-recenter activate)
    (recenter))
  (ad-activate 'evil-search-previous)

  (use-package evil-escape
               :config
               (evil-escape-mode 1)
               (setq-default evil-escape-key-sequence "fd")
               (setq-default evil-escape-delay 0.2))

  (use-package general
    :config
    (general-evil-setup t)

    (general-define-key
     :states '(normal insert emacs)
     :prefix "C-SPC"
     :non-normal-prefix "C-SPC"
     "l" '(avy-goto-line)
     "a" 'align-regexp
     )

    (general-define-key
     :states '(normal motion emacs)
     :prefix "SPC"
     "" nil
     "SPC" '(counsel-M-x :which-key "counsel M-x")
     "bb" '(switch-to-buffer :which-key "switch to buffer")
     "e" '(find-file :which-key "find-file")
     "bd" '(kill-buffer-and-window :which-key "kill-buffer-and-window")
     "by" '(copy-whole-buffer :which-key "copy-whole-buffer")
     "cy" '(clipboard-kill-ring-save :which-key "clipboard-kill-ring-save")
     "cp" '(clipboard-yank :which-key "clipboard-yank")
     "fs" '(save-buffer :which-key "save-buffer")
     "iu" '(insert-char :which-key "insert-char")
     "lf" '(load-file :which-key "load-file")
     "ne" '(flycheck-next-error :which-key "flycheck-next-error")
     "pe" '(flycheck-previous-error :which-key "flycheck-previous-error")
     "rm" '(notmuch :which-key "notmuch")
     "sm" '(message-send-and-exit :which-key "message-send-and-exit")
     "si" '(whitespace-mode :which-key "whitespace-mode")
     "tn" '(linum-mode :which-key "linum-mode")
     "w1" '(delete-other-windows :which-key "delete-other-windows")
     "qq" '(save-buffers-kill-emacs :which-key "save-buffers-kill-emacs")
     "zp" '(zeal-at-point :which-key "zeal-at-point")
     "g"  '(:ignore t :which-key "Git")
     "gs" '(magit-status :which-key "git status")))

(use-package evil-surround
             :config
             (global-evil-surround-mode))

(use-package evil-indent-textobject))

;; Search stuff
(use-package swiper
  :bind (("C-c u" . swiper-all))
  :config
  (setq swiper-include-line-number-in-search t))

;; Makes sure Emacs commands use Ivy completion
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-s" . counsel-grep-or-swiper)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-x C-f" . counsel-find-file))
  :config
  (use-package smex))

(use-package ivy-posframe
  :disabled
  :after ivy
  :config
  (setq ivy-display-function #'ivy-posframe-display-at-point))

  ;; Completion mechanism for various things
(use-package ivy
  :diminish ivy-mode
  :bind (("C-x b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line))
  :config
  ;; Disable ido
  (with-eval-after-load 'ido
    (ido-mode -1)
    ;; Enable ivy
    (ivy-mode 1))
  (setq ivy-display-style 'fancy)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  ;; C-M-j imediate done ivy
  ;; ;; Show recently killed buffers when calling ivy-switch-buffer
  (setq ivy-use-virtual-buffers t)
  ;; (setq ivy-virtual-abbreviate 'full) ; Show the full virtual file paths
  ;; ;; Do not show "./" and "../" in the counsel-find-file completion list
  (setq ivy-extra-directories nil))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t)
  (setq ivy-rich-abbreviate-paths t)
  (setq ivy-rich-path-style 'abbrev))

(use-package counsel-projectile
  :bind ("C-c p p " . counsel-projectile-switch-project)
  :config
  (counsel-projectile-mode))

(use-package projectile
  :after counsel-projectile
  :diminish projectile-mode
  :init
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy) ;So projectile works with ivy
  (setq projectile-indexing-method 'alien)
  :config (setq projectile-enable-caching t))

(use-package projectile-rails
  :after projectile
  :config )

;; Recent files
(use-package recentf
  :config (setq
           recentf-max-saved-items 100
           recentf-max-menu-items 15
           recentf-auto-cleanup 'never)
  (recentf-mode 1)
  :bind ("C-x C-r" . recentf-open-files))

;; Completion framework
(use-package company
  :diminish company-mode
  :init 
  (setq company-idle-delay 0.3)
  (setq company-begin-commands '(self-insert-command))
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-limit 20)
  (setq company-dabbrev-downcase nil)
  :config (global-company-mode))

(use-package company-quickhelp
  :after (company)
  :config
  (company-quickhelp-mode))

(use-package company-childframe
  :diminish company-childframe-mode
  :after company
  :config
  (company-childframe-mode 1)
  ;; let desktop.el not record the company-childframe-mode
  (require 'desktop) ;this line is needed.
  (push '(company-childframe-mode . nil)
        desktop-minor-mode-table))

;; ivy/company results sorting
(use-package prescient) 

(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode))

(use-package company-statistics
  :after company
  :config
  (company-statistics-mode))

(use-package company-flx
  :disabled
  :after company
  :config
  (company-flx-mode +1)
  (setq company-flx-limit 400))

(use-package flycheck
  :diminish flycheck-mode
  :config
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-indication-mode nil)
  (setq flycheck-display-errors-delay 1.5)
  (setq flycheck-idle-change-delay 3)
  (add-hook 'prog-mode-hook 'flycheck-mode))

(use-package smartparens
  :init
  :config
  (progn
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1)
    (setq smartparens-strict-mode t)
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
  
  ;;This setting is needed to use ivy completion:
  (setq magit-completing-read-function 'ivy-completing-read)
  
  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(use-package git-gutter+
  :diminish git-gutter+-mode
  :bind (("C-M-z C-M-s" . git-gutter+-stage-hunks)
         ("C-M-z C-M-c" . git-gutter+-stage-and-commit))
  :init (global-git-gutter+-mode))

(use-package git-gutter-fringe+
  :after git-gutter+
  :config (git-gutter-fr+-minimal))

(use-package avy
  :diminish avy-mode
  :bind (("C-x C-x" . avy-goto-word-or-subword-1)
         ("C-x C-l" . avy-goto-line))
  :config
  (setq avy-timeout-seconds 0.6))

(use-package smartscan
  :bind (("M-p" . smartscan-symbol-go-back)
         ("M-n" . smartscan-symbol-go-forward))
  :config
  (smartscan-mode 1))

(use-package dumb-jump
  :bind ("C-M-g". dumb-jump-go)
  :config
  (dumb-jump-mode))

(use-package undo-tree
  :bind ("C-z" . undo-tree-undo)
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-diff t)))

(use-package neotree
  :disabled
  :bind ("<f8>" . neotree-toggle)
  :config
  (setq neo-smart-open t)
  (setq neo-vc-integration nil)
  ;; Do not allow neotree to be the only open window
  (setq-default neo-dont-be-alone t)
  (setq neo-fit-to-contents nil)
  (setq neo-theme 'arrow)
  (setq neo-window-fixed-size nil))

(use-package beacon
  :diminish beacon-mode
  :defer 10
  :config
  (setq beacon-blink-delay .5)
  (setq beacon-size 5)
  (setq beacon-blink-when-focused t)
  (setq beacon-blink-duration .5)
  (setq beacon-blink-when-window-scrolls t)
  (beacon-mode 1))

(use-package feebleline
  :defer 1
  :config
  (window-divider-mode t)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-places (quote bottom-only))
  (feebleline-mode t))

(use-package rainbow-mode
  :hook (css-mode . rainbow-mode))

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package highlight-numbers
  :diminish highlight-numbers-mode
  :commands highlight-numbers-mode
  :init (add-hook 'python-mode-hook 'highlight-numbers-mode))

(use-package highlight-operators
  :diminish highlight-operators-mode
  :commands highlight-operators-mode
  :init (add-hook 'python-mode-hook 'highlight-operators-mode))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-symbol-mode))

(use-package multiple-cursors
  :defer t)

(use-package expand-region
  :bind(("C-=" . er/expand-region)
        ("C--" . er/contract-region)))

(use-package wgrep)

(use-package visual-regexp
  :defer t
  :config
  (use-package visual-regexp-steroids))

(use-package css-mode :mode ("\\.css\\'" . css-mode))
(use-package scss-mode)

(use-package web-mode
  :mode ("\\.html\\'" . web-mode)
  :mode ("\\.erb\\'" . web-mode)
  :init 
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-attr-value-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-comment-keywords t)
  (setq web-mode-enable-current-element-highlight t))

(use-package company-web
  :hook (web-mode . (lambda ()
    (add-to-list 'company-backends 'company-web-html)
    (add-to-list 'company-backends 'company-web-jade))))

(use-package emmet-mode
  :hook (web-mode html-mode css-mode))

;; ruby / rails
(use-package ruby-mode
  :config
  (progn
    (use-package rbenv)
    (use-package ruby-tools)
    (use-package rspec-mode
      :config
      (progn
        (add-hook 'compilation-mode-hook
                  (lambda ()
                    (when (eq major-mode 'rspec-compilation-mode)
                      (setq compilation-scroll-output t)
                      (local-set-key (kbd "g") (lambda () (interactive) (rspec-rerun))))))
        (setq rspec-use-rvm t)
        (setq rspec-use-rake-when-possible nil)
        (defadvice rspec-compile (around rspec-compile-around activate)
          "Use BASH shell for running the specs because of ZSH issues."
          (let ((shell-file-name "/bin/bash"))
            ad-do-it))))
    (setq ruby-align-to-stmt-keywords '(begin if while unless until case for def))
    (setq ruby-insert-encoding-magic-comment nil)
    (setq ruby-deep-indent-paren nil))
  :mode (("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode)))

(use-package robe
  :after ruby-mode
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (push 'company-robe company-backends))

;; cucumber
(use-package feature-mode
  :mode ("\\.feature$" . feature-mode)
  :config
  (add-hook 'feature-mode-hook
            (lambda ()
              (electric-indent-mode -1))))

;; Javascript / react

(use-package coffee-mode)
(use-package eslint-fix)

(use-package js2-mode
  :init
  (setq js2-include-node-externs t)
  (setq js2-include-browser-externs t)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js-switch-indent-offset 2)
  :config
  (js2-imenu-extras-mode))

(use-package rjsx-mode
  :mode(("\\.js\\'" . rjsx-mode)
  ("\\.jsx\\'" . rjsx-mode)))

(defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
  "Workaround 'sgml-mode' and follow airbnb component style."
  (save-match-data
    (save-excursion
(goto-char (line-beginning-position))
(when (looking-at "^\\( +\\)\/?> *$")
  (let ((empty-spaces (match-string 1)))
    (while (search-forward empty-spaces (line-end-position) t)
      (replace-match (make-string (- (length empty-spaces) sgml-basic-offset)
          32))))))))

(use-package js2-refactor
  :hook (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package add-node-modules-path
  :hook (js2-mode . add-node-modules-path))

(defun setup-tide-mode ()
  "Custom Tide setup function."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :hook
  (js2-mode . setup-tide-mode))

(use-package indium
  :diminish (indium-interaction-mode . "In" )
  :hook (js2-mode . indium-interaction-mode))

(use-package prettier-js
   :after add-node-modules-path
   :hook (js2-mode . prettier-js-mode))

(use-package leuven-theme)
(use-package github-theme :config (load-theme 'github 'no-confirm))
(use-package dracula-theme)
(use-package atom-one-dark-theme)

(provide 'init)
;;; init.el ends here
