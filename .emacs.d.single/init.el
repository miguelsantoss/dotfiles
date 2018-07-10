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
  (defadvice
    evil-search-forward
    (after evil-search-forward-recenter activate)
    (recenter))
  (ad-activate 'evil-search-forward)

  (defadvice
    evil-search-next
    (after evil-search-next-recenter activate)
    (recenter))
  (ad-activate 'evil-search-next)

  (defadvice
    evil-search-previous
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
(use-package swiper)

;; Makes sure Emacs commands use Ivy completion
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-s" . counsel-grep-or-swiper)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-x C-f" . counsel-find-file))
  :config
  (use-package smex :ensure t))

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
  (setq projectile-indexing-method 'alien))

;; Recent files
(use-package 
  recentf 
  :config (setq
           recentf-max-saved-items 100
           recentf-max-menu-items 15
           recentf-auto-cleanup 'never) 
  (recentf-mode 1) 
  :bind ("C-x C-r" . recentf-open-files))

;; Completion framework
(use-package 
  company 
  :defer t 
  :init (global-company-mode) 
  :config (progn (bind-key [remap completion-at-point] #'company-complete company-mode-map) 
		 (setq company-tooltip-align-annotations t company-show-numbers t) 
		 (setq company-dabbrev-downcase nil)) 
  :diminish company-mode)
(use-package 
  company-quickhelp 
  :defer t 
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

(use-package 
  flycheck 
  :diminish flycheck-mode)

(use-package 
  smartparens 
  :config (progn (require 'smartparens-config)) 
  :bind ("C-x j" . smartparens-mode))

(use-package 
  rainbow-delimiters 
  :defer t 
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Git on steroids
(use-package 
  magit 
  :bind ("C-x g" . magit-status))

(use-package avy
  :diminish avy-mode
  :bind (("C-x C-x" . avy-goto-word-or-subword-1)
         ("C-x C-l" . avy-goto-line))
  :config
  (setq avy-timeout-seconds 0.6))

(use-package dumb-jump
  :bind ("C-M-g". dumb-jump-go)
  :config
  (dumb-jump-mode))

(use-package css-mode :mode (("\\.css\\'" . css-mode)))
(use-package web-mode :mode "\\.html\\'")

(use-package leuven-theme)
(use-package github-theme :config (load-theme 'github))
(use-package dracula-theme)
(use-package atom-one-dark-theme)
