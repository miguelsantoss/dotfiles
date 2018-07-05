;; UTF-8 please
(set-language-environment "UTF-8") ;; Set input
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(package-initialize)

(set-frame-font "Monaco 12" nil t)	   ;; Set font
(menu-bar-mode -1)		   ;; Remove menu bar
(tool-bar-mode -1)		   ;; Remove toolbar
(scroll-bar-mode -1)		   ;; Remove scrollbar
(tooltip-mode -1)		   ;; Remove scrollbar
(setq initial-scratch-message nil) ;; Disable text in scratch
(setq inhibit-startup-message t)   ;; Disable startup message

(setq ring-bell-function 'ignore) ;; Disable raping your ears with error ring tone
(setq gc-cons-threshold (* 10 1024 1024)) ;; Reduce the frequency of garbage collection (default is 0.76MB, this sets it to 10MB)

;; Don't load old elc files when el is newer
(setq load-prefer-newer t)

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

(eval-when-compile
  (require 'use-package))
(use-package diminish :ensure t)

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(winner-mode 1)
(global-prettify-symbols-mode +1) ;; Make symbols pretty e.g. lamba

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package github-theme)
(use-package dracula-theme)
(use-package atom-one-dark-theme
             :config (load-theme 'atom-one-dark t))

;; Highlight each source code identifier based on name
(use-package color-identifiers-mode
  :ensure t
  :defer t)

;; These functions are useful. Activate them.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Indent with spaces
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)

;; Turn off the blinking cursor
(blink-cursor-mode -1)

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

(use-package avy
  :ensure t
  :diminish avy-mode
  :bind (("C-x C-SPC" . avy-goto-char)
         ("C-x C-x" . avy-goto-word-or-subword-1)
         ("C-x C-l" . avy-goto-line))
  :config
  (setq avy-timeout-seconds 0.6))

(use-package dumb-jump
  :ensure t
  :bind ("C-M-g". dumb-jump-go)
  :config
  (dumb-jump-mode))

(use-package restart-emacs :ensure t)
