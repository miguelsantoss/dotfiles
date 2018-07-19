;;; package -- summary
;;; Commentary:

;;; Code:
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

;; create backups on a different folder
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Undo windows
(winner-mode 1)
(global-prettify-symbols-mode +1) ;; Make symbols pretty e.g. lamba
(global-linum-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)


(use-package restart-emacs)

(if (eq system-type 'darwin)
    (setq ns-right-alternate-modifier nil))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :defer 1
  :commands (exec-path-from-shell-initialize
             exec-path-from-shell-copy-env)
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package server
  :init
  (server-mode t)
  :config
  (unless (server-running-p)
    (server-start)))

(use-package undo-tree
  :diminish undo-tree-mode
  :demand
  :config
  (global-undo-tree-mode)
  :bind
  (("C-z" . undo-tree-undo)
        ("C-M-z" . undo-tree-redo)))

(use-package smex)
(use-package visual-regexp)
(use-package multiple-cursors)
(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
        ("C--" . er/contract-region)))

(use-package ace-window
  :ensure t
  :bind([remap other-window] . ace-window)
  :custom
  (aw-dispatch-always t)
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

(use-package smartparens
  :diminish smartparens-mode
  :config (smartparens-mode t))

(use-package paren
  :custom
  (show-paren-style 'parenthesis)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :config (which-key-mode t))

(use-package recentf
  :custom
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup 'never)
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (recentf-mode t))

(use-package beacon
  :diminish beacon-mode
  :custom
  (beacon-blink-delay .5)
  (beacon-size 4)
  (beacon-blink-when-focused t)
  (beacon-blink-duration .5)
  (beacon-blink-when-window-scrolls t)
  :config
  (beacon-mode t))

(provide 'setup-core)
;;; setup-core.el ends here
