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

(provide 'setup-core)
