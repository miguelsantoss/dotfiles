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

;; Undo windows
(winner-mode 1)
(global-prettify-symbols-mode +1) ;; Make symbols pretty e.g. lamba

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package restart-emacs :ensure t)

(provide 'setup-core)
