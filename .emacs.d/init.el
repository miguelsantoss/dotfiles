;; UTF-8 please
(set-language-environment "UTF-8") ;; Set input
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(package-initialize)

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

(setq use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))
(use-package diminish)

(defmacro add-λ (hook &rest body)
  (declare (indent 1) (debug t))
  `(add-hook ,hook (lambda () ,@body)))

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Highlight each source code identifier based on name
(use-package color-identifiers-mode
  :defer t)

(use-package avy
  :diminish avy-mode
  :bind (("C-x C-SPC" . avy-goto-char)
         ("C-x C-x" . avy-goto-word-or-subword-1)
         ("C-x C-l" . avy-goto-line))
  :config
  (setq avy-timeout-seconds 0.6))

(use-package dumb-jump
  :bind ("C-M-g". dumb-jump-go)
  :config
  (dumb-jump-mode))

(require 'setup-core)
(require 'setup-evil)
(require 'setup-projectile)
(require 'setup-completion)
(require 'setup-langs)
(require 'setup-magit)
(require 'appearance)

(provide 'init)
;;; init.el ends here
