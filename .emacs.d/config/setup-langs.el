;;; package -- summary
;;; Commentary:

;;; Code:
(defun doom-buffers-in-mode (modes &optional buffer-list derived-p)
  "Return a list of buffers whose `major-mode' is `eq' to MODE(S).

If DERIVED-P, test with `derived-mode-p', otherwise use `eq'."
  (let ((modes (doom-enlist modes)))
    (cl-remove-if-not (if derived-p
                          (lambda (buf)
                            (with-current-buffer buf
                              (apply #'derived-mode-p modes)))
                        (lambda (buf)
                          (memq (buffer-local-value 'major-mode buf) modes)))
                      (or buffer-list (doom-buffer-list)))))


(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun set-company-backend! (modes &rest backends)
  "Prepends BACKENDS (in order) to `company-backends' in MODES.

MODES should be one symbol or a list of them, representing major or minor modes.
This will overwrite backends for MODES on consecutive uses.

If the car of BACKENDS is nil, unset the backends for MODES.

Examples:

  (set-company-backend! 'js2-mode 'company-tide 'company-yasnippet)
  (set-company-backend! 'sh-mode
    '(company-shell :with company-yasnippet))
  (set-company-backend! 'js2-mode
    '(:separate company-irony-c-headers company-irony))
  (set-company-backend! 'sh-mode nil)"
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (let ((hook (intern (format "%s-hook" mode)))
          (fn   (intern (format "+company|init-%s" mode))))
      (cond ((null (car-safe backends))
             (remove-hook hook fn)
             (unintern fn nil))
            ((fset fn
                   (lambda ()
                     (when (or (eq major-mode mode)
                               (and (boundp mode) (symbol-value mode)))
                       (require 'company)
                       (make-local-variable 'company-backends)
                       (dolist (backend (reverse backends))
                         (cl-pushnew backend company-backends
                                     :test (if (symbolp backend) #'eq #'equal))))))
             (add-hook hook fn))))))

(use-package flycheck
  :defer 1
  :diminish (flycheck-mode . "Fly")
  :hook (after-init . global-flycheck-mode))

(defun ms|setup-indentation (n)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset n) ; java/c/c++
  (setq coffee-tab-width n) ; coffeescript
  (setq javascript-indent-level n) ; javascript-mode
  (setq js-indent-level n) ; js-mode
  (setq js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq js-switch-indent-offset n)
  (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq css-indent-offset n))

;; Coffeescript
(use-package coffee-mode
  :mode "\\.coffee\\.*"
  :config
  (defun coffee-indent ()
    (if (coffee-line-wants-indent)
        (coffee-insert-spaces (+ (coffee-previous-indent) coffee-tab-width))
      (coffee-insert-spaces (coffee-previous-indent))))
  (add-λ 'coffee-mode-hook
    (setq-local indent-line-function #'coffee-indent)))

;; Cucumber
(use-package feature-mode
  :mode "\\.feature")

;; Ruby / Rails

(defvar +ruby-rbenv-versions nil
  "Available versions of ruby in rbenv.")

(defvar-local +ruby-current-version nil
  "The currently active ruby version.")

(use-package enh-ruby-mode
  :mode "\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|gemspec\\)\\'"
  :interpreter "ruby"
  :init

  (if (not (executable-find "rbenv"))
      (setq-default +ruby-current-version (string-trim (shell-command-to-string "ruby --version 2>&1 | cut -d' ' -f2")))
    (setq +ruby-rbenv-versions (split-string (shell-command-to-string "rbenv versions --bare") "\n" t))

    (defun ms|ruby|detect-rbenv-version ()
      "Detect the rbenv version for the current project and set the relevant
environment variables."
      (when-let* ((version-str (shell-command-to-string "RBENV_VERSION= ruby --version 2>&1 | cut -d' ' -f2")))
        (setq version-str (string-trim version-str)
              +ruby-current-version version-str)
        (when (member version-str +ruby-rbenv-versions)
          (setenv "RBENV_VERSION" version-str))))
    (add-hook 'enh-ruby-mode-hook #'ms|ruby|detect-rbenv-version))
  (add-hook 'enh-ruby-mode-hook (setq-local projectile-tags-command "ripper-tags -R -f TAGS")))

(use-package inf-ruby
  :after enh-ruby-mode
  :hook (enh-ruby-mode . inf-ruby-console-auto)
  :init
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter))

(use-package ruby-tools
  :diminish
  :after enh-ruby-mode
  :hook (enh-ruby-mode . ruby-tools-mode))

(use-package projectile-rails
  :init (projectile-rails-global-mode))

(use-package ruby-hash-syntax
  :bind (:map enh-ruby-mode-map ("C-c C-:" . ruby-hash-syntax-toggle)))

(defun ms|ruby|cleanup-robe-servers ()
  "Clean up dangling inf robe processes if there are no more `enh-ruby-mode' buffers open."
  ;; FIXME This should wait X seconds before cleaning up
  (unless (or (not robe-mode) (doom-buffers-in-mode 'enh-ruby-mode))
    (let (inf-buffer kill-buffer-query-functions)
      (while (setq inf-buffer (robe-inf-buffer))
        (let ((process (get-buffer-process inf-buffer))
              confirm-kill-processes)
          (when (processp process)
            (kill-process (get-buffer-process inf-buffer))
            (kill-buffer inf-buffer)))))))

(use-package robe
  :hook enh-ruby-mode
  :init
  (defun ms|ruby|init-robe ()
    (when (executable-find "ruby")
      (cl-letf (((symbol-function #'yes-or-no-p) (lambda (_) t)))
        (save-window-excursion
          (ignore-errors (robe-start)))
        (when (robe-running-p)
          (add-hook 'kill-buffer-hook #'+ruby|cleanup-robe-servers nil t)))))
  (add-hook 'enh-ruby-mode-hook #'ms/init-robe)
  (set-company-backend! 'robe-mode 'company-robe))

(use-package company-inf-ruby
  :after inf-ruby
  :config
  :config (set-company-backend! 'inf-ruby-mode 'company-inf-ruby))

(use-package rbenv
  :after enh-ruby-mode
  :init (global-rbenv-mode t))

(use-package rspec-mode
  :mode ("/\\.rspec\\'" . text-mode))

(use-package rubocop
  :hook enh-ruby-mode)

;; Javascript/JSX

(use-package eslint-fix)

(use-package js2-mode
  :custom
  (js2-include-node-externs t)
  (js2-include-browser-externs t)
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil)
  (js2-mode-show-parse-errors t)
  (js2-mode-show-strict-warnings t)
  (js-switch-indent-offset 2)
  :config
  (js2-imenu-extras-mode))

(use-package rjsx-mode :mode ("\\.jsx?\\'" . rjsx-mode))

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
  :diminish js2-refactor-mode
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

;; (use-package tide
  ;; :hook
  ;; (js2-mode . setup-tide-mode))

(use-package indium
  :diminish (indium-interaction-mode . "In" )
  :hook (js2-mode . indium-interaction-mode))

(use-package prettier-js
   :after add-node-modules-path
   :hook (js2-mode . prettier-js-mode))

;; Web mode
(use-package web-mode
  :mode "\\.p?html?$"
  :mode "\\.\\(?:tpl\\|blade\\)\\(\\.php\\)?$"
  :mode "\\.erb$"
  :mode "\\.jsp$"
  :mode "\\.as[cp]x$"
  :mode "\\.hbs$"
  :mode "\\.mustache$"
  :mode "\\.tsx$"
  :mode "\\.vue$"
  :mode "\\.twig$"
  :mode "\\.jinja$"
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-attr-value-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-comment-keywords t)
  (web-mode-enable-current-element-highlight t))

(use-package company-web
  :after web-mode
  :config
  (set-company-backend! 'web-mode 'company-web-html))

(use-package emmet-mode
  :hook (web-mode sgml-mode html-mode css-mode))

(use-package rainbow-mode
  :hook css-mode)

(ms|setup-indentation 2)

(provide 'setup-langs)
;;; setup-langs.el ends here
