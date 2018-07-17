(defun my-setup-indent (n)
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

;; Javascript/JSX
(setq js-switch-indent-offset 2)

(use-package eslint-fix)

(use-package js2-mode
  :custom
  (js2-include-node-externs t)
  (js2-include-browser-externs t)
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil)
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

;; Web mode
(use-package web-mode
  :mode ( ("\\.erb\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.ejs\\'" . web-mode)
   ("\\.mustache\\'" . web-mode))
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
    (add-to-list 'company-backends 'company-web-jade)
    (add-to-list 'company-backends 'company-web-slim))))

(use-package emmet-mode
  :hook (web-mode sgml-mode html-mode css-mode))

(use-package rainbow-mode
  :pin gnu
  :hook css-mode)

(setq indent-tabs-mode nil)
(my-setup-indent 2)

(provide 'setup-langs)
