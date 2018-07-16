;; Javascript/JSX
(setq js-switch-indent-offset 2)

(use-package eslint-fix)

(use-package js2-mode
  :init
  (setq js2-include-node-externs t)
  (setq js2-include-browser-externs t)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
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

;; Web mode
(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.blade\\.php\\'" . web-mode)
   ("\\.jsp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.ejs\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
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

(provide 'setup-langs)
