(setq doom-font (font-spec :family "Monaco" :size 12))
(setq doom-theme 'doom-molokai)

(defun set-custom-keys ()
    (map! "C-s" #'counsel-grep-or-swiper
      "C-x g" #'magit-status
      (:leader  :prefix "p"
        :n "s" #'counsel-projectile-rg)))

(defun icn-commit-message-template (&rest discard)
  "Insert Card ID from branch"
  (interactive)
  (insert (magit-get-current-branch))
  (beginning-of-line)
  (kill-word 1)
  (delete-char 1)
  (forward-word)
  (kill-line)
  (beginning-of-line)
  (insert "[")
  (upcase-word 1)
  (insert "] ")
  (end-of-line)
  (evil-append 0))

(add-hook 'git-commit-setup-hook '(lambda () (run-with-timer 0.4 nil #'icn-commit-message-template)) t)

(defun ms/config-company ()
  (setq company-idle-delay 0))

(defun ms/ag-rg-config ()
  (interactive)
  (setq counsel-ag-base-command "ag -S --nogroup --nocolor --ignore tmp --ignore icn_docker %s ")
  (setq counsel-rg-base-command "rg -S --no-heading --color never -g '!{icn_docker,tmp}/*' %s "))

(defun ms/config ()
  (interactive)
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil)
  (setq projectile-enable-caching t)
  (setq projectile-keymap-prefix (kbd "C-c p"))
  (ms/ag-rg-config))

(defun ivy-config ()
  (interactive)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-extra-directories nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'abbreviate)
  (setq ivy-format-function #'ivy-format-function-arrow)
  (setq ivy-display-style 'fancy)
  (setq ivy-use-selectable-prompt t))

(defun cursor-config ()
  (blink-cursor-mode -1)
  (setq evil-normal-state-cursor 'box)
  (setq evil-insert-state-cursor 'box)
  (setq evil-visual-state-cursor 'box)
  (setq evil-motion-state-cursor 'box)
  (setq evil-replace-state-cursor 'box)
  (setq evil-operator-state-cursor 'box))

(defun ms/apply-config ()
  (interactive)
  (run-with-timer 0.1 nil #'ivy-config)
  (run-with-timer 0.1 nil #'cursor-config)
  (run-with-timer 0.1 nil #'ms/config)
  (run-with-timer 0.1 nil #'set-custom-keys))

(add-hook 'doom-post-init-hook #'ms/apply-config)
