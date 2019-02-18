
;;; -*- lexical-binding: t -*-

;; ====
;; INIT

(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

(defvar *is-mac* (eq system-type 'darwin))
(defvar *is-linux* (eq system-type 'gnu/linux))

(setq inhibit-startup-screen t
      package-archives '(("melpa" . "https://melpa.milkbox.net/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(eval-when-compile
  (require 'package)
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

;; ;; Package system and sources.
;; (require 'package)
;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                  (not (gnutls-available-p))))
;;     (proto (if no-ssl "http" "https")))
;;     ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
;;     (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
;;     ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
;;     (when (< emacs-major-version 24)
;;     ;; For important compatibility libraries like cl-lib
;; (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

;; Pass system shell environment to Emacs. This is important primarily for shell inside Emacs,
;; but also things like Org mode export to Tex PDF don't work, since it relies on running external command pdflatex, which is loaded from PATH.
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Store custom-file separately, don't freak out when it's not found
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; remember risk variables (dir-locals)
(defun risky-local-variable-p (sym &optional _ignored) nil)

(use-package better-defaults)

;; =============
;; MODIFIER KEYS

;; Both command keys are 'Super'
(setq mac-right-command-modifier 'super)
(setq mac-command-modifier 'super)

;; Option or Alt is naturally 'Meta'
(setq mac-option-modifier 'meta)

;; Right Alt (option) can be used to enter symbols like em dashes '—' and euros '€' and stuff.
(setq mac-right-option-modifier 'nil)

;; Control is control, and you also need to change Caps Lock to Control in the Keyboard
;; preferences in macOS.

;; =============
;; SANE DEFAULTS

;; Smoother and nicer scrolling
(setq scroll-margin 5
      scroll-step 1
      next-line-add-newlines nil
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(when *is-linux* (setq x-select-enable-primary nil
                       x-select-enable-clipboard t
                       interprogram-paste-function 'x-cut-buffer-or-selection-value))

(setq tab-always-indent 'complete)

;; Don't bother with auto save and backups.
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Always prefer newer files
(setq load-prefer-newer t)

;; Warn only when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Move file to trash instead of removing.
(setq-default delete-by-moving-to-trash t)

;; Revert (update) buffers automatically when underlying files are changed externally.
(global-auto-revert-mode t)

(setq
 inhibit-startup-message t         ; Don't show the startup message...
 inhibit-startup-screen t          ; ... or screen
 cursor-in-non-selected-windows t  ; Hide the cursor in inactive windows

 echo-keystrokes 0.1               ; Show keystrokes right away, don't show the message in the scratch buffer
 sentence-end-double-space nil     ; Sentences should end in one space, come on!
 help-window-select t              ; Select help window so it's easy to quit it with 'q'
)

;; never kill *scratch* buffer
(add-hook 'kill-buffer-query-functions
          (lambda() (not (equal (buffer-name) "*scratch*"))))

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)      ; y and n instead of yes and no everywhere else
(delete-selection-mode 1)          ; Delete selected text when typing
(global-unset-key (kbd "s-p"))     ; Don't print

(setq ring-bell-function 'ignore)

;; Delete trailing spaces and add new line in the end of a file on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

;; Linear undo and redo.
(use-package undo-tree
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/tmp/undo"))
          undo-tree-auto-save-history t
          undo-tree-visualizer-timestamps t
          undo-tree-visualizer-diff t)))

(global-subword-mode 1)

;; =======
;; VISUALS

;; Enable transparent title bar on macOS
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . light)) ;; {light, dark}
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; Font
(setq +font-face "Monaco")
(setq +font-size 13)
;; (setq mac-allow-anti-aliasing nil)

(defun +set-font ()
  "Set fot according to +font-face and +font-size."
  (interactive)
  (when (member +font-face (font-family-list))
    (set-face-attribute 'default nil
                        :font (concat +font-face " "
                                      (number-to-string +font-size)))))

(+set-font)

;; Nice and simple default light theme.
(setq custom-theme-directory (concat user-emacs-directory "themes"))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

(defun +disable-themes ()
  "Disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  "Disable active themes before loading the new theme."
  (+disable-themes))

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))
(use-package dracula-theme)
(use-package solarized-theme)

(load-theme 'default-black t)

;; Pretty icons
(use-package all-the-icons)
;; MUST DO M-x all-the-icons-install-fonts after

;; Hide toolbar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(when *is-linux* (menu-bar-mode -1))

;; Disable blinking cursor.
(blink-cursor-mode 0)

;; Always wrap lines
(global-visual-line-mode 1)

;; Show line numbers
(global-display-line-numbers-mode 1)
(define-key global-map (kbd "C-x l") 'global-display-line-numbers-mode)

;; Highlight current line
(global-hl-line-mode 1)

(setq-default fill-column 80)
(setq visual-fill-column-center-text t
      visual-fill-column-width (+ 6 fill-column))

;; Show parens and other pairs.
(use-package smartparens
  :config
  (require 'smartparens-config)
  (require 'smartparens-ruby)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

;; Hide minor modes from modeline
(use-package rich-minority
  :config
  (rich-minority-mode t)
  (setf rm-blacklist ""))

;; Display dir if two files have the same name
(use-package uniquify
  :ensure nil
  :init
  (progn
    (setq uniquify-buffer-name-style 'reverse
          uniquify-separator "|"
          uniquify-after-kill-buffer-p t
          uniquify-ignore-buffers-re "^\\*")))

;; ;; Set colors to distinguish between active and inactive windows
;; (set-face-attribute 'mode-line nil :background "SlateGray1")
;; (set-face-attribute 'mode-line-inactive nil :background "grey93")

(use-package treemacs)

;; Show full path in the title bar.
;; (setq-default frame-title-format "%b (%f)")
(setq-default frame-title-format "")
(setq ns-use-proxy-icon nil)

;; Never use tabs, use spaces instead.
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq c-default-style "linux")
(setq standard-indent 2)
(setq-default c-basic-offset 2)
(setq-default tab-width 2)
(setq-default c-basic-indent 2)

;; Show keybindings cheatsheet
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package restart-emacs)

;; ================
;; BASIC NAVIGATION

;; Kill line with CMD-Backspace. Note that thanks to Simpleclip, killing doesn't rewrite the system clipboard.
;; Kill one word with Alt+Backspace.
;; Kill forward word with Alt-Shift-Backspace.
(global-set-key (kbd "s-<backspace>") 'kill-whole-line)
(global-set-key (kbd "M-S-<backspace>") 'kill-word)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)

;; Many commands in Emacs write the current position into mark ring.
;; These custom functions allow for quick movement backward and forward.
;; For example, if you were editing line 6, then did a search with Cmd+f, did something and want to come back,
;; press Cmd+, to go back to line 6. Cmd+. to go forward.
;; These keys are chosen because they are the same buttons as < and >, think of them as arrows.
;; (defun my-pop-local-mark-ring ()
;;   (interactive)
;;   (set-mark-command t))

;; (defun unpop-to-mark-command ()
;;   "Unpop off mark ring. Does nothing if mark ring is empty."
;;   (interactive)
;;   (when mark-ring
;;     (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
;;     (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
;;     (when (null (mark t)) (ding))
;;     (setq mark-ring (nbutlast mark-ring))
;;     (goto-char (marker-position (car (last mark-ring))))))

;; (global-set-key (kbd "s-,") 'my-pop-local-mark-ring)
;; (global-set-key (kbd "s-.") 'unpop-to-mark-command)

(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(setq set-mark-command-repeat-pop t)

;; ============
;; TEXT EDITING

(setq disabled-command-function nil)

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-'" . er/contract-region)))

;; Move-text lines around with meta-up/down.
(use-package move-text
  :config
  (move-text-default-bindings))

(use-package smart-newline
  :bind
  ("<s-return>" . eol-then-smart-newline)
  :hook
  (prog-mode . maybe-enable-smart-newline-mode)
  :init
  (defun smart-newline-no-reindent-first (orig-fun &rest args)
    (cl-letf (((symbol-function 'reindent-then-newline-and-indent) #'newline-and-indent))
      (apply orig-fun args)))
  (defun maybe-enable-smart-newline-mode ()
    (when (not (member major-mode indent-sensitive-modes))
      (smart-newline-mode))
    (advice-add 'smart-newline :around #'smart-newline-no-reindent-first))
  (def eol-then-smart-newline
    (move-end-of-line nil)
    (smart-newline)))

;; Quickly insert new lines above or below the current line, with correct indentation.
(defun smart-open-line ()
  "Insert an empty line after the current line. Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun smart-open-line-above ()
  "Insert an empty line above the current line. Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "s-<return>") 'smart-open-line)            ;; Cmd+Return new line below
(global-set-key (kbd "s-S-<return>") 'smart-open-line-above)    ;; Cmd+Shift+Return new line above

;; Upcase and lowercase word or region, if selected.
;; To capitalize or un-capitalize word use Alt+c and Alt+l
(global-set-key (kbd "M-u") 'upcase-dwim)   ;; Alt+u upcase
(global-set-key (kbd "M-l") 'downcase-dwim) ;; Alt-l lowercase

;; Visually find and replace text
(use-package visual-regexp
  :config
  (define-key global-map (kbd "s-r") 'vr/replace))  ;; Cmd+r find and replace

(use-package multiple-cursors
  :bind (("s-d" . mc/mark-next-like-this)
         ("s-D" . mc/mark-previous-like-this)
         ("C-c s-d" . mc/mark-all-like-this-dwim)))

;; This is rather radical, but saves from a lot of pain in the ass.
;; When split is automatic, always split windows vertically
(setq split-height-threshold 0)
(setq split-width-threshold nil)

;; Move between windows with Control-Command-Arrow and with =Cmd= just like in iTerm.
(use-package windmove
  :bind (("S-<left>" . windmove-left)
         ("S-<right>" . windmove-right)
         ("S-<up>" . windmove-up)
         ("S-<down>" . windmove-down)))

;; Enable winner mode to quickly restore window configurations
(winner-mode t)

;; ==================
;; PROJECT MANAGEMENT

;; Use Projectile for project management.
(use-package projectile
  :config
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-enable-caching t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode t))

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (use-package dired-x
    :ensure nil)

  (setq dired-dwim-target t)
  (--each '(dired-do-rename
            dired-do-copy
            dired-create-directory
            wdired-abort-changes)
    (eval `(defadvice ,it (after revert-buffer activate)
             (revert-buffer))))
  (defun dired-back-to-start-of-files ()
    (interactive)
    (backward-char (- (current-column) 2)))

  (define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
  (define-key dired-mode-map (kbd "k") 'dired-do-delete)

  (put 'dired-find-alternate-file 'disabled nil)

  ;; Delete with C-x C-k to match file buffers and magit
  (define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)

  (eval-after-load "wdired"
    '(progn
       (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
       (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
       (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom))))

;; ==========================================
;; MENUS AND COMPLETION (not code completion)

;; Use minimalist Ivy for most things.
(use-package ivy
  :config
  (ivy-mode t)                          ;; enable Ivy everywhere
  (setq ivy-use-virtual-buffers t)      ;; show bookmarks and recent files in buffer list
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  (setq confirm-nonexistent-file-or-buffer t)

  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (setq ivy-initial-inputs-alist nil)

  ;; (global-set-key (kbd "s-b") 'ivy-switch-buffer)  ;; Cmd+b show buffers and recent files
  ;; (global-set-key (kbd "C-k") 'ivy-immediate-done)
  (global-set-key (kbd "M-s-b") 'ivy-resume))         ;; Alt+Cmd+b resume whatever Ivy was doing


;; Swiper is a better local finder.
(use-package swiper
  :after ivy
  :config
  (global-set-key "\C-s" 'swiper)       ;; Default Emacs Isearch forward...
  (global-set-key "\C-r" 'swiper))       ;; ... and Isearch backward replaced with Swiper

;; Better menus with Counsel (a layer on top of Ivy)
(use-package counsel
  :after ivy
  :custom
  (counsel-ag-base-command "ag -S --nogroup --nocolor --ignore tmp --ignore icn_react/static --ignore icn_docker --ignore lib/assets %s ")
  (counsel-rg-base-command "rg -S --no-heading --color never -g '!{icn_docker,tmp}/*' %s ")
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "s-x") 'counsel-M-x)
  (global-set-key (kbd "C-x f") 'counsel-recentf)  ;; Replace built-in Emacs 'find file' (open file) with Counsel
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-i") 'counsel-imenu))

(use-package smex)  ;; show recent commands when invoking Alt-x (or Cmd+Shift+p)
(use-package flx)   ;; enable fuzzy matching

(use-package avy
  :bind ("C-c C-SPC" . avy-goto-char))

(use-package ace-window
  :bind (([other-window] . ace-window)))

;; ;; Make Ivy a bit more friendly by adding information to ivy buffers, e.g. description of commands in Alt-x, meta info when switching buffers, etc.
;; (use-package ivy-rich
;;   :config
;;   (ivy-rich-mode t)
;;   (setq ivy-rich-path-style 'abbrev)) ;; Abbreviate paths using abbreviate-file-name (e.g. replace “/home/username” with “~”)


;; Integrate Projectile with Counsel
(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

(setq projectile-completion-system 'ivy)             ;; Use Ivy in Projectile

;; ========================
;; VERSION CONTROL WITH GIT

;; Magit
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x m" . #'magit-status-fullscreen)
         ("C-x C-b" . magit-blame-addition))
  :config
  (setq magit-diff-refine-hunk 'all)
  (setq magit-log-auto-more t)
  (setq magit-completing-read-function 'ivy-completing-read)

  (set-default 'magit-push-always-verify nil)
  (set-default 'magit-revert-buffers 'silent)
  (set-default 'magit-no-confirm '(stage-all-changes
                                   unstage-all-changes))

  (defun magit-status-fullscreen (prefix)
    (interactive "P")
    (magit-status)
    (unless prefix
      (delete-other-windows)))

  (defun +magit-display-buffer (buffer)
    "Like `magit-display-buffer-fullframe-status-v1' with two differences:

1. Magit sub-buffers that aren't spawned from a status screen are opened as
   popups.
2. The status screen isn't buried when viewing diffs or logs from the status
   screen.

   Taken from doom-emacs
   "
    (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
      (display-buffer
       buffer (cond
               ;; If opened from an eshell window or popup, use the same window.
               ((or (derived-mode-p 'eshell-mode)
                    (eq (window-dedicated-p) 'side))
                '(display-buffer-same-window))
               ;; Open target buffers below the current one (we want previous
               ;; magit windows to be visible; especially magit-status).
               ((or (bound-and-true-p git-commit-mode)
                    (derived-mode-p 'magit-mode))
                (let ((size (if (eq buffer-mode 'magit-process-mode)
                                0.35
                              0.7)))
                  `(display-buffer-below-selected
                    . ((window-height . ,(truncate (* (window-height) size)))))))
               ;; log/stash/process buffers, unless opened from a magit-status
               ;; window, should be opened in popups.
               ((memq buffer-mode '(magit-process-mode
                                    magit-log-mode
                                    magit-stash-mode))
                '(display-buffer-below-selected))
               ;; Last resort: use current window
               ('(display-buffer-same-window))))))

  ;; (setq magit-display-buffer-function #'+magit-display-buffer)
  ;; (setq magit-popup-display-buffer-action '(+magit-display-popup-buffer))

  (defun enforce-git-commit-conventions ()
    "See https://chris.beams.io/posts/git-commit/"
    (setq fill-column 72
          git-commit-summary-max-length 50
          git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line)))
  (add-hook 'git-commit-mode-hook #'enforce-git-commit-conventions))


(use-package git-timemachine)

(use-package diff-hl
  :config
  (global-diff-hl-mode t))

;; ===============
;; CODE COMPLETION

(use-package company
  :custom
  (company-idle-delay 0.1)
  (company-global-modes '(not org-mode))
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 20)
  (company-echo-delay 0)
  (company-tooltip-flip-when-above t)
  (company-require-match nil)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-occurrence-weight-function #'company-occurrence-prefer-any-closest)
  (company-transformers '(company-sort-prefer-same-case-prefix))
  (company-dabbrev-minimum-length 2)
  (company-dabbrev-code-modes t)
  (company-dabbrev-code-everywhere t)
  :init
  (global-company-mode 1)
  (setq company-continue-commands
        (append company-continue-commands
                '(comint-previous-matching-input-from-input
                  comint-next-matching-input-from-input))))

(use-package company-posframe
  :after company
  :config (company-posframe-mode))

(use-package company-quickhelp
  :after company
  :config
  (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)
  (company-quickhelp-mode 1))

;; Set the company completion vocabulary to css and html when in web-mode.
(defun my-web-mode-hook ()
  (set (make-local-variable 'company-backends)
       '(company-css company-web-html company-yasnippet company-files)))

;; ===========
;; PROGRAMMING

(defvar indent-sensitive-modes '())

(defvar org-folder "~/Sync/org")

(use-package org
  :demand t
  :mode (("\\.org$" . org-mode))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))

  :config
  (setq org-startup-indented t)         ;; Visually indent sections. This looks better for smaller files.
  (setq org-src-tab-acts-natively t)    ;; Tab in source blocks should act like in major mode
  (setq org-src-preserve-indentation t)
  (setq org-log-into-drawer t)          ;; State changes for todos and also notes should go into a Logbook drawer
  (setq org-src-fontify-natively t)     ;; Code highlighting in code blocks
  (setq org-log-done 'time)             ;; Add closed date when todo goes to DONE state
  (setq org-support-shift-select t)     ;; Allow shift selection with arrows.
  (setq org-directory org-folder
        org-default-notes-file (concat org-folder "/todo.org"))
  (setq org-agenda-files '(org-folder))

  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline (lambda () (concat org-folder "/todo.org")) "Todo")
           "* TODO %? \n  %^t")
          ("i" "Idea" entry (file+headline (lambda () (concat org-folder "/ideas.org")) "Ideas")
           "* %? \n %U")
          ("e" "Tweak" entry (file+headline (lambda () (concat org-folder "/tweaks.org")) "Tweaks")
           "* %? \n %U")
          ("l" "Learn" entry (file+headline (lambda () (concat org-folder "/learn.org" )) "Learn")
           "* %? \n")
          ("w" "icn" entry (file+headline (lambda () (concat org-folder "/icn.org")) "Work")
           "* %? \n")
          ("m" "Check movie" entry (file+headline (lambda () (concat org-folder "/check.org")) "Movies")
           "* %? %^g")
          ("n" "Check book" entry (file+headline (lambda () (concat org-folder "/check.org")) "Books")
           "* %^{book name} by %^{author} %^g")))

  (use-package org-projectile
    :bind ("C-c n p" . org-projectile-project-todo-completing-read)
    :config
    (setq org-projectile-projects-file "~/Sync/org/project_todos.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

  (use-package org-bullets
    :config
    (setq org-hide-leading-stars t)
    (add-hook 'org-mode-hook
              (lambda ()
                (org-bullets-mode t)))))

(use-package markdown-mode)
(use-package web-mode
  :config
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-markup-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode)))

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode))
  :commands emmet-mode
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t))


;; Open config file by pressing C-x and then C
(global-set-key (kbd "C-x C") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; EVIL CONFIG

(use-package evil
  :disabled t
  :init
  (setq evil-want-C-u-scroll t
        evil-symbol-word-search t)

  change cursor to box
  (setq evil-normal-state-cursor 'box
        evil-insert-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-motion-state-cursor 'box
        evil-replace-state-cursor 'box
        evil-operator-state-cursor 'box)
  :config
  (evil-mode 1)
  (defadvice evil-scroll-page-down
      (after advice-for-evil-scroll-page-down activate)
    (evil-scroll-line-to-center (line-number-at-pos)))
  (defadvice evil-scroll-page-up
      (after advice-for-evil-scroll-page-up activate)
    (evil-scroll-line-to-center (line-number-at-pos)))
  (defadvice evil-search-next
      (after advice-for-evil-search-next activate)
    (evil-scroll-line-to-center (line-number-at-pos)))
  (defadvice evil-search-previous
      (after advice-for-evil-search-previous activate)
    (evil-scroll-line-to-center (line-number-at-pos)))

  (use-package evil-visualstar
    :after evil
    :commands (evil-visualstar/begin-search
               evil-visualstar/begin-search-forward
               evil-visualstar/begin-search-backward)
    :init
    (evil-define-key* 'visual 'global
                      "*" #'evil-visualstar/begin-search-forward
                      "#" #'evil-visualstar/begin-search-backward))

  (use-package evil-numbers
    :after evil
    :config
    (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

  (use-package evil-matchit
    :after evil
    :config
    (global-evil-matchit-mode 1))

  (use-package evil-surround
    :after evil
    :config
    (global-evil-surround-mode 1)))

(use-package wgrep)

(use-package yasnippet
  :config
  (yas-global-mode 1)

  ;; Jump to end of snippet definition
  (define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)

  ;; Inter-field navigationp
  (defun yas/goto-end-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas--snippets-at-point)))
           (position (yas--field-end (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-end-of-line 1)
        (goto-char position))))

  (defun yas/goto-start-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas--snippets-at-point)))
           (position (yas--field-start (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-beginning-of-line 1)
        (goto-char position))))

  ;; ;; No dropdowns please, yas
  ;; (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

  ;; No need to be so verbose
  (setq yas-verbosity 1)

  ;; Wrap around region
  (setq yas-wrap-around-region t)

  (define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
  (define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)

  (use-package yasnippet-snippets))

(use-package flycheck
  :config
  (setq-default flycheck-disabled-checkers '(less less-stylelin less-stylelintt))
  (setq flycheck-ruby-rubocop-executable "/Users/miguelsantos/.rbenv/versions/2.3.8/lib/ruby/gems/2.3.0/gems/rubocop-0.46.0/bin/rubocop")

  (global-flycheck-mode 1)

  (setq flycheck-indication-mode 'right-fringe)

  ;; A non-descript, left-pointing arrow
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center)

  (use-package flycheck-posframe
    :config
    (flycheck-posframe-mode 1)))

(use-package coffee-mode
  :mode "\\.coffee\\.*"
  :custom
  (coffee-args-repl '("-i" "--nodejs"))
  :config
  (add-to-list 'indent-sensitive-modes '(coffee-mode)))

(use-package js
  :custom
  (js-indent-level 2)
  (js-switch-indent-offset 2))

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter
  ("node" . js2-mode)
  :hook
  (js2-mode . js2-imenu-extras-mode)
  :custom
  (js2-highlight-level 3)
  :config
  (setenv "NODE_NO_READLINE" "1")

  (defun +use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (add-hook 'flycheck-mode-hook #'+use-eslint-from-node-modules))

(use-package nodejs-repl
  :defer t)

(use-package rjsx-mode
  :after js2-mode)

(use-package import-js
  :hook ((js2-mode . run-import-js)))

(use-package json-mode
  :mode (("\\.bowerrc$"     . json-mode)
         ("\\.jshintrc$"    . json-mode)
         ("\\.json_schema$" . json-mode))
  :config (setq js-indent-level 2))

(use-package ruby-mode
  :mode
  (("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . ruby-mode))
  :interpreter "ruby"
  :config
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil)
  ;; encoding comment
  (setq ruby-insert-encoding-magic-comment nil)
  (setq enh-ruby-add-encoding-comment-on-save nil)

  (defun hippie-expand-ruby-symbols (orig-fun &rest args)
    (if (eq major-mode 'ruby-mode)
        (let ((table (make-syntax-table enh-ruby-mode-syntax-table)))
          (modify-syntax-entry ?: "." table)
          (with-syntax-table table (apply orig-fun args)))
      (apply orig-fun args)))
  (advice-add 'hippie-expand :around #'hippie-expand-ruby-symbols)

  (add-λ 'enh-ruby-mode-hook
    (setq-local projectile-tags-command "ripper-tags -R -f TAGS")))

(defvar +ruby-rbenv-versions nil
  "Available versions of ruby in rbenv.")

(defvar-local +ruby-current-version nil
  "The currently active ruby version.")

(use-package rbenv
  :hook (ruby-mode . rbenv-use-corresponding)
  :config
  (setq +ruby-rbenv-versions (split-string (shell-command-to-string "rbenv versions --bare") "\n" t))
  (defun +detect-rbenv-version ()
    "Detect the rbenv version for the current project and set the relevant
environment variables."
    (interactive)
    (when-let* ((version-str (shell-command-to-string "ruby --version 2>&1 | cut -d' ' -f2")))
      (message version-str)
      (setq version-str (string-trim version-str)
            +ruby-current-version version-str)
      (when (member version-str +ruby-rbenv-versions)
        (setenv "RBENV_VERSION" version-str))))
  (add-hook 'enh-ruby-mode-hook #'+detect-rbenv-version)
  (global-rbenv-mode 1))

(use-package ruby-tools
  :after ruby-mode)

(use-package robe
  :hook (ruby-mode . robe-mode)
  :bind (([remap evil-jump-to-tag] . robe-jump))
  :config
  (after company
    (push 'company-robe company-backends)))

(use-package ruby-refactor
  :hook (ruby-mode . ruby-refactor-mode-lauch))

(use-package rubocop
  :hook (ruby-mode . rubocop-mode))

(use-package rspec-mode
  :bind
  ("s-R" . rspec-rerun)
  :after ruby-mode
  :config
  (with-eval-after-load 'yasnippet (rspec-install-snippets)))

(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-auto-enter)))

(use-package projectile-rails
  :config
  (setq projectile-rails-keymap-prefix (kbd "C-c r"))
  (projectile-rails-global-mode 1))

(use-package ruby-hash-syntax
  :after ruby-mode
  :bind
  (:map ruby-mode-map ("C-c C-:" . ruby-hash-syntax-toggle)))

(use-package feature-mode
  :bind (("C-c C-p" . #'ms/cycle-selenium-phantomjs))
  :mode "\\.feature$"
  :config

  (defun ms/change-to (new-mode)
    (kill-word 1)
    (insert new-mode))

  (defun ms/cycle-selenium-phantomjs ()
    "toggle selenium to javascript and other way around"
    (interactive)
    (save-excursion
      (goto-line 1)
      (if (looking-at "@selenium")
          (ms/change-to "@javascript")
        (if (looking-at "@javascript")
            (ms/change-to "@selenium"))))))

(use-package haml-mode
  :mode "\\.haml$")

(defun copy-buffer-file-name ()
  "Copy buffer's full path."
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(defun git-file-path (path)
  "File path in relation to git root."
  (let* ((root (file-truename (vc-git-root path)))
         (filename (file-name-nondirectory path))
         (filename-length (length filename)))
    (let ((chunk (file-relative-name path root)))
      (substring chunk 0 (- (length chunk) filename-length)))))

(global-set-key (kbd "s-f") #'copy-buffer-file-name)

;; =======
;; THE END
