;;; -*- lexical-binding: t -*-

;; ====
;; INIT

;;; Code:

(setq +original-gc-cons-threshold gc-cons-threshold)

(defvar +gc-cons-threshold 41943040 ; 40mb
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar +gc-cons-upper-limit 536870912 ; 512mb
  "The temporary value for `gc-cons-threshold' to defer it.")

(setq gc-cons-threshold +gc-cons-upper-limit)

(defun +restore-startup ()
  "Reset garbage collection settings"
  (run-with-idle-timer 3 nil (lambda ()
                               (setq-default gc-cons-threshold +gc-cons-threshold)
                               (add-hook 'focus-out-hook #'garbage-collect))))

(add-hook 'after-init-hook #'+restore-startup)

(defvar *is-mac* (eq system-type 'darwin))
(defvar *is-linux* (eq system-type 'gnu/linux))

;; Use the develop branch of straight.el
(setq straight-repository-branch "develop")

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(use-package blackout
  :straight (:host github :repo "raxod502/blackout")
  :demand t)

(use-feature straight-x
  ;; Add an autoload for this extremely useful command.
  :commands (straight-x-fetch-all))

(use-package no-littering
  :demand t)

;; (straight-use-package 'org)

(use-package el-patch
  :straight (:host github
                   :repo "raxod502/el-patch"
                   :branch "develop")
  :demand t)

;; TODO: remove
;; (setq use-package-always-ensure t)

(use-package s)
(use-package restart-emacs)

(use-feature server
  :init
  (if (not (server-running-p)) (server-start)))

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-check-startup-files nil)

  :init
  (when *is-mac*
    (setq exec-path
          (or (eval-when-compile
                (require 'cl-lib)
                (exec-path-from-shell-initialize)
                (cl-remove-duplicates exec-path :test #'string=))
              exec-path))))

;; Store custom-file separately, don't freak out when it's not found
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; remember risk variables (dir-locals)
(defun risky-local-variable-p (sym &optional _ignored) nil)

;; =============
;; MODIFIER KEYS

;; Both command keys are 'Super'
(when *is-mac*
  (setq mac-right-command-modifier 'super)
  (setq mac-command-modifier 'super)
  ;; Option or Alt is naturally 'Meta'
  (setq mac-option-modifier 'meta)
  ;; Right Alt (option) can be used to enter symbols like em dashes '—' and euros '€' and stuff.
  (setq mac-right-option-modifier 'nil))


(setq initial-scratch-message nil)
;; =============
;; SANE DEFAULTS

(setq default-directory "~/" )

;; Smoother and nicer scrolling
(setq scroll-margin 0
      scroll-step 0
      next-line-add-newlines nil
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(when *is-linux*
  (setq x-select-enable-primary nil
        x-select-enable-clipboard t
        interprogram-paste-function 'x-cut-buffer-or-selection-value))

(setq tab-always-indent 'complete)

;; Don't bother with auto save and backups.
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Always prefer newer files
(setq load-prefer-newer t)

;; Warn only when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Move file to trash instead of removing.
(setq-default delete-by-moving-to-trash t)

;; Revert (update) buffers automatically when underlying files are changed externally.
(global-auto-revert-mode +1)

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

(defalias 'yes-or-no-p 'y-or-n-p)      ; y and n instead of yes and no everywhere else
(delete-selection-mode 1)          ; Delete selected text when typing
(global-unset-key (kbd "s-p"))     ; Don't print

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

(global-subword-mode +1)

(recentf-mode +1)

;; =======
;; VISUALS

(when *is-linux* (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode +1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Always wrap lines
(global-visual-line-mode +1)

;; Highlight current line
(global-hl-line-mode 0)

;; Show line numbers
(global-display-line-numbers-mode t)
(define-key global-map (kbd "C-x l") 'global-display-line-numbers-mode)

; Enable transparent title bar on macOS
(when *is-mac*
  (add-to-list 'default-frame-alist '(ns-appearance . light)) ;; {light, dark}
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))

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

;; (use-package brutalist-theme
;;   :config
;;   (load-theme 'brutalist t))

(load-theme 'tsdh-light t)

(use-package leuven-theme
  :config
  (load-theme 'leuven t))

(fringe-mode '(2 . 2))

(setq +font "IBM Plex Mono 14")
(setq +font "Go Mono 14")
(setq +font "Consolas 15")
(setq +font "Menlo 14")
(setq +font "Roboto Mono 14")
(setq +font "Fira Code Retina 14")

(let ((font +font))
  (set-frame-font font)
  (add-to-list 'default-frame-alist
               `(font . ,font)))

;; Show parens and other pairs.

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1))

;; Hide minor modes from modeline
(use-package rich-minority
  :config
  (setf rm-blacklist "")
  (rich-minority-mode t))

;; Display dir if two files have the same name
(use-feature uniquify
  :init
  (progn
    (setq uniquify-buffer-name-style 'reverse
          uniquify-separator "|"
          uniquify-after-kill-buffer-p t
          uniquify-ignore-buffers-re "^\\*")))

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

(use-package restart-emacs)

;; ================
;; BASIC NAVIGATION

;; Kill line with CMD-Backspace. Note that thanks to Simpleclip, killing doesn't rewrite the system clipboard.
;; Kill one word with Alt+Backspace.
;; Kill forward word with Alt-Shift-Backspace.
(global-set-key (kbd "s-<backspace>") 'kill-whole-line)
(global-set-key (kbd "M-S-<backspace>") 'kill-word)

;; Many commands in Emacs write the current position into mark ring.
;; These custom functions allow for quick movement backward and forward.
;; For example, if you were editing line 6, then did a search with Cmd+f, did something and want to come back,
;; press Cmd+, to go back to line 6. Cmd+. to go forward.
;; These keys are chosen because they are the same buttons as < and >, think of them as arrows.
(defun my-pop-local-mark-ring ()
  (interactive)
  (set-mark-command t))

(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(global-set-key (kbd "s-,") 'my-pop-local-mark-ring)
(global-set-key (kbd "s-.") 'unpop-to-mark-command)

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
  :bind (("M-m" . #'er/expand-region)
         ("C-'" . #'er/contract-region)))

(use-package change-inner
  :bind (("M-i" . #'change-inner)
         ("M-o" . #'change-outer)))

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
(bind-key "M-c" #'capitalize-dwim)
(bind-key "M-l" #'downcase-dwim)
(bind-key "M-u" #'upcase-dwim)

;; Visually find and replace text
(use-package visual-regexp
  :bind (("M-%" . vr/query-replace)))

(use-package multiple-cursors
  :bind (("s-d" . mc/mark-next-like-this)
         ("s-D" . mc/mark-previous-like-this)
         ("C-c s-d" . mc/mark-all-like-this-dwim))
  :config
  (multiple-cursors-mode +1))

(use-package evil-mc
  :disabled t
  :config
  (global-evil-mc-mode +1))

(use-package crux
  :demand t
  :bind
  ("C-k" . crux-smart-kill-line)
  ("C-c n" . crux-cleanup-buffer-or-region)
  ("C-c f" . crux-recentf-find-file)
  ("C-a" . crux-move-beginning-of-line)
  ("C-c C-j" . crux-duplicate-current-line-or-region)
  :config
  (global-set-key (kbd "C-c C-c") (crux-with-region-or-line comment-or-uncomment-region)))

;; =================
;; WINDOW MANAGEMENT


;; This is rather radical, but saves from a lot of pain in the ass.
;; When split is automatic, always split windows vertically
(setq split-height-threshold 0)
(setq split-width-threshold nil)


;; Go to other windows easily with one keystroke Cmd-something.
(global-set-key (kbd "s-1") (kbd "C-x 1"))  ;; Cmd-1 kill other windows (keep 1)
(global-set-key (kbd "s-2") (kbd "C-x 2"))  ;; Cmd-2 split horizontally
(global-set-key (kbd "s-3") (kbd "C-x 3"))  ;; Cmd-3 split vertically
(global-set-key (kbd "s-0") (kbd "C-x 0"))  ;; Cmd-0...
(global-set-key (kbd "s-w") (kbd "C-x 0"))  ;; ...and Cmd-w to close current window


;; Move between windows with Control-Command-Arrow and with =Cmd= just like in iTerm.
(use-feature windmove
  :bind (("S-<left>" . windmove-left)
         ("S-<right>" . windmove-right)
         ("S-<up>" . windmove-up)
         ("S-<down>" . windmove-down)))

(use-feature winner
  :config
  (winner-mode 1))

;; ==================
;; PROJECT MANAGEMENT

(use-feature dired
  :after dash
  :hook (dired-mode . dired-hide-details-mode)
  :config

  (use-feature dired-x)


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

;; Use Projectile for project management.
(use-package projectile
  :defer 1

  :config
  (setq projectile-switch-project-action #'projectile-dired)
  ;; (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)

  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (global-set-key (kbd "C-x p s s") 'counsel-projectile-ag)

  (projectile-mode +1)

  :blackout t)

(use-package counsel-projectile
  :init

  (counsel-projectile-mode +1)

  :config

  ;; Sort files using `prescient', instead of just showing them in
  ;; lexicographic order.
  (setq counsel-projectile-sort-files t))



(use-package deadgrep
  :defer t
  :bind ("C-c h" . deadgrep))

;; ==========================================
;; MENUS AND COMPLETION (not code completion)

;;;; Candidate selection

; Package `ivy' provides a user interface for choosing from a list of
;; options by typing a query to narrow the list, and then selecting
;; one of the remaining candidates. This offers a significant
;; improvement over the default Emacs interface for candidate
;; selection.
(use-package ivy
  :init/el-patch

  (defvar ivy-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [remap switch-to-buffer]
        'ivy-switch-buffer)
      (define-key map [remap switch-to-buffer-other-window]
        'ivy-switch-buffer-other-window)
      map)
    "Keymap for `ivy-mode'.")

  (define-minor-mode ivy-mode
    (el-patch-concat
      "Toggle Ivy mode on or off.
Turn Ivy mode on if ARG is positive, off otherwise.
Turning on Ivy mode sets `completing-read-function' to
`ivy-completing-read'.
Global bindings:
\\{ivy-mode-map}
Minibuffer bindings:
\\{ivy-minibuffer-map}"
      (el-patch-add
        "\n\nTo make it easier to lazy-load `ivy', this function
sets `completion-in-region-function' regardless of the value of
`ivy-do-completion-in-region'."))
    :group 'ivy
    :global t
    :keymap ivy-mode-map
    (el-patch-remove
      :lighter " ivy")
    (if ivy-mode
        (progn
          (setq completing-read-function 'ivy-completing-read)
          (el-patch-splice 2
            (when ivy-do-completion-in-region
              (setq completion-in-region-function 'ivy-completion-in-region))))
      (setq completing-read-function 'completing-read-default)
      (setq completion-in-region-function 'completion--in-region)))

  (ivy-mode +1)

  :config

  ;; With enough packages loaded, it is easy to get commands like
  ;; `describe-symbol' to offer more than 30,000 candidates. Allow
  ;; sorting in these cases.
  (setq ivy-sort-max-size 50000)

  (global-set-key (kbd "C-c C-r") 'ivy-resume)

  :blackout t)

;; Package `ivy-hydra' provides the C-o binding for Ivy menus which
;; allows you to pick from a set of options for what to do with a
;; selected candidate.
(use-package ivy-hydra)

;; Package `counsel' provides purpose-built replacements for many
;; built-in Emacs commands that use enhanced configurations of `ivy'
;; to provide extra features.
(use-package counsel
  :init/el-patch

  (defvar counsel-mode-map
    (let ((map (make-sparse-keymap)))
      (dolist (binding
               '((execute-extended-command . counsel-M-x)
                 (describe-bindings . counsel-descbinds)
                 (el-patch-remove
                   (describe-function . counsel-describe-function)
                   (describe-variable . counsel-describe-variable))
                 (apropos-command . counsel-apropos)
                 (describe-face . counsel-describe-face)
                 (list-faces-display . counsel-faces)
                 (find-file . counsel-find-file)
                 (find-library . counsel-find-library)
                 (imenu . counsel-imenu)
                 (load-library . counsel-load-library)
                 (load-theme . counsel-load-theme)
                 (yank-pop . counsel-yank-pop)
                 (info-lookup-symbol . counsel-info-lookup-symbol)
                 (pop-to-mark-command . counsel-mark-ring)
                 (bookmark-jump . counsel-bookmark)))
        (define-key map (vector 'remap (car binding)) (cdr binding)))
      map)
    (el-patch-concat
      "Map for `counsel-mode'.
Remaps built-in functions to counsel replacements."
      (el-patch-add
        "\n\nBindings that are remapped by `helpful' have been removed.")))

  (defcustom counsel-mode-override-describe-bindings nil
    "Whether to override `describe-bindings' when `counsel-mode' is active."
    :group 'ivy
    :type 'boolean)

  (define-minor-mode counsel-mode
    "Toggle Counsel mode on or off.
Turn Counsel mode on if ARG is positive, off otherwise. Counsel
mode remaps built-in emacs functions that have counsel
replacements.
Local bindings (`counsel-mode-map'):
\\{counsel-mode-map}"
    :group 'ivy
    :global t
    :keymap counsel-mode-map
    (el-patch-remove
      :lighter " counsel")
    (if counsel-mode
        (progn
          (when (and (fboundp 'advice-add)
                     counsel-mode-override-describe-bindings)
            (advice-add #'describe-bindings :override #'counsel-descbinds))
          (define-key minibuffer-local-map (kbd "C-r")
            'counsel-minibuffer-history))
      (when (fboundp 'advice-remove)
        (advice-remove #'describe-bindings #'counsel-descbinds))))

  :init

  (counsel-mode +1)

  :bind* (;; Keybinding suggested by the documentation of Counsel, see
          ;; https://github.com/abo-abo/swiper.
          ("C-c k" . counsel-rg)
          ("s-x" . counsel-M-x))
  :config/el-patch

  (defcustom counsel-rg-base-command
    (el-patch-concat
      "rg -S --no-heading --line-number --color never "
      (el-patch-add
        "-z --sort path ")
      "%s .")
    (el-patch-concat
      "Alternative to `counsel-ag-base-command' using ripgrep.
Note: don't use single quotes for the regex."
      (el-patch-add
        "\n\nSupport for searching compressed files and for
reporting results in a deterministic order has been added by
`el-patch'."))
    :type 'string
    :group 'ivy)

  :blackout t)

(use-package prescient
  :config

  ;; Remember usage statistics across Emacs sessions.
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :demand t
  :after ivy
  :config

  ;; Use `prescient' for Ivy menus.
  (ivy-prescient-mode +1))

(use-feature isearch
  :config

  ;; Eliminate the 0.25s idle delay for isearch highlighting, as in my
  ;; opinion it usually produces a rather disjointed and distracting
  ;; UX.
  (setq isearch-lazy-highlight-initial-delay 0))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper))
  :config

  ;; Use only one color for subgroups in Swiper highlighting.
  (setq swiper-faces '(swiper-match-face-1
                       swiper-match-face-2
                       swiper-match-face-2
                       swiper-match-face-2)))


(use-package avy
  :bind (("C-=" . avy-goto-char)))

(use-package ace-window
  :bind (("C-x o" . ace-window)))

(use-package snails
  :straight (:host github :repo "manateelazycat/snails")
  :demand t)

;; ========================
;; VERSION CONTROL WITH GIT


;; Magit
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-b" . magit-blame-addition))
  :config
  (setq magit-diff-refine-hunk 'all)
  (setq magit-log-auto-more t)
  (setq magit-completing-read-function 'ivy-completing-read)

  (set-default 'magit-push-always-verify nil)
  (set-default 'magit-revert-buffers 'silent)
  (set-default 'magit-no-confirm '(stage-all-changes
                                   unstage-all-changes))

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

  (defun +magit-display-popup-buffer (buffer &optional alist)
    "TODO"
    (cond ((eq (window-dedicated-p) 'side)
           (if (fboundp '+popup-display-buffer-stacked-side-window)
               (+popup-display-buffer-stacked-side-window buffer alist)
             (display-buffer-in-side-window buffer alist)))
          ((derived-mode-p 'magit-mode)
           (display-buffer-below-selected buffer alist))
          ((display-buffer-in-side-window buffer alist))))

  (setq magit-display-buffer-function #'+magit-display-buffer)
  (setq magit-popup-display-buffer-action '(+magit-display-popup-buffer))

  (defun enforce-git-commit-conventions ()
    "See https://chris.beams.io/posts/git-commit/"
    (setq fill-column 72
          git-commit-summary-max-length 50
          git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line)))
  (add-hook 'git-commit-mode-hook #'enforce-git-commit-conventions))

(use-package git-timemachine)

;; Show changes in the gutter
(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode 't)
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)

  (set-face-background 'git-gutter:modified 'nil)   ;; background color
  (set-face-foreground 'git-gutter:added "green4")
  (set-face-foreground 'git-gutter:deleted "red"))

(use-package git-link)

;; ===============
;; CODE COMPLETION

(use-package company
  :defer 3
  :custom
  (company-idle-delay 0)
  (company-global-modes '(not org-mode))
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 70)
  (company-echo-delay 0)
  (company-tooltip-flip-when-above t)
  (company-require-match nil)
  (company-minimum-prefix-length 2)
  (compan-show-numbers t)
  (company-occurrence-weight-function #'company-occurrence-prefer-any-closest)
  (company-transformers '(company-sort-prefer-same-case-prefix))
  (company-dabbrev-minimum-length 0)
  (company-dabbrev-code-modes t)
  (company-dabbrev-code-everywhere t)

  ;; :bind
  ;; ([remap completion-at-point] . company-manual-begin)
  ;; ([remap complete-symbol] . company-manual-begin)

  :config
  (company-tng-configure-default)
  (setq company-frontends
        '(company-tng-frontend
          company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))

  (global-company-mode +1)

  (setq company-continue-commands
        (append company-continue-commands
                '(comint-previous-matching-input-from-input
                  comint-next-matching-input-from-input))))

(use-package company-quickhelp
  :after company
  :config
  (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)
  (company-quickhelp-mode t))

;; Package `company-prescient' provides intelligent sorting and
;; filtering for candidates in Company completions.
(use-package company-prescient
  :disabled t
  :demand t
  :after company
  :config

  ;; Use `prescient' for Company menus.
  (company-prescient-mode +1))

(use-package company-box
  :disabled t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-max-candidates 50)

  (defun +func1 (test)
    (string " "))

  (setq company-box-icons-functions '(+func1)))

(defvar he-search-loc-backward (make-marker))
(defvar he-search-loc-forward (make-marker))

(defun try-expand-dabbrev-closest-first (old)
  "Try to expand word \"dynamically\", searching the current buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string).  It returns t if a new expansion is found, nil otherwise."
  (let (expansion)
    (unless old
      (he-init-string (he-dabbrev-beg) (point))
      (set-marker he-search-loc-backward he-string-beg)
      (set-marker he-search-loc-forward he-string-end))

    (if (not (equal he-search-string ""))
        (save-excursion
          (save-restriction
            (if hippie-expand-no-restriction
                (widen))

            (let (forward-point
                  backward-point
                  forward-distance
                  backward-distance
                  forward-expansion
                  backward-expansion
                  chosen)

              ;; search backward
              (goto-char he-search-loc-backward)
              (setq expansion (he-dabbrev-search he-search-string t))

              (when expansion
                (setq backward-expansion expansion)
                (setq backward-point (point))
                (setq backward-distance (- he-string-beg backward-point)))

              ;; search forward
              (goto-char he-search-loc-forward)
              (setq expansion (he-dabbrev-search he-search-string nil))

              (when expansion
                (setq forward-expansion expansion)
                (setq forward-point (point))
                (setq forward-distance (- forward-point he-string-beg)))

              ;; choose depending on distance
              (setq chosen (cond
                            ((and forward-point backward-point)
                             (if (< forward-distance backward-distance) :forward :backward))

                            (forward-point :forward)
                            (backward-point :backward)))

              (when (equal chosen :forward)
                (setq expansion forward-expansion)
                (set-marker he-search-loc-forward forward-point))

              (when (equal chosen :backward)
                (setq expansion backward-expansion)
                (set-marker he-search-loc-backward backward-point))

              ))))

    (if (not expansion)
        (progn
          (if old (he-reset-string))
          nil)
      (progn
        (he-substitute-string expansion t)
        t))))

(defun try-expand-line-closest-first (old)
  "Try to complete the current line to an entire line in the buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (let ((expansion ())
        (strip-prompt (and (get-buffer-process (current-buffer))
                           comint-use-prompt-regexp
                           comint-prompt-regexp)))
    (unless old
      (he-init-string (he-line-beg strip-prompt) (point))
      (set-marker he-search-loc-backward he-string-beg)
      (set-marker he-search-loc-forward he-string-end))

    (if (not (equal he-search-string ""))
        (save-excursion
          (save-restriction
            (if hippie-expand-no-restriction
                (widen))

            (let (forward-point
                  backward-point
                  forward-distance
                  backward-distance
                  forward-expansion
                  backward-expansion
                  chosen)

              ;; search backward
              (goto-char he-search-loc-backward)
              (setq expansion (he-line-search he-search-string
                                              strip-prompt t))

              (when expansion
                (setq backward-expansion expansion)
                (setq backward-point (point))
                (setq backward-distance (- he-string-beg backward-point)))

              ;; search forward
              (goto-char he-search-loc-forward)
              (setq expansion (he-line-search he-search-string
                                              strip-prompt nil))

              (when expansion
                (setq forward-expansion expansion)
                (setq forward-point (point))
                (setq forward-distance (- forward-point he-string-beg)))

              ;; choose depending on distance
              (setq chosen (cond
                            ((and forward-point backward-point)
                             (if (< forward-distance backward-distance) :forward :backward))

                            (forward-point :forward)
                            (backward-point :backward)))

              (when (equal chosen :forward)
                (setq expansion forward-expansion)
                (set-marker he-search-loc-forward forward-point))

              (when (equal chosen :backward)
                (setq expansion backward-expansion)
                (set-marker he-search-loc-backward backward-point))

              ))))

    (if (not expansion)
        (progn
          (if old (he-reset-string))
          ())
      (progn
        (he-substitute-string expansion t)
        t))))

;; Hippie expand: sometimes too hip
(setq hippie-expand-try-functions-list '(try-expand-dabbrev-closest-first
                                         try-complete-file-name
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-expand-all-abbrevs
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; Create own function to expand lines (C-S-.)
(defun hippie-expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-expand-line-closest-first
                                            try-expand-line-all-buffers)))
    (end-of-line)
    (hippie-expand nil)))

;; Don't case-fold when expanding with hippe
(defun hippie-expand-no-case-fold ()
  (interactive)
  (let ((case-fold-search nil))
    (hippie-expand nil)))

(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'hippie-expand-try-functions-list)

(use-package eacl
  :ensure nil
  :straight (:host github :repo "redguardtoo/eacl")
  :demand t)

;; ===========
;; PROGRAMMING

(defvar indent-sensitive-modes '())

(use-package markdown-mode)

;; Web-mode is an autonomous emacs major-mode for editing web templates.
;; HTML documents can embed parts (CSS / JavaScript) and blocks (client / server side).
(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :config

  ;; Indent by two spaces by default.
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  ;; Autocomplete </ instantly.
  (setq web-mode-enable-auto-closing t))

;; Emmet
(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode))
  :commands emmet-mode
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t))

(defvar org-folder "~/Sync/org")

(use-package org
  :disabled t
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
  (setq org-directory "~/Sync/org"
        org-default-notes-file (concat org-folder "/todo.org"))
  (setq org-agenda-files '("~/Sync/org"))

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
    ;; :bind ("C-c n p" . org-projectile-project-todo-completing-read)
    :config
    (setq org-projectile-projects-file "~/Sync/org/project_todos.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

  (use-package org-bullets
    :config
    (setq org-hide-leading-stars t)
    (add-hook 'org-mode-hook
              (lambda ()
                (org-bullets-mode t)))))

;; Open config file by pressing C-x and then C
(global-set-key (kbd "C-x C") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(setq-default evil-want-C-u-scroll t)
(setq-default evil-want-C-d-scroll t)
(setq-default evil-symbol-word-search t)
(setq-default evil-esc-delay 0)

(use-package evil
  :init
  (setq evil-normal-state-cursor 'box
        evil-insert-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-motion-state-cursor 'box
        evil-replace-state-cursor 'box
        evil-operator-state-cursor 'box
        evil-emacs-state-cursor 'box)

  :config
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

  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

  (evil-mode t)

  (use-package evil-visualstar
    :commands (evil-visualstar/begin-search
               evil-visualstar/begin-search-forward
               evil-visualstar/begin-search-backward)
    :init
    (evil-define-key* 'visual 'global
                      "*" #'evil-visualstar/begin-search-forward
                      "#" #'evil-visualstar/begin-search-backward))

  (use-package evil-numbers
    :config
    (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

  (use-package evil-matchit
    :config
    (global-evil-matchit-mode 1))

  (use-package evil-surround
    :config
    (global-evil-surround-mode 1)))

(use-package wgrep)

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (use-package yasnippet-snippets)
  (global-set-key (kbd "C-c s") 'company-yasnippet))

(use-package flycheck
  :disabled t
  :config
  (setq-default flycheck-disabled-checkers '(less less-stylelin less-stylelintt))
  ;; (setq flycheck-ruby-rubocop-executable "bundle exec rubocop")

  (global-flycheck-mode +1)

  (setq flycheck-indication-mode 'left-fringe)
  ;; A non-descript, left-pointing arrow

  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center)

  (use-package flycheck-posframe
    :config
    (flycheck-posframe-mode 1)))

(use-package dumb-jump
  :demand t
  :bind* (("C-M-d" . dumb-jump-quick-look)
          ([remap evil-jump-to-tag] . dumb-jump-go))
  :config
  (setq dumb-jump-selector 'ivy))

(use-package lsp-mode
  :config
  (use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
    :config
    (setq lsp-prefer-flymake nil)
    (setq lsp-ui-doc-max-height 8)
    (setq lsp-ui-doc-max-width 35)
    (setq lsp-ui-sideline-ignore-duplicate t))

  (use-package company-lsp
    :config
    (setq company-transformers nil)
    (setq company-lsp-async t)
    (setq company-lsp-enable-recompletion t)
    (setq company-lsp-enable-snippet t)
    (push 'company-lsp company-backends)))

(use-package coffee-mode
  :mode "\\.coffee\\.*"
  :config
  (setq coffee-args-repl '("-i" "--nodejs"))
  (setq coffee-tab-width 2)
  (setq coffee-indent-like-python-mode t)

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

;; (use-package rjsx-mode
;;   :after js2-mode)

(use-package json-mode
  :mode (("\\.bowerrc$"     . json-mode)
         ("\\.jshintrc$"    . json-mode)
         ("\\.json_schema$" . json-mode))
  :config (setq js-indent-level 2))

(use-package ruby-mode
  :mode
  (("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . ruby-mode))
  :interpreter "ruby"
  :bind (("C-c C-d" . #'+ruby-add-debug-line))
  :config
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil)

  (setq ruby-insert-encoding-magic-comment nil)
  (setq enh-ruby-add-encoding-comment-on-save nil)

  (subword-mode -1)

  (defun +ruby-add-debug-line ()
    (interactive)
    (save-excursion
      (end-of-line)
      (smart-open-line)
      (insert "binding.pry")
      (indent-according-to-mode)))

  (defun hippie-expand-ruby-symbols (orig-fun &rest args)
    (if (eq major-mode 'ruby-mode)
        (let ((table (make-syntax-table ruby-mode-syntax-table)))
          (modify-syntax-entry ?: "." table)
          (with-syntax-table table (apply orig-fun args)))
      (apply orig-fun args)))
  (advice-add 'hippie-expand :around #'hippie-expand-ruby-symbols)

  (add-λ 'ruby-mode-hook
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
  (add-hook 'ruby-mode-hook #'+detect-rbenv-version)
  (global-rbenv-mode 1))

(use-package robe
  :hook (ruby-mode . robe-mode)
  :bind (([remap evil-jump-to-tag] . robe-jump)
         ("C-c C-h" . robe-doc))
  :config
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

(use-package rspec-mode
  :after ruby-mode
  :bind
  ("s-R" . rspec-rerun)
  :config
  (with-eval-after-load 'yasnippet
    (rspec-install-snippets)))

(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-auto-enter))
  :config
  (setq inf-ruby-console-environment "development")

  (defcustom +ruby-extensions-file
    "../console_extensions.rb"
    "File loaded when a ruby console is started.
Name is relative to the project root.")

  (defun +run-ruby-console ()
    (interactive)

    (let ((default-directory (projectile-project-root))
          (was-running (get-buffer-process inf-ruby-buffer)))
      ;; This function automatically decides between starting
      ;; a new console or visiting an existing one.
      (inf-ruby-console-auto)
      (when (and (not was-running)
                 (get-buffer-process (current-buffer))
                 (file-readable-p +ruby-extensions-file))
        ;; If this brand new buffer has lots of lines then
        ;; some exception probably happened.
        (send-string
         (get-buffer-process (current-buffer))
         (concat "require '" +ruby-extensions-file
                 "'\n")))))
  (global-set-key (kbd "C-c M-j") #'+run-ruby-console))

(use-package yari)

;; (use-package projectile-rails
;;   :after projectile
;;   :config
;;   (setq projectile-rails-keymap-prefix (kbd "C-c r"))
;;   (projectile-rails-global-mode 1))

(use-package feature-mode
  :bind (("C-c C-p" . #'+cycle-selenium-phantomjs)
         ("C-c C-d" . #'+feature-add-debug-line))
  :mode "\\.feature$"
  :config

  (defun +feature-add-debug-line ()
    (interactive)
    (save-excursion
      (end-of-line)
      (smart-open-line-above)
      (insert "And I debug with pry")
      (indent-according-to-mode)))

  (defun +change-to (new-mode)
    (kill-word 1)
    (insert new-mode))

  (defun +cycle-selenium-phantomjs ()
    "toggle selenium to javascript and other way around"
    (interactive)
    (save-excursion
      (goto-line 1)
      (if (looking-at "@selenium")
          (+change-to "@javascript")
        (if (looking-at "@javascript")
            (+change-to "@selenium"))))))

(use-package haml-mode
  :mode "\\.haml$")

(use-package go-mode)
(use-package go-eldoc)
(use-package go-guru)
(use-package gorepl-mode)
(use-package company-go
  :after go-mode
  :config
  (setq company-go-show-annotation t)
  (eval-after-load 'company
    '(push 'company-go company-backends)))

(use-package kotlin-mode)

(executable-find company-go-gocode-command)
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

(use-package string-inflection
  :config
  (global-set-key (kbd "C-c i") 'string-inflection-cycle)
  (global-set-key (kbd "C-c C") 'string-inflection-camelcase)
  (global-set-key (kbd "C-c L") 'string-inflection-lower-camelcase)
  (global-set-key (kbd "C-c J") 'string-inflection-java-style-cycle))

(use-package keyfreq
  :config
  (keyfreq-mode +1)
  (keyfreq-autosave-mode +1))

(use-package dash
  :config
  (use-package s
    :config
    (defun open-line-below ()
      (interactive)
      (end-of-line)
      (newline)
      (indent-for-tab-command))

    (defun open-line-above ()
      (interactive)
      (beginning-of-line)
      (newline)
      (forward-line -1)
      (indent-for-tab-command))

    (defun new-line-in-between ()
      (interactive)
      (newline)
      (save-excursion
        (newline)
        (indent-for-tab-command))
      (indent-for-tab-command))

    (defun new-line-dwim ()
      (interactive)
      (let ((break-open-pair (or (and (looking-back "{" 1) (looking-at "}"))
                                 (and (looking-back ">" 1) (looking-at "<"))
                                 (and (looking-back "(" 1) (looking-at ")"))
                                 (and (looking-back "\\[" 1) (looking-at "\\]")))))
        (newline)
        (when break-open-pair
          (save-excursion
            (newline)
            (indent-for-tab-command)))
        (indent-for-tab-command)))

    (defun duplicate-current-line-or-region (arg)
      "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
      (interactive "p")
      (if (region-active-p)
          (let ((beg (region-beginning))
                (end (region-end)))
            (duplicate-region arg beg end)
            (one-shot-keybinding "d" (λ (duplicate-region 1 beg end))))
        (duplicate-current-line arg)
        (one-shot-keybinding "d" 'duplicate-current-line)))

    (defun one-shot-keybinding (key command)
      (set-temporary-overlay-map
       (let ((map (make-sparse-keymap)))
         (define-key map (kbd key) command)
         map) t))

    (defun replace-region-by (fn)
      (let* ((beg (region-beginning))
             (end (region-end))
             (contents (buffer-substring beg end)))
        (delete-region beg end)
        (insert (funcall fn contents))))

    (defun duplicate-region (&optional num start end)
      "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
      (interactive "p")
      (save-excursion
        (let* ((start (or start (region-beginning)))
               (end (or end (region-end)))
               (region (buffer-substring start end)))
          (goto-char end)
          (dotimes (i num)
            (insert region)))))

    (defun paredit-duplicate-current-line ()
      (back-to-indentation)
      (let (kill-ring kill-ring-yank-pointer)
        (paredit-kill)
        (yank)
        (newline-and-indent)
        (yank)))

    (defun duplicate-current-line (&optional num)
      "Duplicate the current line NUM times."
      (interactive "p")
      (if (bound-and-true-p paredit-mode)
          (paredit-duplicate-current-line)
        (save-excursion
          (when (eq (point-at-eol) (point-max))
            (goto-char (point-max))
            (newline)
            (forward-char -1))
          (duplicate-region num (point-at-bol) (1+ (point-at-eol))))))

    ;; automatically indenting yanked text if in programming-modes

    (defvar yank-indent-modes '(prog-mode
                                sgml-mode
                                js2-mode)
      "Modes in which to indent regions that are yanked (or yank-popped)")

    (defvar yank-advised-indent-threshold 1000
      "Threshold (# chars) over which indentation does not automatically occur.")

    (defun yank-advised-indent-function (beg end)
      "Do indentation, as long as the region isn't too large."
      (if (<= (- end beg) yank-advised-indent-threshold)
          (indent-region beg end nil)))

    (defadvice yank (after yank-indent activate)
      "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
      (if (and (not (ad-get-arg 0))
               (--any? (derived-mode-p it) yank-indent-modes))
          (let ((transient-mark-mode nil))
            (yank-advised-indent-function (region-beginning) (region-end)))))

    (defadvice yank-pop (after yank-pop-indent activate)
      "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
      (if (and (not (ad-get-arg 0))
               (member major-mode yank-indent-modes))
          (let ((transient-mark-mode nil))
            (yank-advised-indent-function (region-beginning) (region-end)))))

    (defun yank-unindented ()
      (interactive)
      (yank 1))

    ;; toggle quotes

    (defun current-quotes-char ()
      (nth 3 (syntax-ppss)))

    (defalias 'point-is-in-string-p 'current-quotes-char)

    (defun move-point-forward-out-of-string ()
      (while (point-is-in-string-p) (forward-char)))

    (defun move-point-backward-out-of-string ()
      (while (point-is-in-string-p) (backward-char)))

    (defun alternate-quotes-char ()
      (if (eq ?' (current-quotes-char)) ?\" ?'))

    (defun toggle-quotes ()
      (interactive)
      (if (point-is-in-string-p)
          (let ((old-quotes (char-to-string (current-quotes-char)))
                (new-quotes (char-to-string (alternate-quotes-char)))
                (start (make-marker))
                (end (make-marker)))
            (save-excursion
              (move-point-forward-out-of-string)
              (backward-delete-char 1)
              (set-marker end (point))
              (insert new-quotes)
              (move-point-backward-out-of-string)
              (delete-char 1)
              (insert new-quotes)a
              (set-marker start (point))
              (replace-string new-quotes (concat "\\" new-quotes) nil start end)
              (replace-string (concat "\\" old-quotes) old-quotes nil start end)))
        (error "Point isn't in a string")))

    ;; kill region if active, otherwise kill backward word

    (defun kill-region-or-backward-word ()
      (interactive)
      (if (region-active-p)
          (kill-region (region-beginning) (region-end))
        (backward-kill-word 1)))

    (defun kill-to-beginning-of-line ()
      (interactive)
      (kill-region (save-excursion (beginning-of-line) (point))
                   (point)))

    ;; copy region if active
    ;; otherwise copy to end of current line
    ;;   * with prefix, copy N whole lines

    (defun copy-to-end-of-line ()
      (interactive)
      (kill-ring-save (point)
                      (line-end-position))
      (message "Copied to end of line"))

    (defun copy-whole-lines (arg)
      "Copy lines (as many as prefix argument) in the kill ring"
      (interactive "p")
      (kill-ring-save (line-beginning-position)
                      (line-beginning-position (+ 1 arg)))
      (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

    (defun copy-line (arg)
      "Copy to end of line, or as many lines as prefix argument"
      (interactive "P")
      (if (null arg)
          (copy-to-end-of-line)
        (copy-whole-lines (prefix-numeric-value arg))))

    (defun save-region-or-current-line (arg)
      (interactive "P")
      (if (region-active-p)
          (kill-ring-save (region-beginning) (region-end))
        (copy-line arg)))

    (defun kill-and-retry-line ()
      "Kill the entire current line and reposition point at indentation"
      (interactive)
      (back-to-indentation)
      (kill-line))

    (defun camelize-buffer ()
      (interactive)
      (goto-char 0)
      (ignore-errors
        (replace-next-underscore-with-camel 0))
      (goto-char 0))

    ;; kill all comments in buffer
    (defun comment-kill-all ()
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (comment-kill (save-excursion
                        (goto-char (point-max))
                        (line-number-at-pos)))))

    (defun incs (s &optional num)
      (let* ((inc (or num 1))
             (new-number (number-to-string (+ inc (string-to-number s))))
             (zero-padded? (s-starts-with? "0" s)))
        (if zero-padded?
            (s-pad-left (length s) "0" new-number)
          new-number)))

    (defun goto-closest-number ()
      (interactive)
      (let ((closest-behind (save-excursion (search-backward-regexp "[0-9]" nil t)))
            (closest-ahead (save-excursion (search-forward-regexp "[0-9]" nil t))))
        (push-mark)
        (goto-char
         (cond
          ((and (not closest-ahead) (not closest-behind)) (error "No numbers in buffer"))
          ((and closest-ahead (not closest-behind)) closest-ahead)
          ((and closest-behind (not closest-ahead)) closest-behind)
          ((> (- closest-ahead (point)) (- (point) closest-behind)) closest-behind)
          ((> (- (point) closest-behind) (- closest-ahead (point))) closest-ahead)
          :else closest-ahead))))

    (defun change-number-at-point (arg)
      (interactive "p")
      (unless (or (looking-at "[0-9]")
                  (looking-back "[0-9]"))
        (goto-closest-number))
      (save-excursion
        (while (looking-back "[0-9]")
          (forward-char -1))
        (re-search-forward "[0-9]+" nil)
        (replace-match (incs (match-string 0) arg) nil nil)))

    (defun subtract-number-at-point (arg)
      (interactive "p")
      (change-number-at-point (- arg)))

    (defun replace-next-underscore-with-camel (arg)
      (interactive "p")
      (if (> arg 0)
          (setq arg (1+ arg))) ; 1-based index to get eternal loop with 0
      (let ((case-fold-search nil))
        (while (not (= arg 1))
          (search-forward-regexp "\\b_[a-z]")
          (forward-char -2)
          (delete-char 1)
          (capitalize-word 1)
          (setq arg (1- arg)))))

    (defun snakeify-current-word ()
      (interactive)
      (er/mark-word)
      (let* ((beg (region-beginning))
             (end (region-end))
             (current-word (buffer-substring-no-properties beg end))
             (snakified (s-snake-case current-word)))
        (replace-string current-word snakified nil beg end)))

    (defun camelcase-region (start end)
      "Changes region from snake_case to camelCase"
      (interactive "r")
      (save-restriction (narrow-to-region start end)
                        (goto-char (point-min))
                        (while (re-search-forward "_\\(.\\)" nil t)
                          (replace-match (upcase (match-string 1))))))

    ;; ----------------------------------------------------------------------
    ;; cadged largely from http://xahlee.org/emacs/elisp_idioms.html:
    ;;
    (defun camelcase-word-or-region ()
      "Changes word or region from snake_case to camelCase"
      (interactive)
      (let (pos1 pos2 bds)
        (if (and transient-mark-mode mark-active)
            (setq pos1 (region-beginning) pos2 (region-end))
          (progn
            (setq bds (bounds-of-thing-at-point 'symbol))
            (setq pos1 (car bds) pos2 (cdr bds))))
        (camelcase-region pos1 pos2)))

    ;; ----------------------------------------------------------------------
    ;; snakecase-region
    ;; Given a region of text in camelCase format, changes it to snake_case.
    ;;
    ;; BUG: This is actually just a repeat of camelcase-region!
    (defun snakecase-region (start end)
      "Changes region from camelCase to snake_case"
      (interactive "r")
      (save-restriction (narrow-to-region start end)
                        (goto-char (point-min))
                        (while (re-search-forward "_\\(.\\)" nil t)
                          (replace-match (upcase (match-string 1))))))

    ;; ----------------------------------------------------------------------
    ;; Given a region of text in camelCase format, changes it to snake_case.
    (defun snakecase-word-or-region ()
      "Changes word or region from camelCase to snake_case"
      (interactive)
      (let (pos1 pos2 bds)
        (if (and transient-mark-mode mark-active)
            (setq pos1 (region-beginning) pos2 (region-end))
          (progn
            (setq bds (bounds-of-thing-at-point 'symbol))
            (setq pos1 (car bds) pos2 (cdr bds))))
        (snakecase-region pos1 pos2)))

    (defun kebab-current-word ()
      (interactive)
      (er/mark-word)
      (let* ((beg (region-beginning))
             (end (region-end))
             (current-word (buffer-substring-no-properties beg end))
             (kebabed (s-dashed-words current-word)))
        (replace-string current-word kebabed nil beg end)))

    (defun transpose-params ()
      "Presumes that params are in the form (p, p, p) or {p, p, p} or [p, p, p]"
      (interactive)
      (let* ((end-of-first (cond
                            ((looking-at ", ") (point))
                            ((and (looking-back ",") (looking-at " ")) (- (point) 1))
                            ((looking-back ", ") (- (point) 2))
                            (t (error "Place point between params to transpose."))))
             (start-of-first (save-excursion
                               (goto-char end-of-first)
                               (move-backward-out-of-param)
                               (point)))
             (start-of-last (+ end-of-first 2))
             (end-of-last (save-excursion
                            (goto-char start-of-last)
                            (move-forward-out-of-param)
                            (point))))
        (transpose-regions start-of-first end-of-first start-of-last end-of-last)))

    (defun move-forward-out-of-param ()
      (while (not (looking-at ")\\|, \\| ?}\\| ?\\]"))
        (cond
         ((point-is-in-string-p) (move-point-forward-out-of-string))
         ((looking-at "(\\|{\\|\\[") (forward-list))
         (t (forward-char)))))

    (defun move-backward-out-of-param ()
      (while (not (looking-back "(\\|, \\|{ ?\\|\\[ ?"))
        (cond
         ((point-is-in-string-p) (move-point-backward-out-of-string))
         ((looking-back ")\\|}\\|\\]") (backward-list))
         (t (backward-char)))))

    (autoload 'zap-up-to-char "misc"
      "Kill up to, but not including ARGth occurrence of CHAR.")

    (defun css-expand-statement ()
      (interactive)
      (save-excursion
        (end-of-line)
        (search-backward "{")
        (forward-char 1)
        (let ((beg (point)))
          (newline)
          (er/mark-inside-pairs)
          (replace-regexp ";" ";\n" nil (region-beginning) (region-end))
          (indent-region beg (point)))))

    (defun css-contract-statement ()
      (interactive)
      (end-of-line)
      (search-backward "{")
      (while (not (looking-at "}"))
        (join-line -1))
      (back-to-indentation))

    (defun +join-line-indent ()
      (interactive)
      (save-excursion
        (join-line)
        (indent-according-to-mode)))

    (global-set-key (kbd "C-M-j") #'+join-line-indent)
    (global-set-key (kbd "C-w") 'kill-region-or-backward-word)
    (global-set-key (kbd "C-c C--") 'camelcase-word-or-region)
    (global-set-key (kbd "C-c C-_") 'snakecase-word-or-region)))

(use-package restclient)

(use-package yaml-mode)
(use-package ag)
(use-package editorconfig)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; (use-package centaur-tabs
;;   :config
;;   (centaur-tabs-mode t)
;;   :bind
;;   ("C-<prior>" . centaur-tabs-backward)
;;   ("C-<next>" . centaur-tabs-forward))

(use-package scala-mode
  :interpreter
    ("scala" . scala-mode))
