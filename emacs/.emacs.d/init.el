;;; -*- lexical-binding: t -*-

;; ====
;; INIT

;;; Code:

(setq comp-speed 3
      comp-deferred-compilation t
      native-comp-async-report-warnings-errors nil)

(if (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
  (message "Native compilation is available")
(message "Native complation is *not* available"))

(if (functionp 'json-serialize)
  (message "Native JSON is available")
(message "Native JSON is *not* available"))

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

;; Clear out recipe overrides (in case of re-init).
(setq straight-recipe-overrides nil)

;; Bootstrap the package manager, straight.el.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
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

(straight-use-package 'org)

(use-package el-patch
  :demand t)

(eval-when-compile
  (require 'el-patch))

(use-package restart-emacs)

(set-default-coding-systems 'utf-8)

(use-feature server
  :demand t
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

;; (use-package exec-path-from-shell
;;   :config
;;   (exec-path-from-shell-initialize)
;;   (if (and (fboundp 'native-comp-available-p)
;;            (native-comp-available-p))
;;       (progn
;;         (message "Native comp is available")
;;         (add-to-list 'exec-path (expand-file-name "~/homebrew/opt/gccemacs/bin"))
;;         (setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
;;                                        (when (getenv "LIBRARY_PATH")
;;                                          ":")
;;                                        (car (file-expand-wildcards
;;                                              (expand-file-name "~/homebrew/opt/gcc/lib/gcc/*")))))
;;         ;; Only set after LIBRARY_PATH can find gcc libraries.
;;         (setq comp-deferred-compilation t))
;;     (message "Native comp is *not* available")))

;; Store custom-file separately, don't freak out when it's not found
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; remember risk variables (dir-locals)
(defun risky-local-variable-p (sym &optional _ignored) nil)

;; =============
;; MODIFIER KEYS

(defun +ms/switch-left-and-right-option-keys ()
"Switch left and right option keys.
On some external keyboards the left and right option keys are swapped,
this command switches the keys so that they work as expected."
  (interactive)
  (let ((current-left  mac-option-modifier)
        (current-right mac-right-option-modifier))
    (setq mac-option-modifier       current-right
          mac-right-option-modifier current-left)))

(when *is-mac*
  ;; Both command keys are 'Super'
  (setq mac-right-command-modifier 'super)
  (setq mac-command-modifier 'super)
  ;; Option or Alt is naturally 'Meta'
  (setq mac-option-modifier 'meta)
  ;; Right Alt (option) can be used to enter symbols like em dashes '—' and euros '€' and stuff.
  (setq mac-right-option-modifier 'nil)
  (+ms/switch-left-and-right-option-keys))

(setq-default evil-want-C-u-scroll t)
(setq-default evil-want-C-d-scroll t)
(setq-default evil-symbol-word-search t)
(setq-default evil-esc-delay 0)

(use-package evil
  :init

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

;; =============
;; SANE DEFAULTS

(setq initial-scratch-message nil)
(setq default-directory "~/" )

;; Smoother and nicer scrolling
(setq
 scroll-margin 0
 scroll-step 0
 next-line-add-newlines nil
 scroll-conservatively 100000
 scroll-preserve-screen-position 1
 )

(pixel-scroll-mode t)

(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(use-package golden-ratio-scroll-screen
  :disabled t
  :bind
  ([remap scroll-down-command] . 'golden-ratio-scroll-screen-down)
  ([remap scroll-up-command] . 'golden-ratio-scroll-screen-up))


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
;; (use-package undo-tree
;;   :config
;;   (progn
;;     (global-undo-tree-mode)
;;     (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/tmp/undo"))
;;           undo-tree-auto-save-history t
;;           undo-tree-visualizer-timestamps t
;;           undo-tree-visualizer-diff t)))

(use-package undo-fu
  :demand t
  :config
  (setq undo-limit 400000
        undo-strong-limit 3000000
        undo-outer-limit 3000000)

  ;; (global-undo-tree-mode -1)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)

(use-package undo-fu-session
  :demand t
  :preface

  (setq undo-fu-session-compression t
        undo-fu-session-directory (concat user-emacs-directory "undo-fu-session/")
        undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))

  :config
  (global-undo-fu-session-mode +1)))

  ;; HACK We avoid `:config' here because `use-package's `:after' complicates
  ;;      the load order of a package's `:config' block and makes it impossible
  ;;      for the user to override its settings with merely `after!' (or
  ;;      `eval-after-load'). See jwiegley/use-package#829.
  ;; :config
  ;;         ;; HACK Use the faster zstd to compress undo files instead of gzip
  ;;         (when (executable-find "zstd")
  ;;           (defadvice! doom--undo-fu-session-use-zstd-a (filename)
  ;;             :filter-return #'undo-fu-session--make-file-name
  ;;             (if undo-fu-session-compression
  ;;                 (concat (file-name-sans-extension filename) ".zst")
  ;;               filename)))
          ;; ))


(global-subword-mode -1)

(recentf-mode +1)

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

;; ================
;; BASIC NAVIGATION

(use-package which-key
  :config
  (which-key-mode))

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
    (smart-newline-mode)
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

;; (global-set-key (kbd "s-<return>") 'smart-open-line)            ;; Cmd+Return new line below
;; (global-set-key (kbd "s-S-<return>") 'smart-open-line-above)    ;; Cmd+Shift+Return new line above

;; Upcase and lowercase word or region, if selected.
;; To capitalize or un-capitalize word use Alt+c and Alt+l
(bind-key "M-c" #'capitalize-dwim)
(bind-key "M-l" #'downcase-dwim)
(bind-key "M-u" #'upcase-dwim)

;; Package `visual-regexp' provides an alternate version of
;; `query-replace' which highlights matches and replacements as you
;; type.
(use-package visual-regexp
  :bind (([remap query-replace] . #'vr/query-replace)))

(use-package multiple-cursors
  :bind (("s-d" . mc/mark-next-like-this)
         ("s-D" . mc/mark-previous-like-this)
         ("C-c s-d" . mc/mark-all-like-this-dwim))
  :config
  (multiple-cursors-mode +1))

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

(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode))

;; =================
;; WINDOW MANAGEMENT

;; This is rather radical, but saves from a lot of pain in the ass.
;; When split is automatic, always split windows vertically
(setq split-height-threshold 0)
(setq split-width-threshold nil)

;; Move between windows with Control-Command-Arrow and with =Cmd= just like in iTerm.
(use-feature windmove
  :config
  (windmove-default-keybindings))

(use-feature winner
  :config
  (winner-mode 1))

(use-package ace-window
  :bind (("C-x o" . ace-window)))

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
  :defer 2

  :config
  (setq projectile-switch-project-action 'projectile-commander)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'default)

  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (projectile-mode +1)

  :blackout t)

(use-package deadgrep
  :defer t
  :bind ("C-c h" . deadgrep))

;; ==========================================
;; MENUS AND COMPLETION (not code completion)

;;;; Candidate selection

;; Package `prescient' is a library for intelligent sorting and
;; filtering in various contexts.
(use-package prescient
  :disabled t
  :config

  ;; Remember usage statistics across Emacs sessions.
  (prescient-persist-mode +1)

  ;; The default settings seem a little forgetful to me. Let's try
  ;; this out.
  (setq prescient-history-length 1000))

;; Use minimalist Ivy for most things.
(use-package ivy
  :disabled t
  :diminish                             ;; don't show Ivy in minor mode list
  :init
  (let ((standard-search-fn #'+ivy-prescient-non-fuzzy)
        (alt-search-fn #'ivy--regex-ignore-order))
    (setq ivy-re-builders-alist
          `((counsel-rg     . ,standard-search-fn)
            (swiper         . ,standard-search-fn)
            (swiper-isearch . ,standard-search-fn)
            (t . ,alt-search-fn))
          ivy-more-chars-alist
          '((counsel-rg . 1)
            (counsel-search . 2)
            (t . 3))))

  :config
  (ivy-mode 1)                          ;; enable Ivy everywhere

  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-sort-max-size 7500)

  (setq ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-read-action-function #'ivy-hydra-read-action
        ivy-read-action-format-function #'ivy-read-action-format-columns
        ivy-initial-inputs-alist nil
        projectile-completion-system 'ivy
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function #'ignore
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t)


  ;; (setq ivy-re-builders-alist
  ;;     '((swiper . ivy--regex-plus)
  ;;       (t      . ivy--regex-fuzzy)))   ;; enable fuzzy searching everywhere except for Swiper

  (global-set-key (kbd "s-b") 'ivy-switch-buffer)  ;; Cmd+b show buffers and recent files
  (global-set-key (kbd "M-s-b") 'ivy-resume))      ;; Alt+Cmd+b resume whatever Ivy was doing


;; Swiper is a better local finder.
(use-package swiper
  :disabled t
  :config
  (global-set-key "\C-s" 'swiper)
  (global-set-key "\C-r" 'swiper)
  (setq swiper-action-recenter t))


;; Better menus with Counsel (a layer on top of Ivy)
(use-package counsel
  :disabled t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)            ;; Alt+x run command
  (global-set-key (kbd "s-x") 'counsel-M-x)            ;; Alt+x run command
  (global-set-key (kbd "s-P") 'counsel-M-x)            ;; Cmd+Shift+p run command
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)  ;; Replace built-in Emacs 'find file' (open file) with Counsel
  (global-set-key (kbd "s-o") 'counsel-find-file))     ;; Cmd+o open file

;; (use-package smex)  ;; show recent commands when invoking Alt-x (or Cmd+Shift+p)
;; (use-package flx)   ;; enable fuzzy matching
;; (use-package avy)   ;; enable avy for quick navigation

(use-package ivy-prescient
  :disabled t
  :hook (ivy-mode . ivy-prescient-mode)
  :hook (ivy-prescient-mode . prescient-persist-mode)
  :commands +ivy-prescient-non-fuzzy

  :init
  (setq prescient-filter-method '(literal regexp initialism fuzzy))

  :config
  (setq ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer
               lsp-ivy-workspace-symbol ivy-resume ivy--restore-session
               counsel-grep counsel-git-grep counsel-rg counsel-ag
               counsel-ack counsel-fzf counsel-pt counsel-imenu
               counsel-yank-pop counsel-recentf counsel-buffer-or-recentf)
        ivy-prescient-retain-classic-highlighting t)
  (defun +ivy-prescient-non-fuzzy (str)
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))

  )


;; Make Ivy a bit more friendly by adding information to ivy buffers, e.g. description of commands in Alt-x, meta info when switching buffers, etc.
(use-package ivy-rich
  :disabled t
  :after ivy
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (setq ivy-rich-path-style 'abbrev)        ;; Abbreviate paths using abbreviate-file-name (e.g. replace “/home/username” with “~”)

  (ivy-rich-mode +1)
  (ivy-rich-project-root-cache-mode +1))


;; Integrate Projectile with Counsel
(use-package counsel-projectile
  :disabled t
  :config
  (setq counsel-projectile-sort-files t)
  (counsel-projectile-mode +1)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "s-p") 'counsel-projectile-find-file)         ;; Cmd+p open file in current project
  (global-set-key (kbd "s-F") 'counsel-projectile-rg))     ;; Cmd+Shift+F search in current git repository

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Configure directory extension.
;; NOTE: The file `vertico-directory.el' must be installed manually.
(use-package vertico-directory
  :straight nil
  :load-path "straight/build/vertico/extensions"
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))
  ;; Tidy shadowed file names

;; Use the `orderless' completion style. Additionally enable
;; `partial-completion' for file path expansion. `partial-completion' is
;; important for wildcard support. Multiple files can be opened at once
;; with `find-file' if you enter a wildcard. You may also give the
;; `initials' completion style a try.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; (restart-emacs)

;; A few more useful configurations...
(use-package emacs
  :init
  ;; (global-set-key (kbd "M-x") 'execute-extended-command)
  (global-set-key (kbd "s-x") 'execute-extended-command)            ;; Alt+x run command
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; (setq projectile-completion-system 'ivy)             ;; Use Ivy in Projectile

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("C-s" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
)

(use-package amx
  :disabled t)



(use-package avy
  :bind (("C-=" . avy-goto-char)))

;; ========================
;; VERSION CONTROL WITH GIT


;; Magit
(use-package magit
  :defer 3
  :bind (("C-x g" . magit-status)
         ("C-x C-b" . magit-blame-addition))
  :config
  ;; (setq magit-diff-refine-hunk 'all)
  ;; (setq magit-log-auto-more t)

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
  :demand t
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
(use-package forge)

;; ===============
;; CODE COMPLETION

(use-package company
  :defer 2

  :custom
  (company-idle-delay 0.15)
  (company-global-modes '(not org-mode))
  (company-minimum-prefix-length 1)
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
  (company-show-numbers t)

  ;; :bind
  ;; ([remap completion-at-point] . company-manual-begin)
  ;; ([remap complete-symbol] . company-manual-begin)

  :config
  ;; (company-tng-configure-default)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  ;; (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; (setq company-continue-commands
  ;;       (append company-continue-commands
  ;;               '(comint-previous-matching-input-from-input
  ;;                 comint-next-matching-input-from-input)))

  (global-company-mode +1)

  :blackout t)

(use-package company-quickhelp
  :demand t
  :after company

  :config
  (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)
  (company-quickhelp-mode t)

  :blackout t)

;; Package `company-prescient' provides intelligent sorting and
;; filtering for candidates in Company completions.
(use-package company-prescient
  :demand t
  :after company

  :config
  ;; Use `prescient' for Company menus.
  (company-prescient-mode +1)

  :blackout t)

(defvar he-search-loc-backward (make-marker))
(defvar he-search-loc-forward (make-marker))

(defun try-expand-dabbrev-closest-first (old)
  "Try to expand word \"dynamically\", searching the current buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string). It returns t if a new expansion is found, nil otherwise."
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

;; ===========
;; PROGRAMMING

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
         ("\\.html?\\'" . web-mode)
         ("\\.hbs?\\'" . web-mode))
  :config

  ;; Indent by two spaces by default.
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  ;; Autocomplete </ instantly.
  (setq web-mode-enable-auto-closing t))

(use-package emmet-mode
  :demand t
  :hook (web-mode . emmet-mode))

(defvar org-folder "~/Sync/org")

(use-package org
  :demand t
  :mode (("\\.org$" . org-mode))

  :bind (("C-c l" . org-store-link)
         ("C-c L" . org-insert-link-global)
         ("C-c a" . org-agenda)
         ("C-c O" . org-open-at-point-global)
         ("C-c c" . org-capture)
         ("C-c j" . org-clock-goto))

  :hook ((org-mode . auto-fill-mode)
         (org-mode . (lambda () (display-line-numbers-mode 0)))
         (org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode))

  :config
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (setq org-startup-indented t)         ;; Visually indent sections. This looks better for smaller files.
  (setq org-log-into-drawer t)          ;; State changes for todos and also notes should go into a Logbook drawer
  (setq org-log-done 'note)             ;; Add closed date when todo goes to DONE state
  (setq org-support-shift-select t)     ;; Allow shift selection with arrows.
  (setq org-hide-leading-stars nil)     ;; Allow shift selection with arrows.

  (setq org-fontify-quote-and-verse-blocks t) ;; Highlight quotes

  (setq org-src-window-setup 'other-window) ;; Better source code window editing

  ;; Highlight and indent source code blocks
  (setq org-src-fontify-natively t)     ;; Code highlighting in code blocks
  (setq org-src-tab-acts-natively t)    ;; Tab in source blocks should act like in major mode
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)

  (setq org-indent-indentation-per-level 1)
  (setq org-adapt-indentation nil)

  (setq org-cycle-separator-lines 1)

  (add-to-list 'org-structure-template-alist
               '("s" . "src"))

  (setq org-todo-keywords
        '((sequence "TODO(n)" "IN-PROGRESS(p)" "IN REVIEW(r)" "IN TESTING(t)" "|" "DONE(d)")
          (sequence "WAITING(w)" "HOLD(h)" "|" "CANCELLED(c)")))

  (require 'find-lisp)

  (setq org-directory org-folder
        org-default-notes-file (concat org-folder "/inbox.org")
        org-agenda-files (find-lisp-find-files org-folder "\.org$"))

  (setq org-capture-templates
        `(
          ("t" "todo" entry (file ,(concat org-folder "/refile.org"))
           "* TODO %?\n" :clock-in t :clock-resume t)
          ("T" "todo with link" entry (file ,(concat org-folder "/refile.org"))
           "* TODO %?\n%a\n" :clock-in t :clock-resume t)
          ("e" "email" entry (file ,(concat org-folder "/refile.org"))
           "* TODO %? Email: %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
           :clock-in t :clock-resume t :immediate-finish nil)
          ("i" "Idea" entry (file+headline ,(concat org-folder "/refile.org"))
           "* %? \n %U")
          ("j" "Journal entry" entry (file+olp+datetree ,(concat org-folder "/journal.org"))
           "* %?\n" :clock-in t :clock-resume t)
          ("J" "Journal with link" entry (file+olp+datetree ,(concat org-folder "/journal.org"))
           "* %?\n%a\n" :clock-in t :clock-resume t)
          ("l" "Learn" entry (file+headline ,(concat org-folder "/refile.org"))
           "* %? \n")
          ("n" "note" entry (file ,(concat org-folder "/refile.org"))
           "* %? :NOTE:\n%a\n" :clock-in t :clock-resume t)
          ("w" "org-protocol" entry (file ,(concat org-directory "/refile.org"))
           "* TODO Review %c\n%U\n" :immediate-finish t)
          ("p" "Protocol" entry (file+headline ,(concat org-folder "/refile.org") "Refile")
           "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
          ("L" "Protocol Link" entry (file+headline ,(concat org-folder "/refile.org") "Refile")
           "* %? [[%:link][%:description]] \nCaptured On: %U")
          ))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)))

  (use-feature org-inlinetask
    :demand t)

  (use-feature org-protocol
    :demand t)

  (use-package org-bullets
    :demand t
    :hook (org-mode . org-bullets-mode))

  (use-package org-pomodoro
    :demand t))

(use-package neuron-mode
  :demand t
  :hook (neuron-mode . company-neuron-setup)
  :config
  (setq neuron-default-zettelkasten-directory "~/Sync/notes"))

  (use-package writegood-mode
    :bind ("C-c g" . writegood-mode)
    :config
    (add-to-list 'writegood-weasel-words "actionable"))

(use-package deft
  :demand t
  :bind (("C-x C-g" . 'deft-find-file)
         ("C-M-x" . 'deft))
  :config
  (setq deft-default-extension "org"
        deft-extensions '("org")
        deft-directory org-folder
        deft-recursive t
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t))

;; Make bug references clickable (redirecting to bug tracker, jira in this case)
(use-feature bug-reference
  :demand t
  :hook (org-mode . bug-reference-mode)
  :config
    (setq bug-reference-bug-regexp "\\(NTP-\\([0-9]\\{5\\}\\)\\)"
          bug-reference-url-format "https://nezasa.atlassian.net/browse/NTP-%s"))

(global-set-key (kbd "C-x i") (lambda () (interactive) (find-file "~/Sync/org/inbox.org")))
(global-set-key (kbd "C-x t") (lambda () (interactive) (find-file "~/Sync/org/todo.org")))

(use-package olivetti
  :demand t
  :hook (org-mode . olivetti-mode)
  :custom (olivetti-body-width 80))

;; Open config file by pressing C-x and then C
(global-set-key (kbd "C-x C") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (use-package yasnippet-snippets)
  (global-set-key (kbd "C-c s") 'company-yasnippet))

(use-package flycheck
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
          ([remap evil-jump-to-tag] . dumb-jump-go)))

(use-package json-mode
  :mode (("\\.bowerrc$"     . json-mode)
         ("\\.jshintrc$"    . json-mode)
         ("\\.json_schema$" . json-mode)))

(use-package js2-mode
  :mode "\\.m?js\\'"
  :interpreter "node"
  :config
  (setq js-chain-indent t
        ;; Don't mishighlight shebang lines
        js2-skip-preprocessor-directives t
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        ;; js2-strict-trailing-comma-warning nil
        ;; js2-strict-missing-semi-warning nil
        ;; maximum fontification
        js2-highlight-level 3
        js2-highlight-external-variables t
        js2-idle-timer-delay 0.1))

(use-package scala-mode
  :interpreter
    ("scala" . scala-mode))

(use-package haskell-mode
  :config
  (setq haskell-process-suggest-remove-import-lines t  ; warnings for redundant imports etc
        haskell-process-auto-import-loaded-modules t))

(use-package tuareg
  :config
  (add-hook 'tuareg-mode-hook (lambda ()
                                (progn
                                  (define-key tuareg-mode-map (kbd "C-c C-s")
                                    'utop)
                                  (setq compile-command
                                        "opam config exec corebuild ")))))

(use-package utop
  :hook (tuareg-mode . utop-minor-mode)
  :config
  (setq utop-command "opam config exec utop -- -emacs"
        merlin-error-after-save nil))


(use-package merlin
  :hook (tuareg-mode . merlin-mode)
  :config
  (eval-after-load 'company
    '(push 'merlin-company-backend company-backends))

  (use-package flycheck-ocaml
    :config
    ;; Disable Merlin's own error checking
    (setq merlin-error-after-save nil)
    ;; Enable Flycheck checker
    (flycheck-ocaml-setup)))

(defun copy-buffer-file-name ()
  "Copy buffer's full path."
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(global-set-key (kbd "s-f") #'copy-buffer-file-name)

(use-package string-inflection
  :config
  (global-set-key (kbd "C-c i") 'string-inflection-cycle)
  (global-set-key (kbd "C-c C") 'string-inflection-camelcase)
  (global-set-key (kbd "C-c L") 'string-inflection-lower-camelcase)
  (global-set-key (kbd "C-c J") 'string-inflection-java-style-cycle))

(use-package dash
  :demand t
  :config
  (use-package s
    :demand t
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

    (defun +join-line-indent ()
      (interactive)
      (save-excursion
        (join-line)
        (indent-according-to-mode)))

    (global-set-key (kbd "C-M-j") #'+join-line-indent)
    (global-set-key (kbd "C-w") 'kill-region-or-backward-word)
    (global-set-key (kbd "C-c C--") 'camelcase-word-or-region)
    (global-set-key (kbd "C-c C-_") 'snakecase-word-or-region)))

(use-package jq-mode
  :demand t)

(use-package restclient
  :demand t
  :mode (("\\.http\\'" . restclient-mode))
  :straight (restclient :type git :host github :repo
  "pashky/restclient.el"
  :files ("restclient*.el")))

(use-package ag)

;; =======
;; VISUALS

;; Allow you to resize frames however you want, not just in whole
;; columns. "The 80s called, they want their user interface back"
(setq frame-resize-pixelwise t)

(when *is-linux* (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode +1)
(setq cursor-type 'bar)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Always wrap lines
(global-visual-line-mode +1)

;; Don't highlight current line
(global-hl-line-mode 0)

;; Fill column
(setq fci-rule-column 90)

;; disable bidirectional editing
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Show line numbers
(global-display-line-numbers-mode t)
(define-key global-map (kbd "C-x l") 'global-display-line-numbers-mode)

; Enable transparent title bar on macOS
(when *is-mac*
  (add-to-list 'default-frame-alist '(ns-appearance . light)) ;; {light, dark}
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(setq visible-bell t)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; Load custom themes
(setq custom-theme-directory (concat user-emacs-directory "themes"))
(when (file-directory-p custom-theme-directory)
      (add-to-list 'custom-theme-load-path custom-theme-directory)
      (add-to-list 'load-path custom-theme-directory))

(defun +disable-themes ()
  "Disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  "Disable active themes before loading the new theme."
  (+disable-themes))

(fringe-mode '(10 . 10))

(when *is-mac*
  ;; Render thinner fonts
  (setq ns-use-thin-smoothing t)
  ;; Don't open a file in a new frame
  (setq ns-pop-up-frames nil))

(use-package doom-themes
  :config
  (load-theme 'doom-flatwhite t))

(load-theme 'tomorrow-night t)

;; (setq +font "LiterationMono Nerd Font")
;; (setq +font "Inconsolata Nerd Font")
;; (set-frame-font (concat +font "-" (number-to-string (/ +font-size 10))))
;; (setq +font "Roboto Mono")
(setq +font "SFMono Nerd Font")
(setq +font "Iosevka Custom")
(setq +font "JetBrainsMonoMedium Nerd Font")
;; (setq +font "IBM Plex Mono")
(setq +font-size 220)

(set-face-attribute 'default nil
                    :font +font
                    :weight 'normal
                    :width 'condensed
                    :height +font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font +font
                    :weight 'normal
                    :width 'condensed
                    :height +font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font "Overpass Nerd Font"
                    :height 260
                    :weight 'regular)

;; Prune the build cache for straight.el; this will prevent it from
;; growing too large. Do this after the final hook to prevent packages
;; installed there from being pruned.
(straight-prune-build-cache)
