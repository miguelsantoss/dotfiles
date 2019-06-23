;;; -*- lexical-binding: t -*-

;; ====
;; INIT

;;; Code:

(setq package-enable-at-startup nil)

(setq +original-gc-cons-threshold gc-cons-threshold)

(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold +original-gc-cons-threshold)))

(setq gc-cons-threshold (* 5 1000 1000))

(defmacro def (name &rest body)
  (declare (indent 1) (debug t))
  `(defun ,name (&optional _arg)
     ,(if (stringp (car body)) (car body))
     (interactive "p")
     ,@(if (stringp (car body)) (cdr `,body) body)))

(defmacro λ (&rest body)
  (declare (indent 1) (debug t))
  `(lambda ()
     (interactive)
     ,@body))

(defmacro add-λ (hook &rest body)
  (declare (indent 1) (debug t))
  `(add-hook ,hook (lambda () ,@body)))

(defmacro after (feature &rest forms)
  (declare (indent 1) (debug t))
  `(,(if (or (not (bound-and-true-p byte-compile-current-file))
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         #'progn
       #'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

(defmacro radian-protect-macros (&rest body)
  "Eval BODY, protecting macros from incorrect expansion.
This macro should be used in the following situation:

Some form is being evaluated, and this form contains as a
sub-form some code that will not be evaluated immediately, but
will be evaluated later. The code uses a macro that is not
defined at the time the top-level form is evaluated, but will be
defined by time the sub-form's code is evaluated. This macro
handles its arguments in some way other than evaluating them
directly. And finally, one of the arguments of this macro could
be interpreted itself as a macro invocation, and expanding the
invocation would break the evaluation of the outer macro.

You might think this situation is such an edge case that it would
never happen, but you'd be wrong, unfortunately. In such a
situation, you must wrap at least the outer macro in this form,
but can wrap at any higher level up to the top-level form."
  (declare (indent 0))
  `(eval '(progn ,@body)))

(defmacro radian-defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. DOCSTRING and BODY are as in
`defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "radian-defadvice: no docstring provided"))
  `(progn
     (defun ,name ,arglist
       ,(let ((article (if (string-match-p "^:[aeiou]" (symbol-name where))
                           "an"
                         "a")))
          (format "%s\n\nThis is %s `%S' advice for `%S'."
                  docstring article where
                  (if (and (listp place)
                           (memq (car place) ''function))
                      (cadr place)
                    place)))
       ,@body)
     (advice-add ',place ',where #',name)
     ',name))

(defmacro radian-defhook (name arglist hook docstring &rest body)
  "Define a function called NAME and add it to a hook.
ARGLIST is as in `defun'. HOOK is the hook to which to add the
function. DOCSTRING and BODY are as in `defun'."
  (declare (indent 2)
           (doc-string 4))
  (unless (string-match-p "-hook$" (symbol-name hook))
    (error "Symbol `%S' is not a hook" hook))
  (unless (stringp docstring)
    (error "radian-defhook: no docstring provided"))
  `(progn
     (defun ,name ,arglist
       ,(format "%s\n\nThis function is for use in `%S'."
                docstring hook)
       ,@body)
     (add-hook ',hook ',name)))

(setq is-mac (eq system-type 'darwin))
(setq is-linux (eq system-type 'gnu/linux))

(setq straight-repository-branch "develop")
(setq straight-recipe-overrides nil)
;; (setq straight-check-for-modifications 'live-with-find)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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
(setq use-package-always-defer t)

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

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
  :straight (:host github
                   :repo "raxod502/el-patch"
                   :branch "develop")
  :demand t)

(use-package bind-key)

;;;; Environment

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-check-startup-files nil)

  :init
  (when is-mac
    (setq exec-path
          (or (eval-when-compile
                (require 'cl-lib)
                (exec-path-from-shell-initialize)
                (cl-remove-duplicates exec-path :test #'string=))
              exec-path))))

;; If you have something on the system clipboard, and then kill
;; something in Emacs, then by default whatever you had on the system
;; clipboard is gone and there is no way to get it back. Setting the
;; following option makes it so that when you kill something in Emacs,
;; whatever was previously on the system clipboard is pushed into the
;; kill ring. This way, you can paste it with `yank-pop'.
(setq save-interprogram-paste-before-kill t)

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
          ("C-c k" . counsel-rg))
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

;; Split windows horizontally (into tall subwindows) rather than
;; vertically (into wide subwindows) by default.
(el-patch-defun split-window-sensibly (&optional window)
  "Split WINDOW in a way suitable for `display-buffer'.
WINDOW defaults to the currently selected window.
If `split-height-threshold' specifies an integer, WINDOW is at
least `split-height-threshold' lines tall and can be split
vertically, split WINDOW into two windows one above the other and
return the lower window.  Otherwise, if `split-width-threshold'
specifies an integer, WINDOW is at least `split-width-threshold'
columns wide and can be split horizontally, split WINDOW into two
windows side by side and return the window on the right.  If this
can't be done either and WINDOW is the only window on its frame,
try to split WINDOW vertically disregarding any value specified
by `split-height-threshold'.  If that succeeds, return the lower
window.  Return nil otherwise.
By default `display-buffer' routines call this function to split
the largest or least recently used window.  To change the default
customize the option `split-window-preferred-function'.
You can enforce this function to not split WINDOW horizontally,
by setting (or binding) the variable `split-width-threshold' to
nil.  If, in addition, you set `split-height-threshold' to zero,
chances increase that this function does split WINDOW vertically.
In order to not split WINDOW vertically, set (or bind) the
variable `split-height-threshold' to nil.  Additionally, you can
set `split-width-threshold' to zero to make a horizontal split
more likely to occur.
Have a look at the function `window-splittable-p' if you want to
know how `split-window-sensibly' determines whether WINDOW can be
split."
  (let ((window (or window (selected-window))))
    (or (el-patch-let
            (($fst (and (window-splittable-p window)
                        ;; Split window vertically.
                        (with-selected-window window
                          (split-window-below))))
             ($snd (and (window-splittable-p window t)
                        ;; Split window horizontally.
                        (with-selected-window window
                          (split-window-right)))))
          (el-patch-swap $fst $snd)
          (el-patch-swap $snd $fst))
        (and
         ;; If WINDOW is the only usable window on its frame (it
         ;; is the only one or, not being the only one, all the
         ;; other ones are dedicated) and is not the minibuffer
         ;; window, try to split it s/vertically/horizontally
         ;; disregarding the value of `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
         (not (window-minibuffer-p window))
         (let ((split-height-threshold 0))
           (when (window-splittable-p window)
             (with-selected-window window
               (split-window-below))))))))

(use-feature windmove
  :demand t
  :config

  (windmove-default-keybindings))

(use-feature winner
  :demand t
  :config

  (winner-mode +1))

(use-package ace-window
  :bind (("C-x o" . ace-window)))

;; Package `transpose-frame' provides simple commands to mirror,
;; rotate, and transpose Emacs windows: `flip-frame', `flop-frame',
;; `transpose-frame', `rotate-frame-clockwise',
;; `rotate-frame-anticlockwise', `rotate-frame'.
(use-package transpose-frame)

;; Package `buffer-move' provides simple commands to swap Emacs
;; windows: `buf-move-up', `buf-move-down', `buf-move-left',
;; `buf-move-right'.
(use-package buffer-move)

;; Feature `ibuffer' provides a more modern replacement for the
;; `list-buffers' command.
(use-feature ibuffer
  :bind (([remap list-buffers] . ibuffer)))

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

;; Don't bother with auto save and backups.
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

(bind-key "M-c" #'capitalize-dwim)
(bind-key "M-l" #'downcase-dwim)
(bind-key "M-u" #'upcase-dwim)

;; When filling paragraphs, assume that sentences end with one space
;; rather than two.
(setq sentence-end-double-space nil)

(use-feature warnings
  :config

  ;; Ignore the warning we get when a huge buffer is reverted and the
  ;; undo information is too large to be recorded.
  (add-to-list 'warning-suppress-log-types '(undo discard-info)))

(use-package undo-tree
  :demand t
  :bind (;; By default, `undo' (and by extension `undo-tree-undo') is
         ;; bound to C-_ and C-/, and `undo-tree-redo' is bound to
         ;; M-_. It's logical to also bind M-/ to `undo-tree-redo'.
         ;; This overrides the default binding of M-/, which is to
         ;; `dabbrev-expand'.
         :map undo-tree-map
         ("M-/" . undo-tree-redo))
  :config

  (global-undo-tree-mode +1)

  ;; Disable undo-in-region. It sounds like a cool feature, but
  ;; unfortunately the implementation is very buggy and usually causes
  ;; you to lose your undo history if you use it by accident.
  (setq undo-tree-enable-undo-in-region nil)

  :blackout t)

(use-feature subword
  :demand t
  :config

  (global-subword-mode +1)

  :blackout t)

;; Visually find and replace text
(use-package visual-regexp
  :bind (("M-%" . vr/query-replace)))

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
  :bind (("C-c C-SPC" . avy-goto-char)))

(use-package deadgrep
  :commands (deadgrep))

(use-package wgrep)

(use-package phi-search
  :disabled t
  :bind (("C-s" . phi-search)
         ("C-r" . phi-search-backward)))

(bind-key "s-u" #'revert-buffer)

;; Revert (update) buffers automatically when underlying files are changed externally.
(setq auto-revert-interval 1)
(setq revert-without-query '(".*"))
(global-auto-revert-mode +1)

;; Package `smartparens' provides an API for manipulating paired
;; delimiters of many different types, as well as interactive commands
;; and keybindings for operating on paired delimiters at the
;; s-expression level. It provides a Paredit compatibility layer.
(use-package smartparens
  :demand t
  :config

  ;; Load the default pair definitions for Smartparens.
  (require 'smartparens-config)

  ;; Enable Smartparens functionality in all buffers.
  (smartparens-global-mode +1)

  ;; When in Paredit emulation mode, Smartparens binds M-( to wrap the
  ;; following s-expression in round parentheses. By analogy, we
  ;; should bind M-[ to wrap the following s-expression in square
  ;; brackets. However, this breaks escape sequences in the terminal,
  ;; so it may be controversial upstream. We only enable the
  ;; keybinding in windowed mode.
  (when (display-graphic-p)
    (setf (map-elt sp-paredit-bindings "M-[") #'sp-wrap-square))

  ;; Set up keybindings for s-expression navigation and manipulation
  ;; in the style of Paredit.
  (sp-use-paredit-bindings)

  ;; Highlight matching delimiters.
  (show-smartparens-global-mode +1)

  ;; Prevent all transient highlighting of inserted pairs.
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)

  ;; Don't disable autoskip when point moves backwards. (This lets you
  ;; open a sexp, type some things, delete some things, etc., and then
  ;; type over the closing delimiter as long as you didn't leave the
  ;; sexp entirely.)
  (setq sp-cancel-autoskip-on-backward-movement nil)

  ;; Disable Smartparens in Org-related modes, since the keybindings
  ;; conflict.
  (add-to-list 'sp-ignore-modes-list #'org-mode)
  (add-to-list 'sp-ignore-modes-list #'org-agenda-mode)

  ;; Make C-k kill the sexp following point in Lisp modes, instead of
  ;; just the current line.
  (bind-key [remap kill-line] #'sp-kill-hybrid-sexp smartparens-mode-map
            (apply #'derived-mode-p sp-lisp-modes))

  ;; When pressing RET after a newly entered pair, add an extra
  ;; newline and indent. See
  ;; https://github.com/Fuco1/smartparens/issues/80#issuecomment-18910312.
  ;;
  ;; What is currently here is to be considered a hack.

  (defun +smartparens-indent-new-pair (&rest _)
    "Insert an extra newline after point, and reindent."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  ;; Smartparens is broken in `cc-mode' as of Emacs 27. See
  ;; <https://github.com/Fuco1/smartparens/issues/963>.
  (when (version<= "27" emacs-version)
    (dolist (fun '(c-electric-paren c-electric-brace))
      (add-to-list 'sp--special-self-insert-commands fun)))

  (dolist (mode '(c-mode c++-mode css-mode objc-mode java-mode
                         js2-mode json-mode lua-mode
                         python-mode sh-mode web-mode go-mode
                         protobuf-mode typescript-mode))
    (sp-local-pair mode "{" nil :post-handlers
                   '((+smartparens-indent-new-pair "RET")
                     (+smartparens-indent-new-pair "<return>"))))

  (dolist (mode '(js2-mode json-mode python-mode web-mode))
    (sp-local-pair mode "[" nil :post-handlers
                   '((+smartparens-indent-new-pair "RET")
                     (+smartparens-indent-new-pair "<return>"))))

  (dolist (mode '(python-mode sh-mode js2-mode lua-mode go-mode
                              typescript-mode))
    (sp-local-pair mode "(" nil :post-handlers
                   '((+smartparens-indent-new-pair "RET")
                     (+smartparens-indent-new-pair "<return>"))))

  (dolist (mode '(python-mode))
    (sp-local-pair mode "\"\"\"" "\"\"\"" :post-handlers
                   '((+smartparens-indent-new-pair "RET")
                     (+smartparens-indent-new-pair "<return>"))))

  ;; Work around https://github.com/Fuco1/smartparens/issues/783.
  (setq sp-escape-quotes-after-insert nil)

  :blackout t)

(use-feature abbrev
  :blackout t)

(use-package yasnippet
  :bind (:map yas-minor-mode-map

              ;; Disable TAB from expanding snippets, as I don't use it and
              ;; it's annoying.
              ("TAB" . nil)
              ("<tab>" . nil))
  :config

  ;; Reduce verbosity. The default value is 3. Bumping it down to 2
  ;; eliminates a message about successful snippet lazy-loading setup
  ;; on every(!) Emacs init. Errors should still be shown.
  (setq yas-verbosity 2)

  ;; Make it so that Company's keymap overrides Yasnippet's keymap
  ;; when a snippet is active. This way, you can TAB to complete a
  ;; suggestion for the current field in a snippet, and then TAB to
  ;; move to the next field. Plus, C-g will dismiss the Company
  ;; completions menu rather than cancelling the snippet and moving
  ;; the cursor while leaving the completions menu on-screen in the
  ;; same location.
  (use-feature company
    :config

    ;; This function translates the "event types" I get from
    ;; `map-keymap' into things that I can pass to `lookup-key' and
    ;; `define-key'. It's a hack, and I'd like to find a built-in
    ;; function that accomplishes the same thing while taking care of
    ;; any edge cases I might have missed in this ad-hoc solution.
    (defun +yasnippet-normalize-event (event)
      "This function is a complete hack, do not use.
But in principle, it translates what we get from `map-keymap'
into what `lookup-key' and `define-key' want."
      (if (vectorp event)
          event
        (vector event)))

    ;; Here we define a hybrid keymap that delegates first to
    ;; `company-active-map' and then to `yas-keymap'.
    (defvar +yasnippet-then-company-keymap
      ;; It starts out as a copy of `yas-keymap', and then we
      ;; merge in all of the bindings from `company-active-map'.
      (let ((keymap (copy-keymap yas-keymap)))
        (map-keymap
         (lambda (event company-cmd)
           (let* ((event (+yasnippet-normalize-event event))
                  (yas-cmd (lookup-key yas-keymap event)))
             ;; Here we use an extended menu item with the
             ;; `:filter' option, which allows us to dynamically
             ;; decide which command we want to run when a key is
             ;; pressed.
             (define-key keymap event
               `(menu-item
                 nil ,company-cmd :filter
                 (lambda (cmd)
                   ;; There doesn't seem to be any obvious
                   ;; function from Company to tell whether or not
                   ;; a completion is in progress (à la
                   ;; `company-explicit-action-p'), so I just
                   ;; check whether or not `company-my-keymap' is
                   ;; defined, which seems to be good enough.
                   (if company-my-keymap
                       ',company-cmd
                     ',yas-cmd))))))
         company-active-map)
        keymap)
      "Keymap which delegates to both `company-active-map' and `yas-keymap'.
The bindings in `company-active-map' only apply if Company is
currently active.")

    (radian-defadvice radian--advice-company-overrides-yasnippet
        (yas--make-control-overlay &rest args)
      :around yas--make-control-overlay
      "Allow `company' keybindings to override those of `yasnippet'."
      ;; The function `yas--make-control-overlay' uses the current
      ;; value of `yas-keymap' to build the Yasnippet overlay, so to
      ;; override the Yasnippet keymap we only need to dynamically
      ;; rebind `yas-keymap' for the duration of that function.
      (let ((yas-keymap +yasnippet-then-company-keymap))
        (apply yas--make-control-overlay args))))

  :blackout yas-minor-mode)

;;;; Indentation

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

(defun +indent-defun ()
  "Indent the surrounding defun."
  (interactive)
  (save-excursion
    (when (beginning-of-defun)
      (let ((beginning (point)))
        (end-of-defun)
        (let ((end (point)))
          (let ((inhibit-message t)
                (message-log-max nil))
            (indent-region beginning end)))))))

(bind-key* "C-M-q" #'+indent-defun)


;;;; Autocompletion

;;; Definition location

;; Package `dumb-jump' provides a mechanism to jump to the definitions
;; of functions, variables, etc. in a variety of programming
;; languages. The advantage of `dumb-jump' is that it doesn't try to
;; be clever, so it "just works" instantly for dozens of languages
;; with zero configuration.
(use-package dumb-jump
  :init

  (dumb-jump-mode +1)

  :bind (:map dumb-jump-mode-map
              ("M-Q" . dumb-jump-quick-look))
  :bind* (("C-M-d" . dumb-jump-go-prompt)
          ([remap evil-jump-to-tag] . dumb-jump-go)
          ("C-x 4 g" . dumb-jump-go-other-window)
          ("C-x 4 d" . +dumb-jump-go-prompt-other-window))
  :config

  (defun +dumb-jump-go-prompt-other-window ()
    "Like `dumb-jump-go-prompt' but use a different window."
    (interactive)
    (let ((dumb-jump-window 'other))
      (dumb-jump-go-prompt))))

(use-feature eldoc
  :demand t

  :config

  (setq eldoc-echo-area-use-multiline-p nil)

  :blackout t)

(use-package flycheck
  :defer 4
  :bind-keymap (("C-c !" . flycheck-command-map))
  :config

  (setq-default flycheck-disabled-checkers '(less less-stylelin less-stylelintt))

  ;; (setq flycheck-ruby-rubocop-executable "/Users/miguelsantos/.rbenv/versions/2.3.7/lib/ruby/gems/2.3.0/gems/rubocop-0.46.0/bin/rubocop")

  (global-flycheck-mode +1)

  ;; Run a syntax check when changing buffers, just in case you
  ;; modified some other files that impact the current one. See
  ;; https://github.com/flycheck/flycheck/pull/1308.
  (add-to-list 'flycheck-check-syntax-automatically 'idle-buffer-switch)

  ;; (setq flycheck-indication-mode 'left-fringe)
  ;; ;; A non-descript, left-pointing arrow
  ;; (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
  ;;   [16 48 112 240 112 48 16] nil nil 'center)

  ;; For the above functionality, check syntax in a buffer that you
  ;; switched to only briefly. This allows "refreshing" the syntax
  ;; check state for several buffers quickly after e.g. changing a
  ;; config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors in the echo area after only 0.2 seconds, not 0.9.
  (setq flycheck-display-errors-delay 0.2)
  :blackout t)

;; Store custom-file separately, don't freak out when it's not found
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; remember risk variables (dir-locals)
(defun risky-local-variable-p (sym &optional _ignored) nil)

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

(setq default-directory "~/" )

;; Smooth scrolling
(setq scroll-margin 1)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;; Slower scrolling on linux
(when is-linux
  (setq mouse-wheel-scroll-amount '(1)))

(when is-linux
  (setq x-select-enable-primary nil
        x-select-enable-clipboard t
        interprogram-paste-function 'x-cut-buffer-or-selection-value))

(setq tab-always-indent 'complete)

;; Always prefer newer files
(setq load-prefer-newer +1)

;; Warn only when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Move file to trash instead of removing.
(setq-default delete-by-moving-to-trash t)

(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

(setq cursor-in-non-selected-windows t)

(setq echo-keystrokes 1e-6)

; Select help window so it's easy to quit it with 'q'
;; (setq  help-window-select t)

(setq fill-column 80)

;; never kill *scratch* buffer
(add-hook 'kill-buffer-query-functions
          (lambda() (not (equal (buffer-name) "*scratch*"))))

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)      ; y and n instead of yes and no everywhere else
(global-unset-key (kbd "s-p"))     ; Don't print

;; Feature `delsel' provides an alternative behavior for certain
;; actions when you have a selection active. Namely: if you start
;; typing when you have something selected, then the selection will be
;; deleted; and if you press DEL while you have something selected, it
;; will be deleted rather than killed. (Otherwise, in both cases the
;; selection is deselected and the normal function of the key is
;; performed.)
(use-feature delsel
  :demand t
  :config

  (delete-selection-mode +1))

(setq ring-bell-function 'ignore)

;; Delete trailing spaces and add new line in the end of a file on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

(use-feature recentf
  :demand t

  :config
  (recentf-mode +1)
  (setq recentf-max-saved-items 100)

  :blackout t)

;; =======
;; VISUALS

;; Enable transparent title bar on macOS
(when is-mac
  (add-to-list 'default-frame-alist '(ns-appearance . light)) ;; {light, dark}
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(set-frame-font "mononoki-14" nil)

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

;; (use-package zenburn-theme)
;; (use-package solarized-theme)

(load-theme 'tsdh-light t)

;; Hide toolbar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(when is-linux (menu-bar-mode -1))

;; Disable blinking cursor.
(blink-cursor-mode +1)

;; Always wrap lines
(global-visual-line-mode +1)

;; Show line numbers
(global-display-line-numbers-mode +1)
(define-key global-map (kbd "C-x l") 'global-display-line-numbers-mode)

;; Highlight current line
(global-hl-line-mode +1)

;; Display dir if two files have the same name
(use-feature uniquify
  :init

  (progn
    (setq uniquify-buffer-name-style 'reverse
          uniquify-separator "|"
          uniquify-after-kill-buffer-p t
          uniquify-ignore-buffers-re "^\\*"))

  :blackout t)

;; Show full path in the title bar.
;; (setq-default frame-title-format "%b (%f)")
(setq-default frame-title-format "")
(setq ns-use-proxy-icon nil)

;; ================
;; BASIC NAVIGATION

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
  :bind (("C-=" . #'er/expand-region)
         ("C-'" . #'er/contract-region)))

(use-package change-inner
  :bind (("M-i" . #'change-inner)
         ("M-o" . #'change-outer)))

(defvar indent-sensitive-modes '())

(use-package smart-newline
  :demand t
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

(use-package multiple-cursors
  :bind (("s-d" . mc/mark-next-like-this)
         ("s-D" . mc/mark-previous-like-this)
         ("C-c s-d" . mc/mark-all-like-this-dwim))
  :config
  (multiple-cursors-mode +1))

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

  (define-key global-map (kbd "C-x C-j") 'dired-jump)
  (define-key global-map [remap dired] 'dired-jump)

  (put 'dired-find-alternate-file 'disabled nil)

  ;; Delete with C-x C-k to match file buffers and magit
  (define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)

  (eval-after-load "wdired"
    '(progn
       (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
       (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
       (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom))))

;; ===============
;; CODE COMPLETION

;; Package `company' provides an in-buffer autocompletion framework.
;; It allows for packages to define backends that supply completion
;; candidates, as well as optional documentation and source code. Then
;; Company allows for multiple frontends to display the candidates,
;; such as a tooltip menu. Company stands for "Complete Anything".
(use-package company
  :defer 3
  :init

  (defvar +company-backends-global
    '(company-capf
      company-files
      (company-dabbrev-code company-keywords)
      company-dabbrev)
    "Values for `company-backends' used everywhere.
If `company-backends' is overridden by Radian, then these
backends will still be included.")

  :bind (;; Remap the standard Emacs keybindings for invoking
         ;; completion to instead use Company. You might think this
         ;; could be put in the `:bind*' declaration below, but it
         ;; seems that `bind-key*' does not work with remappings.
         ([remap completion-at-point] . company-manual-begin)
         ([remap complete-symbol] . company-manual-begin)

         ;; The following are keybindings that take effect whenever
         ;; the completions menu is visible, even if the user has not
         ;; explicitly interacted with Company.

         :map company-active-map

         ;; Make TAB always complete the current selection, instead of
         ;; only completing a common prefix.
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)

         ;; The following are keybindings that only take effect if the
         ;; user has explicitly interacted with Company. Note that
         ;; `:map' from above is "sticky", and applies also below: see
         ;; https://github.com/jwiegley/use-package/issues/334#issuecomment-349473819.

         :filter (company-explicit-action-p)

         ;; Make RET trigger a completion if and only if the user has
         ;; explicitly interacted with Company, instead of always
         ;; doing so.
         ("<return>" . company-complete-selection)
         ("RET" . company-complete-selection)

         ;; We then make <up> and <down> abort the completions menu
         ;; unless the user has interacted explicitly. Note that we
         ;; use `company-select-previous' instead of
         ;; `company-select-previous-or-abort'. I think the former
         ;; makes more sense since the general idea of this `company'
         ;; configuration is to decide whether or not to steal
         ;; keypresses based on whether the user has explicitly
         ;; interacted with `company', not based on the number of
         ;; candidates.
         ;;
         ;; Note that M-p and M-n work regardless of whether explicit
         ;; interaction has happened yet, and note also that M-TAB
         ;; when the completions menu is open counts as an
         ;; interaction.
         ("<up>" . company-select-previous)
         ("<down>" . company-select-next))

  :bind* (;; The default keybinding for `completion-at-point' and
          ;; `complete-symbol' is M-TAB or equivalently C-M-i. We
          ;; already remapped those bindings to `company-manual-begin'
          ;; above. Here we make sure that they definitely invoke
          ;; `company-manual-begin' even if a minor mode binds M-TAB
          ;; directly.
          ("M-TAB" . company-manual-begin))

  :config

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-numbers t)

  ;; Prevent non-matching input (which will dismiss the completions
  ;; menu), but only if the user interacts explicitly with Company.
  (setq company-require-match #'company-explicit-action-p)

  ;; Company appears to override our settings in `company-active-map'
  ;; based on `company-auto-complete-chars'. Turning it off ensures we
  ;; have full control.
  (setq company-auto-complete-chars nil)

  ;; Only search the current buffer to get suggestions for
  ;; `company-dabbrev' (a backend that creates suggestions from text
  ;; found in your buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open.
  (setq company-dabbrev-other-buffers nil)

  ;; Make the `company-dabbrev' backend fully case-sensitive, to
  ;; improve the UX when working with domain-specific words that have
  ;; particular casing.
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)

  ;; When candidates in the autocompletion tooltip have additional
  ;; metadata, like a type signature, align that information to the
  ;; right-hand side. This usually makes it look neater.
  (setq company-tooltip-align-annotations t)

  (global-company-mode +1)

  :blackout t)


;; Package `company-prescient' provides intelligent sorting and
;; filtering for candidates in Company completions.
(use-package company-prescient
  :demand t
  :after company
  :config

  ;; Use `prescient' for Company menus.
  (company-prescient-mode +1))

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

      (progn
        (he-substitute-string expansion t)
        t)))))

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
(global-set-key (kbd "C-,") 'completion-at-point)

;; ===========
;; PROGRAMMING

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

;; ========
;; ORG MODE

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

(use-package coffee-mode
  :mode "\\.coffee\\.*"
  :custom
  (coffee-args-repl '("-i" "--nodejs"))
  (coffee-tab-width 2)
  (coffee-indent-like-python-mode t)
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

(use-package rjsx-mode
  :after js2-mode)

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

  (setq ruby-insert-encoding-magic-comment nil)
  (setq enh-ruby-add-encoding-comment-on-save nil)

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
  (add-hook 'enh-ruby-mode-hook #'+detect-rbenv-version)
  (global-rbenv-mode 1))

(use-package robe
  :hook (ruby-mode . robe-mode)
  :bind (([remap evil-jump-to-tag] . robe-jump))

  :config
  (after company
    (push 'company-robe company-backends))

  :blackout t)

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

;; (use-package projectile-rails
;;   :after projectile
;;   :config
;;   (setq projectile-rails-keymap-prefix (kbd "C-c r"))
;;   (projectile-rails-global-mode 1))

(use-package feature-mode
  :bind (("C-c C-p" . #'+cycle-selenium-phantomjs)
         ("C-c C-d" . #'+add-debug-line))
  :mode "\\.feature$"
  :config

  (defun +add-debug-line ()
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

(use-package clojure-mode
  :config/el-patch

  ;; `clojure-mode' does not correctly identify the docstrings of
  ;; protocol methods as docstrings, and as such electric indentation
  ;; does not work for them. Additionally, when you hack a
  ;; clojure.core function, such as defonce or defrecord, to provide
  ;; docstring functionality, those docstrings are (perhaps rightly,
  ;; but annoyingly) not recognized as docstrings either. However,
  ;; there is an easy way to get electric indentation working for all
  ;; potential docstrings: simply tell `clojure-mode' that *all*
  ;; strings are docstrings. This will not change the font locking,
  ;; because for some weird reason `clojure-mode' determines whether
  ;; you're in a docstring by the font color instead of the other way
  ;; around. Note that this will cause electric indentation by two
  ;; spaces in *all* multiline strings, but since there are not very
  ;; many non-docstring multiline strings in Clojure this is not too
  ;; inconvenient.
  ;;
  ;; Unfortunately, `clojure-in-docstring-p' is defined as an inline
  ;; function, so after changing it, we also have to replace
  ;; `clojure-indent-line'. That is done in an advice in the
  ;; `radian-clojure-strings-as-docstrings-mode' minor mode.

  (defsubst (el-patch-swap clojure-in-docstring-p
                           radian--clojure-in-string-p)
    ()
    (el-patch-concat
      "Check whether point is in "
      (el-patch-swap "a docstring" "any type of string")
      ".")
    (let ((ppss (syntax-ppss)))
      ;; are we in a string?
      (when (nth 3 ppss)
        ;; check font lock at the start of the string
        ((el-patch-swap eq memq)
         (get-text-property (nth 8 ppss) 'face)
         (el-patch-wrap 1
           ('font-lock-string-face 'font-lock-doc-face))))))

  (defun (el-patch-swap clojure-indent-line
                        radian--advice-clojure-strings-as-docstrings)
      ()
    (el-patch-concat
      "Indent current line as Clojure code."
      (el-patch-add
        "\n\nThis is an `:override' advice for `clojure-indent-line'."))
    (if (el-patch-swap
          (clojure-in-docstring-p)
          (radian--clojure-in-string-p))
        (save-excursion
          (beginning-of-line)
          (when (and (looking-at "^\\s-*")
                     (<= (string-width (match-string-no-properties 0))
                         (string-width (clojure-docstring-fill-prefix))))
            (replace-match (clojure-docstring-fill-prefix))))
      (lisp-indent-line)))

  :config

  ;; Customize indentation like this:
  ;;
  ;; (some-function
  ;;   argument
  ;;   argument)
  ;;
  ;; (some-function argument
  ;;                argument)
  ;;
  ;; (-> foo
  ;;   thread
  ;;   thread)
  ;;
  ;; (->> foo
  ;;   thread
  ;;   thread)
  ;;
  ;; (:keyword
  ;;   map)

  (setq clojure-indent-style :align-arguments)

  ;; Ideally, we would be able to set the identation rules for *all*
  ;; keywords at the same time. But until we figure out how to do
  ;; that, we just have to deal with every keyword individually. See
  ;; https://github.com/raxod502/radian/issues/26.
  (radian-protect-macros
    (define-clojure-indent
      (-> 1)
      (->> 1)
      (:import 0)
      (:require 0)
      (:use 0)))

  (define-minor-mode radian-clojure-strings-as-docstrings-mode
    "Treat all Clojure strings as docstrings.
You want to turn this on if you want to treat strings like
docstrings even though they technically are not, and you want to
turn it off if you have multiline strings that are not
docstrings."
    nil nil nil
    (if radian-clojure-strings-as-docstrings-mode
        (advice-add #'clojure-indent-line :override
                    #'radian--advice-clojure-strings-as-docstrings)
      (advice-remove #'clojure-indent-line
                     #'radian--advice-clojure-strings-as-docstrings))))

;; Package `cider' provides integrated Clojure and ClojureScript REPLs
;; directly in Emacs, a Company backend that uses a live REPL
;; connection to retrieve completion candidates, and documentation and
;; source lookups for Clojure code.
(use-package cider
  :config

  ;; By default, any error messages that occur when CIDER is starting
  ;; up are placed in the *nrepl-server* buffer and not in the
  ;; *cider-repl* buffer. This is silly, since no-one wants to check
  ;; *nrepl-server* every time they start a REPL, and if you don't
  ;; then startup errors (including errors in anything loaded by the
  ;; :main namespace) are effectively silenced. So we copy everything
  ;; from the *nrepl-server* buffer to the *cider-repl* buffer, as
  ;; soon as the latter is available.
  ;;
  ;; Note that this does *not* help in the case of things going so
  ;; horribly wrong that the REPL can't even start. In this case you
  ;; will have to check the *nrepl-server* buffer manually. Perhaps an
  ;; error message that is visible from any buffer could be added in
  ;; future.
  ;;
  ;; Thanks to malabarba on Clojurians Slack for providing the
  ;; following code:

  (radian-defhook radian--cider-dump-nrepl-server-log ()
    cider-connected-hook
    "Copy contents of *nrepl-server* to beginning of *cider-repl*."
    (save-excursion
      (goto-char (point-min))
      (insert
       (with-current-buffer nrepl-server-buffer
         (buffer-string)))))

  ;; Use the :emacs profile defined in profiles.clj. This enables lots
  ;; of cool extra features in the REPL.
  ;; (setq cider-lein-parameters "with-profile +emacs repl :headless")

  ;; The CIDER welcome message often obscures any error messages that
  ;; the above code is supposed to be making visible. So, we need to
  ;; turn off the welcome message.
  (setq cider-repl-display-help-banner nil)

  ;; Sometimes in the CIDER REPL, when Emacs is running slowly, you
  ;; can manage to press TAB before the Company completions menu pops
  ;; up. This triggers a `completing-read', which is disorienting. So
  ;; we reset TAB to its default functionality (i.e. indent only) in
  ;; the CIDER REPL.
  (setq cider-repl-tab-command 'indent-for-tab-command)

  ;; Don't focus the cursor in the CIDER REPL once it starts. Since
  ;; the REPL takes so long to start up, especially for large
  ;; projects, you either have to wait for a minute without doing
  ;; anything or be prepared for your cursor to suddenly shift buffers
  ;; without warning sometime in the near future. This is annoying, so
  ;; turn off the behavior. For a historical perspective see [1].
  ;;
  ;; [1]: https://github.com/clojure-emacs/cider/issues/1872
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)

  ;; Use figwheel-sidecar for launching ClojureScript REPLs. This
  ;; supports a fully integrated ClojureScript development experience
  ;; in Emacs, for use with e.g. [1]. The last three forms are from
  ;; the definition of `cider--cljs-repl-types'; the first two work
  ;; around [2].
  ;;
  ;; [1]: https://github.com/reagent-project/reagent-template
  ;; [2]: https://github.com/reagent-project/reagent-template/issues/132
  (setq cider-cljs-lein-repl
        "(do
  (require 'clojure.java.shell)
  (clojure.java.shell/sh \"lein\" \"clean\")
  (require 'figwheel-sidecar.repl-api)
  (figwheel-sidecar.repl-api/start-figwheel!)
  (figwheel-sidecar.repl-api/cljs-repl))")

  :blackout t)

;; Package `clj-refactor' provides automated refactoring commands for
;; Clojure code.
(use-package clj-refactor
  :init

  (radian-defhook radian--clj-refactor-enable ()
    clojure-mode-hook
    "Enable `clj-refactor' mode properly.
This means that `yas-minor-mode' also needs to be enabled, and
the `clj-refactor' keybindings need to be installed."
    (clj-refactor-mode +1)
    (yas-minor-mode +1)
    (cljr-add-keybindings-with-prefix "C-c RET"))

  :config

  ;; Make clj-refactor show its messages right away, instead of
  ;; waiting for you to do another command.

  (radian-defadvice radian--advice-clj-refactor-message-eagerly (&rest args)
    :override cljr--post-command-message
    "Make `clj-refactor' show messages right away.
Otherwise, it waits for you to do another command, and then
overwrites the message from *that* command."
    (apply #'message args))

  ;; Automatically sort project dependencies after changing them.
  (setq cljr-auto-sort-project-dependencies t)

  ;; Don't print a warning when starting a REPL outside of project
  ;; context.
  (setq cljr-suppress-no-project-warning t)

  :blackout t)

;;;; Utilities

(defun copy-buffer-file-name ()
  "Copy buffer's full path."
  (interactive)
  (when buffer-file-name
    (kill-new (git-file-path (file-truename buffer-file-name)))))

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
  :bind (("C-c i" . string-inflection-cycle)
         ("C-c C" . string-inflection-camelcase)
         ("C-c L" . string-inflection-lower-camelcase)
         ("C-c J" . string-inflection-java-style-cycle)))

(use-package crux
  :bind (("C-k" . crux-smart-kill-line)
         ("C-a" . crux-move-beginning-of-line)
         ("C-c o" . crux-open-with)
         ([(shift return)] . crux-smart-open-line)
         ("s-<backspace>" . crux-kill-whole-line)
         ("C-c n" . crux-cleanup-buffer-or-region)))

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
              (insert new-quotes)
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

    (defun +join-line-indent ()
      (interactive)
      (save-excursion
        (forward-line +1)
        (join-line)
        (indent-according-to-mode)))

    (defun newline-anywhere ()
      "Add a newline from anywhere in the line."
      (interactive)
      (end-of-line)
      (newline-and-indent))

    (global-set-key (kbd "C-M-j") #'+join-line-indent)
    (global-set-key (kbd "C-w") 'kill-region-or-backward-word)
    (global-set-key (kbd "C-c C-j") 'duplicate-current-line-or-region)))
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

  ;; (defun +magit-display-popup-buffer (buffer &optional alist)
  ;;   "TODO"
  ;;   (cond ((eq (window-dedicated-p) 'side)
  ;;          (if (fboundp '+popup-display-buffer-stacked-side-window)
  ;;              (+popup-display-buffer-stacked-side-window buffer alist)
  ;;            (display-buffer-in-side-window buffer alist)))
  ;;         ((derived-mode-p 'magit-mode)
  ;;          (display-buffer-below-selected buffer alist))
  ;;         ((display-buffer-in-side-window buffer alist))))

  (setq magit-display-buffer-function #'+magit-display-buffer)
  ;; (setq magit-popup-display-buffer-action '(+magit-display-popup-buffer))

  (defun enforce-git-commit-conventions ()
    "See https://chris.beams.io/posts/git-commit/"
    (setq fill-column 72
          git-commit-summary-max-length 50
          git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line)))
  (add-hook 'git-commit-mode-hook #'enforce-git-commit-conventions))

(use-package git-timemachine)

;; Show changes in the gutter
(use-package git-gutter-fringe
  :defer 2

  :config

  (global-git-gutter-mode +1)

  (setq-default fringes-outside-margins t)

  ;; (define-fringe-bitmap 'git-gutter-fr:added [224]
  ;;   nil nil '(center repeated))
  ;; (define-fringe-bitmap 'git-gutter-fr:modified [224]
  ;;   nil nil '(center repeated))
  ;; (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
  ;;   nil nil 'bottom)

  ;; (set-face-background 'git-gutter:modified 'nil)   ;; background color
  ;; (set-face-foreground 'git-gutter:added "green4")
  ;; (set-face-foreground 'git-gutter:deleted "red")

  :blackout git-gutter-mode)

;; Package `git-link' provides a simple function M-x git-link which
;; copies to the kill ring a link to the current line of code or
;; selection on GitHub, GitLab, etc.
(use-package git-link
  :config

  ;; Link to a particular revision of a file rather than using the
  ;; branch name in the URL.
  (setq git-link-use-commit t))

(global-set-key (kbd "C-x C") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(use-package restart-emacs)
