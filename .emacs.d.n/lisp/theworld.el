;;; theworld.el --- make the thing
;;; commentary:
;;; functions             | ms/asdf
;;; pred functions        | ms/asdf-p
;;; interactive functions | ms/asdf
;;; enable vars           | ms/enable-asdf-p
;;; vars                  | ms/asdf
;;; buffer local vars     | ms/enable-asdf
;;; code:

(setq
 ms/enable-osx-p (eq system-type 'darwin)
 ms/enable-linux-p (eq system-type 'gnu/linux)
 ms/enable-work-p ms/enable-osx-p
 )

(setq ms/xrdb-fallback-values
      ;; for when we're away from $HOME.
      `(("*.background"         . ,(face-attribute 'default :background))
        ("Emacs.powerlinescale" . "1.1")
        ("Emacs.theme"          . "base16-grayscale-light")
        ("emacs.powerline"      . "bar")
        ("st.borderpx"          . "10")
        ("st.font"              . "Go Mono-13")
        ("st.font_variable"     . "Go-13")
        ))

(defmacro defconfig-base (label &rest body)
  `(defun ,(intern (concat "ms/" (prin1-to-string label))) nil
     ,@body))

;; commander
(defmacro defconfig (label &rest body)
  `(defconfig-base ,label
     (let ((config-name ,(prin1-to-string label)))
       (message (concat "loading " config-name "..."))
       (catch 'config-catch
         (setq ,(intern (format "ms/enable-%s-p" (prin1-to-string label))) nil)
         ,@body
         (setq ,(intern (format "ms/enable-%s-p" (prin1-to-string label))) t)
         ))))

;; guards!
(defmacro ms/guard (&rest conditions)
  (if (not (eval (cons 'and conditions)))
      '(when t (throw 'config-catch (concat "config guard " config-name)))))

;; interactive
(defmacro defcommand (label args &rest body)
  `(defun ,(intern (concat "ms/" (prin1-to-string label))) ,args
     (interactive)
     ,@body))

(defconfig use-package
  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))

  (package-initialize)

  ;; Bootstrap `use-package'
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (eval-when-compile
    (require 'use-package))

  (setq use-package-always-ensure t)
  )

(defconfig straight
  (let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
        (bootstrap-version 2))
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
  (setq straight-cache-autoloads t)
  )

(defconfig bedrock
  (use-package s)
  (use-package f)
  (use-package hydra)
  (use-package general)
  (use-package request)
  (require 'seq)

  (defmacro ms/shell-exec(command)
    "trim the newline from shell exec"
    `(replace-regexp-in-string "\n$" ""
       (shell-command-to-string ,command)))

  (defun ms/shell-exec-dontcare (command)
    ;; todo here: junk buffer
    (let* (
            (bufname (concat "*killme-shell" (number-to-string (random)) "*"))
            (junk-buffer (get-buffer-create bufname))
            )
      (shell-command command junk-buffer)
      (kill-buffer junk-buffer)))


  (defun mapcar* (f &rest xs)
    "MAPCAR for multiple sequences F XS."
    (if (not (memq nil xs))
      (cons (apply f (mapcar 'car xs))
        (apply 'mapcar* f (mapcar 'cdr xs)))))

  ;; setq namespace
  (defmacro setq-ms (namespace &rest lst)
    `(mapcar*
       (lambda (pair)
         (let ((key (car pair))
                (value (car (cdr pair))))
           (set
             (intern (concat (prin1-to-string ',namespace) "-" (prin1-to-string key)))
             (eval value)
             )))
       (seq-partition ',lst 2)
       ))

  (defun ~ (path)
    (concat (getenv "HOME") "/" path))

  ;; todo: take a look at general-describe-keybindings later
  ;; binding wrappers
  (defmacro ms/bind (&rest binds)
    `(general-define-key
       :states '(normal visual)
       :prefix "SPC"
       ,@binds
       ))

  (defmacro ms/bind-mode(keymaps &rest binds)
    `(general-define-key
       :prefix "SPC"
       :states '(visual normal)
       :keymaps ,keymaps
       ,@binds))

  (defmacro ms/bind-leader-mode (mode &rest binds)
    `(general-define-key
       :prefix ","
       :states '(visual normal)
       :keymaps (intern (concat (symbol-name ,mode) "-mode-map"))
       ,@binds))

  ;; this was removed
  ;; cf https://github.com/abo-abo/swiper/pull/1570/files#diff-c7fad2f9905e642928fa92ae655e23d0L4500
  (defun counsel-switch-to-buffer-or-window (buffer-name)
    "Display buffer BUFFER-NAME and select its window.
 This behaves as `switch-to-buffer', except when the buffer is
 already visible; in that case, select the window corresponding to
 the buffer."
    (let ((buffer (get-buffer buffer-name)))
      (if (not buffer)
        (shell buffer-name)
        (let (window-of-buffer-visible)
          (catch 'found
            (walk-windows (lambda (window)
                            (and (equal (window-buffer window) buffer)
                              (throw 'found (setq window-of-buffer-visible window))))))
          (if window-of-buffer-visible
            (select-window window-of-buffer-visible)
            (switch-to-buffer buffer))))))

  (defcommand find-or-open (filepath)
    "Find or open FILEPATH."
    (let
      ((filename (file-name-nondirectory filepath)))
      (if (get-buffer filename)
        (counsel-switch-to-buffer-or-window filename)
        (find-file filepath)
        )))

  ;; wrap passwordstore
  (defun pass (key)
    (ms/shell-exec (concat "pass " key)))

  (defun get-resource (name)
    "Get X resource value, with a fallback value NAME."
    (let ((default (cdr (assoc name ms/xrdb-fallback-values)))
           (result (if (executable-find "xrq")
                     (ms/shell-exec (format "xrq '%s' 2>/dev/null" name))
                     "")))
      (if (string= result "") default result)))

  (defun reload-init()
    "Reload init.el with straight.el."
    (interactive)
    (straight-transaction
      (straight-mark-transaction-as-init)
      (message "Reloading init.el...")
      (load user-init-file nil 'nomessage)
      (message "Reloading init.el... done.")))

  (let ((extend-file (~ "extend.el")))
    (when (file-exists-p extend-file)
      (eval-and-compile (load extend-file))))
  )

(defconfig util
  (use-package pcre2el)

  ;; a macro for when something is not on melpa yet (assumes github)
  (defmacro ms/use-package (name repo &rest config)
    `(progn
       (straight-use-package '(,(make-symbol (symbol-name name)) :host github :repo ,repo))
       ;; assume first arg is :config
       ,@(cdr config)))

  (defun get-string-from-file (filePath)
    "Return filePath's file content."
    (with-temp-buffer
      (insert-file-contents filePath)
      (buffer-string)))

  (defcommand what-line ()
    "Print the current line number (in the buffer) of point."
    (save-restriction
      (widen)
      (save-excursion
        (beginning-of-line)
        (1+ (count-lines 1 (point))))))

  ;; todo: this isn't working with anchors in other frames
  (defun ms/eww-browse-existing-or-new (url)
    "If eww is displayed, use that for URL, else open here."
    (if (get-buffer-window "*eww*" 0)
      (url-retrieve url 'eww-render
        (list url nil (get-buffer "*eww*")))
      (eww url)))

  (defun ms/color-is-light-p (name)
    (let*
      ((rgb (color-name-to-rgb name))
        (red (first rgb))
        (green (second rgb))
        (blue (third rgb))
        ;; cf https://en.wikipedia.org/wiki/YIQ#From_RGB_to_YIQ
        (yiq (+ (* red .299) (* green .587) (* blue .114))))
      (>= yiq 0.5)
      ))

  (defun ms/color-tone (name light dark)
    "tone name a percent based on if light or dark - generally want softer value for dark."
    (if (ms/color-is-light-p name)
      (color-darken-name name light)
      (color-lighten-name name dark)))

  (defun ms/what-face (pos)
    (interactive "d")
    (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
      (if face (message "Face: %s" face) (message "No face at %d" pos))))

  (defcommand what-major-mode ()
    "Reveal current major mode."
    (message "%s" major-mode))

  (defcommand what-minor-modes ()
    (message
      (format "%s"
        (delq nil
          (mapcar
            (lambda (x)
              (let ((car-x (car x)))
                (when (and (symbolp car-x) (symbol-value car-x))
                  x)))
            minor-mode-alist))))
    (ms/look-at-last-message)
    )

  (defun sudo-edit (&optional arg)
    "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
    (interactive "P")
    (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                   (ido-read-file-name "Find file(as root): ")))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

  (defun ms/get-functions()
    (mapcar*
      (lambda(item)
        (s-chomp (s-chop-prefix "(defconfig " (car item))))
      (s-match-strings-all
        "^(defconfig [^ \(\)]+"
        (get-string-from-file (~ ".emacs.d/lisp/theworld.el")))))

  (defun ms/check-for-orphans()
    "Check to see if any defconfigs are missing from init."
    (let ((initfile (get-string-from-file (~ ".emacs.d/init.el"))))
      (mapcar
        (lambda(conf)
          (when (not (s-contains? conf initfile))
            (message (concat "orphaned function! " conf))))
        (ms/get-functions))))

  (defcommand jump-config()
    (ivy-read "config: " (ms/get-functions)
      :action
      (lambda (option)
        (interactive)
        (ms/find-or-open (~ ".emacs.d/lisp/theworld.el"))
        (goto-char (point-min))
        (re-search-forward (concat "defconfig " option))
        (ms/focus-line)
        )))

  (defcommand toggle-bloat()
    "toggle bloat in the current buffer"
    (if (not (bound-and-true-p company-mode))
      (progn
        (company-mode)
        (flycheck-mode)
        (font-lock-mode)
        (git-gutter-mode))
      (progn
        (company-mode -1)
        (flycheck-mode -1)
        (font-lock-mode 0)
        (git-gutter-mode 0))))

  (defun ms/toggle-bloat-global(toggle)
    "toggle global bloat - must be called on it's own"
    (if toggle
      (progn
        (global-company-mode)
        (global-flycheck-mode)
        (global-font-lock-mode)
        (global-git-gutter-mode t))
      (progn
        (global-company-mode -1)
        (global-flycheck-mode -1)
        (global-font-lock-mode 0)
        (global-git-gutter-mode nil))))

  (use-package simpleclip)
  (use-package restart-emacs)

  (defcommand buffercurl ()
    "curl buffer from url grabbed from clipboard"

    (request
      (simpleclip-get-contents)
      :type "GET"
      :parser 'buffer-string
      :success
      (function*
        (lambda (&key data &allow-other-keys)
          (interactive)
          (insert data)))))

  (defun current-line-empty-p ()
    (save-excursion
      (beginning-of-line)
      (looking-at "[[:space:]]*$")))

  (defcommand focus-line (&rest ignore)
    (evil-scroll-line-to-center (ms/what-line)))

  (defun ms/get-last-message()
    (with-current-buffer (get-buffer "*Messages*")
      (goto-char (point-max))
      (previous-line 1)
      (let ((beg (line-beginning-position 1))
             (end (line-beginning-position 2)))
        (buffer-substring beg end))))

  (defun ms/look-at-last-message()
    (interactive)
    (ms/find-or-open (~ ".emacs.d/lisp/scratch.el"))
    (goto-char (point-max))
    (insert "\n")
    (insert (ms/get-last-message))
    (previous-line 1)
    )

  (defun ms/parse-font (font)
    (let* ((parts (s-split "-" font))
            (family (first parts))
            (size (string-to-number (second parts))))
      ;; height is in 1/10th of pt
      `(:family ,family :height ,(* 10 size))))

  ;; (defmacro @ (input) (eval `(backquote ,input)))

  (defun ms/set-faces-variable (faces)
    (dolist (face faces)
      (apply 'set-face-attribute face nil (ms/parse-font (get-resource "st.font_variable")))))

  (defun ms/set-faces-monospace (faces)
    (dolist (face faces)
      (apply 'set-face-attribute face nil (ms/parse-font (get-resource "st.font")))))

  (defcommand set-buffer-face-variable ()
    (setq buffer-face-mode-face (ms/parse-font (get-resource "st.font_variable")))
    (buffer-face-mode t))

  (defcommand set-buffer-face-monospace ()
    (setq buffer-face-mode-face (ms/parse-font (get-resource "st.font")))
    (buffer-face-mode t))

  (ms/bind
    ;; reconsider these, moved from w -> q for query
    "qf" 'ms/what-face
    "qm" 'ms/what-major-mode
    "qi" 'ms/what-minor-modes
    "qq" 'ms/look-at-last-message

    ;; this should maybe be more generic ie mx history when not in shell
    "qh" 'counsel-shell-history

    "fE" 'sudo-edit
    "jc" 'ms/jump-config
    "tb" 'ms/toggle-bloat
    "iu" 'ms/buffercurl
    )
  )

(defconfig sanity
  (setq
   ;; todo: relook at this setting
   auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
   backup-directory-alist `(("." . ,(~ ".emacs.d/backups")))
   coding-system-for-read 'utf-8
   coding-system-for-write 'utf-8
   delete-old-versions -1
   global-auto-revert-mode t
   inhibit-startup-screen t
   initial-scratch-message ""
   ring-bell-function 'ignore
   sentence-end-double-space nil
   vc-follow-symlinks t ;; auto follow symlinks
   vc-make-backup-files t
   version-control t
   network-security-level 'high
   ;; ouch - todo: revisit this
   gc-cons-threshold 10000000
   frame-resize-pixelwise t
   )

  ;; trim gui
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (when (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  ;; cursor
  (show-paren-mode 1)
  (blink-cursor-mode 0)

  ;; custom
  (defconst custom-file (~ ".emacs.d/custom.el"))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))

  (load custom-file 'noerr)

  ;; persistent session:
  ;; note: (desktop-clear) to clean/kill everything.
  (make-directory (~ ".emacs.desktop") t)
  (setq-ms desktop
           restore-eager 5
           auto-save-timeout 30
           path (list (~ ".emacs.desktop"))
           )

  ;; todo: maybe change this, get recent files opened instead (don't care about file states)
  ;; (desktop-save-mode 1)

  (setq browse-url-browser-function 'browse-url-generic)

  ;; Removes *scratch* from buffer after the mode has been set.
  (add-hook 'after-change-major-mode-hook
            (lambda() (if (get-buffer "*scratch*") (kill-buffer "*scratch*"))))

  ;; disable semantic mode, this may bite me lets try it out
  (with-eval-after-load 'semantic
    (add-to-list 'semantic-inhibit-functions (lambda () t))
    )

  ;; set default to be handled by global bloat toggle
  (global-font-lock-mode 0)

  (fset 'yes-or-no-p 'y-or-n-p)

  (defcommand toggle-modeline ()
    (make-local-variable 'ms/modeline)

    (if mode-line-format
        (progn
          (setq ms/modeline mode-line-format)
          (setq mode-line-format nil))
      (setq mode-line-format ms/modeline))
    (redraw-frame))

  ;; don't ask to kill running processes when killing a buffer.
  (setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

  ;; don't popup buffers with output when launching things
  (add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

  ;; save recent files
  (recentf-mode 1)
  (setq recentf-max-menu-items 300)
  ;; (run-at-time nil (* 5 60) 'recentf-save-list)

  (setq whitespace-line-column 120)

  (defcommand insert-filename ()
    (insert (f-filename (buffer-file-name))))

  (defcommand insert-filepath ()
    (insert (buffer-file-name)))

  ;; a report toggle command for debuggong on keybind
  (defcommand toggle-report ()
    (if (profiler-running-p)
        (progn
          (profiler-report)
          (profiler-stop))
      (profiler-cpu-start)))

  (ms/bind
   "js" (lambda() (interactive) (ms/find-or-open (~ ".emacs.d/lisp/scratch.el")))
   "jS" (lambda() (interactive) (ms/find-or-open (~ ".emacs.d/lisp/scratch.txt")))
   "jm" (lambda() (interactive) (counsel-switch-to-buffer-or-window  "*Messages*"))

   "t" '(:ignore t :which-key "Toggle")
   "tw" 'whitespace-mode
   "tn" 'linum-mode
   "tl" 'toggle-truncate-lines
   "ts" 'ms/style
   "ti" 'reload-init
   "tm" 'ms/toggle-modeline
   "tp" 'ms/toggle-report

   "i" '(:ignore t :which-key "Insert")
   "ic" 'insert-char
   "if" 'ms/insert-filename
   "ip" 'ms/insert-filepath
   )
  )

(defconfig evil
  (use-package evil
    ;; for evil-collection
    :init (setq evil-want-integration nil)
    :config (evil-mode 1)
    )

  (use-package evil-collection :config (evil-collection-init))

  (defun ms/zz-scroll (&rest optional)
    (let* ((scrollcount (/ (window-total-size) 7))
           (halfheight (/ (window-total-size) 2))
           (scrollcheck (- halfheight scrollcount)))
      (if (> (line-number-at-pos) scrollcheck)
          (evil-scroll-line-down scrollcount)
        )))

  (add-function :after (symbol-function 'evil-scroll-line-to-center) #'ms/zz-scroll)

  (general-evil-setup t)

  ;; defaults to fd/spacemacs-like config
  (use-package evil-escape :config (evil-escape-mode))
  (use-package evil-lion :config (evil-lion-mode))
  (use-package evil-commentary :config (evil-commentary-mode))
  (use-package evil-anzu :config (setq anzu-cons-mode-line-p nil)) ;; displays current match and total matches.
  (use-package evil-matchit :config (global-evil-matchit-mode 1))
  (use-package evil-numbers
    :config
    (general-nmap (kbd "C-c +") 'evil-numbers/inc-at-pt)
    (general-nmap (kbd "C-c -") 'evil-numbers/dec-at-pt)
    )

  (use-package evil-fringe-mark
    :config
    (setq evil-fringe-mark-show-special nil)
    (global-evil-fringe-mark-mode t)
    )

  (use-package evil-goggles
    :config
    (setq evil-goggles-duration 0.100)
    (setq evil-goggles-pulse t)
    ;; fun visual vim mode
    (evil-goggles-mode 0)
    )

  (use-package evil-surround :config (global-evil-surround-mode 1))
  (use-package evil-embrace
    :config
    (general-define-key
     :states 'normal
     "c" (general-key-dispatch 'evil-change "s" #'embrace-change)
     "d" (general-key-dispatch 'evil-delete "s" #'embrace-delete))

    (general-define-key
     :states 'visual
     ;; `evil-change' is not bound in `evil-visual-state-map' by default but
     ;; inherited from `evil-normal-state-map'
     ;; if you don't want "c" to be affected in visual state, you should add this
     "c" #'evil-change
     "d" #'evil-delete
     "s" #'embrace-add
     )

    (evil-embrace-enable-evil-surround-integration))

  (use-package evil-snipe
    :config
    (setq evil-snipe-repeat-scope 'whole-visible)
    (setq evil-snipe-spillover-scope 'whole-visible)
    (evil-snipe-override-mode +1)
    (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

  (use-package evil-exchange
    :config
    (evil-exchange-cx-install))

  ;; Overload shifts so that they don't lose the selection
  (define-key evil-visual-state-map (kbd ">") 'djoyner/evil-shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") 'djoyner/evil-shift-left-visual)
  (define-key evil-visual-state-map [tab] 'djoyner/evil-shift-right-visual)
  (define-key evil-visual-state-map [S-tab] 'djoyner/evil-shift-left-visual)

  (defun djoyner/evil-shift-left-visual ()
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun djoyner/evil-shift-right-visual ()
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  ;; todo: get this to hook
  ;; think it depends on gnu archive updating correctly.
  (use-package evil-org
    :commands evil-org-mode
    :after org
    :init (add-hook 'org-mode-hook 'evil-org-mode)
    :config
    (add-hook 'evil-org-mode-hook
              (lambda ()
                (evil-org-set-key-theme
                 '(
                   textobjects
                   insert
                   navigation
                   additional
                   shift
                   todo
                   heading
                   )))))

  ;; persist marks
  (add-to-list 'desktop-locals-to-save 'evil-markers-alist)

  ;; match qutebrowser fwd back
  (general-nmap
   "H" 'previous-buffer
   "L" 'next-buffer)

  (defcommand should-skip (buffername)
    (or
     ;; (member buffername '("scratch.el"))
     (s-starts-with? "*" buffername)
     (s-starts-with? "magit" buffername))
    )

  (defcommand maybe-next ()
    (when (ms/should-skip (buffer-name))
      (let ((temp (window-next-buffers)))
        (next-buffer)
        (set-window-next-buffers nil temp)
        )))

  (defcommand maybe-prev ()
    (when (ms/should-skip (buffer-name))
      (let ((temp (window-prev-buffers)))
        (previous-buffer)
        (set-window-prev-buffers nil temp)
        )))

  (advice-add #'next-buffer :after #'ms/maybe-next)
  (advice-add #'previous-buffer :after #'ms/maybe-prev)

  (general-nmap
   "]s" 'flyspell-goto-next-error
   "[b" 'evil-prev-buffer
   "]b" 'evil-next-buffer
   )

  (use-package avy
    :config

    (setq avy-all-windows 'all-frames)
    (setq avy-timeout-seconds 0.2)

    (general-mmap
     "z" 'avy-goto-char-timer)

    (general-nmap
     "s" 'avy-goto-char-timer
     "zz" 'evil-scroll-line-to-center
     )))

(defconfig flycheck
  (use-package flycheck
    :config
    ;; cf http://www.flycheck.org/en/latest/user/syntax-checks.html#check-automatically
    (setq-ms flycheck
             check-syntax-automatically '(save mode-enabled idle-change new-line)
             idle-change-delay 1
             )

    ;; (flycheck) disable jshint since we prefer eslint checking
    (setq-default
     flycheck-disabled-checkers
     (append flycheck-disabled-checkers
             '(javascript-jshint)))

    ;; use eslint with web-mode for jsx files
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    )

  ;; (ms/bind
  ;;   "e" '(:ignore t :which-key "Errors")
  ;;   "en" 'flycheck-next-error
  ;;   "ep" 'flycheck-previous-error
  ;;   )

  (general-nmap
   "]e" 'flycheck-next-error
   "[e" 'flycheck-previous-error
   ))

(defconfig treemacs
  (use-package treemacs)
  (use-package treemacs-evil)
  (use-package treemacs-projectile)
  )

(defconfig company
  (use-package company
    :custom
    (company-idle-delay 1 0)
    (company-selection-wrap-around t)
    (company-tooltip-align-annotations t)
    (company-dabbrev-downcase nil)
    (company-dabbrev-ignore-case t)
    (company-tooltip-align-annotations t)
    (company-tooltip-margin 2)
    (company-tooltip-align-annotations t)
    :config

    ;; TODO: investigate tab handling like VS completely
    (define-key company-active-map [tab] 'company-complete)
    )

  (use-package company-quickhelp
    :init
    (company-quickhelp-mode 1)
    (setq company-quickhelp-delay 0.3)
    )
  )

(defconfig editing
  (use-package editorconfig :config (editorconfig-mode 1))
  (setq tab-width 4)

  (use-package aggressive-indent
    :config
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
    )

  (use-package smartparens
    :init
    :config
    (add-to-list 'sp-ignore-modes-list 'circe-channel-mode)
    (smartparens-global-mode)
    )

  ;; from https://github.com/syl20bnr/spacemacs/blob/bd7ef98e4c35fd87538dd2a81356cc83f5fd02f3/layers/%2Bdistributions/spacemacs-bootstrap/config.el
  ;; GPLv3
  (defvar spacemacs--indent-variable-alist
    ;; Note that derived modes must come before their sources
    '(((awk-mode c-mode c++-mode java-mode groovy-mode
                 idl-mode java-mode objc-mode pike-mode) . c-basic-offset) (python-mode . python-indent-offset)
                 (cmake-mode . cmake-tab-width)
                 (coffee-mode . coffee-tab-width)
                 (cperl-mode . cperl-indent-level)
                 (css-mode . css-indent-offset)
                 (elixir-mode . elixir-smie-indent-basic)
                 ((emacs-lisp-mode lisp-mode) . lisp-indent-offset)
                 (enh-ruby-mode . enh-ruby-indent-level)
                 (erlang-mode . erlang-indent-level)
                 (js2-mode . js2-basic-offset)
                 (js3-mode . js3-indent-level)
                 ((js-mode json-mode) . js-indent-level)
                 (latex-mode . (LaTeX-indent-level tex-indent-basic))
                 (livescript-mode . livescript-tab-width)
                 (mustache-mode . mustache-basic-offset)
                 (nxml-mode . nxml-child-indent)
                 (perl-mode . perl-indent-level)
                 (puppet-mode . puppet-indent-level)
                 (ruby-mode . ruby-indent-level)
                 (rust-mode . rust-indent-offset)
                 (scala-mode . scala-indent:step)
                 (sgml-mode . sgml-basic-offset)
                 (sh-mode . sh-basic-offset)
                 (typescript-mode . typescript-indent-level)
                 (web-mode . web-mode-markup-indent-offset)
                 (yaml-mode . yaml-indent-offset))
    "An alist where each key is either a symbol corresponding
      to a major mode, a list of such symbols, or the symbol t,
      acting as default. The values are either integers, symbols
      or lists of these.")

  (defun spacemacs//set-evil-shift-width ()
    "Set the value of `evil-shift-width' based on the indentation settings of the
      current major mode."
    (let ((shift-width
           (catch 'break
             (dolist (test spacemacs--indent-variable-alist)
               (let ((mode (car test))
                     (val (cdr test)))
                 (when (or (and (symbolp mode) (derived-mode-p mode))
                           (and (listp mode) (apply 'derived-mode-p mode))
                           (eq 't mode))
                   (when (not (listp val))
                     (setq val (list val)))
                   (dolist (v val)
                     (cond
                      ((integerp v) (throw 'break v))
                      ((and (symbolp v) (boundp v))
                       (throw 'break (symbol-value v))))))))
             (throw 'break (default-value 'evil-shift-width)))))
      (when (and (integerp shift-width)
                 (< 0 shift-width))
        (setq-local evil-shift-width shift-width))))

  (add-hook 'after-change-major-mode-hook 'spacemacs//set-evil-shift-width 'append)

  ;; only trim whitespace on lines you edit
  (use-package ws-butler :config (ws-butler-global-mode))

  ;; to always trim it all
  ;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; todo: call yas-describe-tables sometime
  (use-package yasnippet-snippets)
  (use-package yasnippet
    :config
    (yas-global-mode 1))

  (add-hook 'sh-mode-hook
            (lambda () (sh-electric-here-document-mode -1))))

(defconfig-base style
  (interactive)
  ;; todo: an xresources theme that doesn't suck/covers extensions that base16 covers
  (use-package base16-theme)
  ;; https://github.com/waymondo/apropospriate-theme
  ;;(use-package ujelly-theme)

  (let ((theme (intern (get-resource "Emacs.theme"))))
    (when (boundp 'ms/loaded-theme)
      (disable-theme ms/loaded-theme))
    (load-theme theme)
    (setq ms/loaded-theme theme))

  (set-face-attribute 'fringe nil :background nil)
  (set-face-background 'font-lock-comment-face nil)

  ;; current frames
  (mapc (lambda(frame)
          (interactive)
          (set-frame-parameter frame 'internal-border-width
                               (string-to-number (get-resource "st.borderpx")))
          (redraw-frame frame))
        (frame-list))

  ;; future frames
  (when (alist-get 'internal-border-width default-frame-alist)
    (setq default-frame-alist (assq-delete-all 'internal-border-width default-frame-alist)))
  (add-to-list 'default-frame-alist
               `(internal-border-width . ,(string-to-number (get-resource "st.borderpx"))))

  ;; this is only viable if can get it on internal window edges only (not right now)
  ;; http://emacsredux.com/blog/2015/01/18/customizing-the-fringes/
  ;; (fringe-mode (string-to-number (get-resource "st.borderpx")))

  ;; sync w/ term background
  (set-background-color (get-resource "*.background"))

  ;; assume softer vertical border by matching comment face
  (set-face-attribute 'vertical-border
                      nil :foreground (face-attribute 'font-lock-comment-face :foreground))

  ;; this doesn't persist across new frames even though the docs say it should
  (set-face-attribute 'fringe nil :background nil)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (set-face-attribute 'fringe nil :background nil)))

  ;; set font on current and future
  (set-face-attribute 'default nil :font (get-resource "st.font"))
  (set-frame-font (get-resource "st.font") nil t)

  (setq ms/colored-whitespace? nil)
  (defun color-whitespace-mode(&rest maybe)
    (when (not ms/colored-whitespace?)
      (set-face-attribute 'whitespace-space nil :background nil)
      (set-face-attribute 'whitespace-tab nil :background nil)
      (set-face-attribute 'whitespace-newline nil
                          :foreground (face-attribute 'whitespace-space :foreground))
      (setq ms/colored-whitespace? t)
      )
    )

  (advice-add 'whitespace-mode :after #'color-whitespace-mode )
  ;; (advice-add 'whitespace-mode :after #'color-whitespace-mode )

  (use-package hl-todo
    :config
    (let* ((comment-color (face-attribute 'font-lock-comment-face :foreground))
           (highlight-color (ms/color-tone comment-color 30 30)))

      (setq hl-todo-keyword-faces
            `(("TODO" . ,highlight-color)
              ("todo" . ,highlight-color)
              ("NOTE" . ,highlight-color)
              ("note" . ,highlight-color)
              )))

    (global-hl-todo-mode)
    )

  ;; NO BOLD
  ;; (set-face-bold-p doesn't cover everything, some fonts use slant and underline as bold...)
  (mapc (lambda (face)
          (set-face-attribute face nil
                              :weight 'normal
                              :slant 'normal
                              :underline nil
                              ;;:inherit nil
                              ))
        (face-list))

  (use-package dimmer
    :config (setq dimmer-fraction 0.5)
    (dimmer-mode 0))

  ;; gross colors, but need something so we have a signifier in unique match case
  ;; todo: maybe fix gross colors
  ;; (set-face-attribute 'avy-lead-face nil :background (ms/color-tone (face-attribute 'default :background) 30 30))
  ;; (set-face-attribute 'avy-lead-face nil :foreground (ms/color-tone (face-attribute 'default :foreground) 30 30))

  (ms/spaceline)
  )

(defconfig spaceline
  (defun spacemacs/compute-powerline-height ()
    "Return an adjusted powerline height."
    (let ((scale (if (and (boundp 'powerline-scale) powerline-scale)
                     powerline-scale 1)))
      (truncate (* scale (frame-char-height)))))

  (use-package spaceline
    :config
    (require 'spaceline-config)
    (setq-ms powerline
             scale (string-to-number (get-resource "Emacs.powerlinescale"))
             height (spacemacs/compute-powerline-height)
             default-separator (get-resource "Emacs.powerline")
             )

    ;; (set-face-attribute 'spaceline-highlight-face nil :background (face-attribute 'spaceline-evil-normal :background))

    ;; todo: make a circe segment

    ;; note to self: abandon this, look at switch-to-next, prev-buffer source, grab that and replace return
    (defun ms/next-buffer-name ()
      (-first (lambda (bufname) (not (ms/should-skip bufname)))
              (-map 'buffer-name (window-next-buffers))))

    (defun ms/prev-buffer-name ()
      (-first (lambda (bufname) (not (ms/should-skip bufname)))
              (-map 'buffer-name
                    (-map 'first (window-prev-buffers)))))

    (spaceline-define-segment next-buffers
                              "Docstring"
                              ;; A single form whose value is the value of the segment.
                              ;; It may return a string, an image or a list of such.
                              (when t
                                (concat
                                 (s-left 8 (ms/prev-buffer-name))
                                 " - "
                                 (s-left 8 (ms/next-buffer-name)))
                                )
                              :enabled t
                              )

    ;; this is needed to set default for new buffers?
    (spaceline-spacemacs-theme)

    (spaceline-compile 'main
                       '(
                         anzu
                         (remote-host projectile-root ">>" buffer-id buffer-modified)
                         (flycheck-error flycheck-warning)
                         process
                         )
                       '(
                         ;; maybe
                         (version-control :when active)
                         (next-buffers :when active)
                         (org-clock :when active)
                         (org-pomodoro :when active)
                         info-nodes
                         ((line-column buffer-position)
                          :separator " |" )
                         (battery :when active)
                         ))

    (setq mode-line-format '("%e" (:eval (spaceline-ml-main))))

    ;; set the modeline for all existing buffers
    ;; todo: make this unset modeline on not matching spawn
    (defcommand refresh-all-modeline ()
      (dolist (buf (buffer-list))
        (when (not (s-starts-with-p "*spawn-shell" (buffer-name buf)))
          (with-current-buffer buf
            (setq mode-line-format '("%e" (:eval (spaceline-ml-main))))))))

    ;; todo: somehow this has no effect in init
    (ms/refresh-all-modeline)
    (ms/bind "tM" 'ms/refresh-all-modeline)
    ))

(defconfig interface
  ;; todo: into occur/search buffer solution for better finding when don't know what we're looking for
  (use-package ivy
    :custom
    (ivy-initial-inputs-alist nil)
    (ivy-extra-directories nil)
    (ivy-re-builders-alist '((t . ivy--regex-plus)))
    (ivy-use-virtual-buffers t)
    (ivy-virtual-abbreviate 'abbreviate)
    (ivy-format-function #'ivy-format-function-arrow)
    (ivy-display-style 'fancy)
    (ivy-use-selectable-prompt t)
    :config

    (ivy-mode 1)

    (use-package prescient
      :config
      (prescient-persist-mode)
      (use-package ivy-prescient
        :config
        (ivy-prescient-mode)))

    (use-package ivy-rich
      :custom
      (ivy-virtual-abbreviate 'full)
      (ivy-rich-switch-buffer-align-virtual-buffer t)
      (ivy-rich-abbreviate-paths t)
      (ivy-rich-path-style 'abbrev)
      :config
      (ivy-set-display-transformer 'ivy-switch-buffer
        'ivy-rich-switch-buffer-transformer)))

  ;; counsel
  (use-package counsel
    :custom
    (counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
    (counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s .")
    :bind
    (([remap execute-extended-command] . counsel-M-x)
      ([remap find-file] . counsel-find-file))
    :config
    (use-package rg)
    (use-package swiper
      :custom
      (swiper-include-line-number-in-search t)
      :bind (("C-s" . counsel-grep-or-swiper))))

  (use-package ranger
    :init (setq ranger-override-dired t)
    :config
    (setq-ms ranger
      show-literal nil
      show-hidden t
      cleanup-eagerly t
      )

    ;; call with eg 'dired-mode
    (defcommand kill-buffers-by-mode (mode)
      (mapc (lambda (buffer)
              (when (eq mode (buffer-local-value 'major-mode buffer))
                (kill-buffer buffer)))
        (buffer-list)))

    (defcommand kill-ranger-buffers ()
      (ms/kill-buffers-by-mode 'ranger-mode))

    (advice-add #'ranger-close :after #'ms/kill-ranger-buffers)

    (defcommand deer-with-last-shell ()
      (let ((current-buffer (buffer-name (current-buffer))))
        (if (or (s-match "\*spawn-shell.*" current-buffer)
              (s-match "\*shell-[1-9]\*" current-buffer))
          (setq ms/last-shell current-buffer)
          (setq ms/last-shell shell-pop-last-shell-buffer-name)))
      (deer))

    (ms/bind "d" 'ms/deer-with-last-shell)

    (defcommand open () (async-shell-command (format "xdg-open \"%s\"" (dired-get-file-for-visit))))
    (define-key ranger-normal-mode-map (kbd "RET") 'ms/open)
    )

  (defun my-resize-margins ()
    (let ((margin-size (if ms/center (/ (- (frame-width) 120) 2) 0)))
      (set-window-margins nil margin-size margin-size)))

  (defcommand toggle-margin ()
    (if (not (bound-and-true-p ms/center))
      (setq ms/center nil))

    (if ms/center
      (remove-hook 'window-configuration-change-hook #'my-resize-margins)
      (add-hook 'window-configuration-change-hook #'my-resize-margins))

    (setq ms/center (not ms/center))
    (my-resize-margins))

  (defcommand kill-current-buffer()
    (kill-buffer nil))

  (defcommand follow-mode ()
    (follow-mode)
    (delete-other-windows)
    (evil-window-vsplit))

  (ms/bind
    "/"   'counsel-rg
    "TAB" '(switch-to-other-buffer :which-key "prev buffer")
    "SPC" 'counsel-M-x

    ;; windows
    "w" '(:ignore t :which-key "Windows")
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right
    "wd" 'evil-window-delete
    "ww" 'other-window
    "wf" 'ms/follow-mode
    "wc" 'ms/toggle-margin

    "wm" 'delete-other-windows ;; window-max
    "wo" 'other-frame

    ;; Applications
    "a" '(:ignore t :which-key "Applications")

    "b" '(:ignore t :which-key "Buffers")
    "bd" 'ms/kill-current-buffer

    "j" '(:ignore t :which-key "Jump")
    "jd" 'counsel-imenu
    )

  (use-package alert
    :config (setq alert-default-style 'libnotify))

  (use-package which-key
    :config
    (setq-ms which-key
      idle-delay 1.5
      side-window-max-width 0.33
      sort-order 'which-key-key-order-alpha
      )
    (which-key-setup-side-window-right-bottom)
    (which-key-mode)
    )



  (use-package ace-jump-buffer
    :config
    (defun dynamic-ajb-height()
      (setq ajb-max-window-height (/ (window-total-size) 2)))

    (dynamic-ajb-height)
    (add-hook 'window-configuration-change-hook 'dynamic-ajb-height)
    (setq ajb-sort-function 'bs--sort-by-recentf)

    ;; todo: kill buffers by regexp command
    (ms/bind
      "bs" 'ace-jump-buffer
      "bm" 'ace-jump-same-mode-buffers
      )
    )

  (defcommand kill-other-buffers ()
    "Kill all other buffers."
    (mapc 'kill-buffer
      (delq (current-buffer)
        (remove-if-not 'buffer-file-name (buffer-list)))))

  (ms/bind
    "bb" 'counsel-ibuffer
    "bK" 'ms/kill-other-buffers
    "bk" 'kill-matching-buffers
    )
  )

(defconfig projectile
  (use-package projectile
    :custom
    (projectile-completion-system 'ivy)
    (projectile-require-project-root nil)
    (projectile-switch-project-action 'dired-jump)
    (projectile-mode-line
      '(:eval
         (format " Pr[%s]"
           (projectile-project-name))))
    (projectile-enable-caching t)
    (projectile-keymap-prefix (kbd "C-c p"))
    :config
    (projectile-mode))

  ;; still assuming git command, maybe lean on projectile for file listing
  (defun get-project-files (project-root)
    (let* ((default-directory (expand-file-name project-root))
            (project-files-relative (s-split "\n" (shell-command-to-string counsel-git-cmd) t)))
      (mapcar (lambda (file) (concat default-directory file)) project-files-relative)))

  (defcommand jump-file ()
    (let* ( ;; bail out if we're not in a project
            (project-root (condition-case nil (projectile-project-root) (error nil)))
            (project-files
              (if project-root
                (get-project-files project-root)
                '()))

            ;; mapc keeps the buffers for some reason, even though buffer-file-name
            ;; is doc'd to return nil and does in the single case
            (open-buffers (s-split "\n" (mapconcat 'buffer-file-name (buffer-list) "\n") t))
            (recent-files recentf-list)

            ;; todo: test getting projects of open buffers listing
            ;; (misc-project-files (car (mapcar 'get-project-files ms/projectile-roots)))
            )

      (ivy-read "file: "
        (-distinct (append project-files open-buffers recent-files))
        :action #'find-file)))

  (ms/bind "jf" 'ms/jump-file )
  )

(defconfig git
  (use-package magit
    :config
    (setq-ms magit
             save-repository-buffers 'dontask
             repository-directories (list (~ "git"))
             ))

  (macroexpand-1
   '(ms/use-package magit-todos "alphapapa/magit-todos"
                    :config
                    (setq magit-todos-nice ms/enable-linux-p)
                    (evil-define-key nil magit-todos-section-map "j" nil)
                    (magit-todos-mode))
   )

  (use-package magit-svn :config
    (add-hook 'magit-mode-hook 'magit-svn-mode))

  (use-package evil-magit
    :config
    (evil-define-key
      evil-magit-state magit-mode-map "?" 'evil-search-backward))

  (use-package git-gutter-fringe
    :config
    (setq git-gutter-fr:side 'right-fringe)
    ;; fails when too many buffers open on windows
    (if ms/enable-linux-p (global-git-gutter-mode t))
    )

  (defhydra git-smerge-menu ()
    "
      movement^^^^               merge action^^           other
      ---------------------^^^^  -------------------^^    -----------
      [_n_]^^    next hunk       [_b_] keep base          [_u_] undo
      [_N_/_p_]  prev hunk       [_m_] keep mine          [_r_] refine
      [_j_/_k_]  move up/down    [_a_] keep all           [_q_] quit
      ^^^^                       [_o_] keep other
      ^^^^                       [_c_] keep current
      ^^^^                       [_C_] combine with next"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("N" smerge-prev)
    ("j" evil-next-line)
    ("k" evil-previous-line)
    ("a" smerge-keep-all)
    ("b" smerge-keep-base)
    ("m" smerge-keep-mine)
    ("o" smerge-keep-other)
    ("c" smerge-keep-current)
    ("C" smerge-combine-with-next)
    ("r" smerge-refine)
    ("u" undo-tree-undo)
    ("q" nil :exit t))

  (defcommand git-status()
    (magit-status)
    (if (> (frame-pixel-height) (frame-pixel-width))
        (delete-other-windows)))


  (use-package vdiff
    :config
    (evil-define-key 'normal vdiff-mode-map "," vdiff-mode-prefix-map)

    (evil-define-minor-mode-key 'normal 'vdiff-mode "]c" 'vdiff-next-hunk)
    (evil-define-minor-mode-key 'normal 'vdiff-mode "[c" 'vdiff-previous-hunk)
    (evil-define-minor-mode-key 'normal 'vdiff-mode "zc" 'vdiff-close-fold)
    (evil-define-minor-mode-key 'normal 'vdiff-mode "zM" 'vdiff-close-all-folds)
    (evil-define-minor-mode-key 'normal 'vdiff-mode "zo" 'vdiff-open-fold)
    (evil-define-minor-mode-key 'normal 'vdiff-mode "zR" 'vdiff-open-all-folds)
    (evil-define-minor-mode-key 'motion 'vdiff-mode "go" 'vdiff-receive-changes)
    (evil-define-minor-mode-key 'motion 'vdiff-mode "gp" 'vdiff-send-changes)
    )

  (general-nmap
   "]g" 'git-gutter:next-hunk
   "[g" 'git-gutter:previous-hunk
   )

  ;; alias:
  (defcommand magit-history () (magit-log-buffer-file))

  (ms/bind
   "g" '(:ignore t :which-key "git")
   "gb" 'magit-blame
   "gl" 'magit-log-current
   "gm" 'git-smerge-menu/body
   "gd" 'vdiff-mode ; ,h for a hydra!
   "gs" 'ms/git-status
   "gh" 'ms/magit-history
   )
  )

(defconfig jump
  (use-package smart-jump
    :config
    (setq dumb-jump-selector 'ivy)
    (setq dumb-jump-force-searcher 'rg)
    (smart-jump-setup-default-registers)
    (ms/bind
     "j" '(:ignore t :which-key "Jump")
     "jj" 'smart-jump-go
     "jb" 'smart-jump-back
     )

    (advice-add #'smart-jump-go :after #'ms/focus-line)
    ))

(defconfig restclient
  (use-package restclient
    :config
    (ms/bind-leader-mode
     'restclient
     "ei" 'restclient-http-send-current-stay-in-window
     )
    )

  (use-package company-restclient)
  )

(defconfig ledger
  (use-package ledger-mode)
  (use-package flycheck-ledger)
  (use-package evil-ledger
    :config
    (add-hook 'ledger-mode-hook #'evil-ledger-mode)
    )
  )

(defconfig lsp
  (use-package lsp-ui)
  (use-package lsp-javascript-flow)
  (use-package lsp-javascript-typescript)

  (use-package cquery)


  (defun my-company-transformer (candidates)
    (let ((completion-ignore-case t))
      (all-completions (company-grab-symbol) candidates)))

  (defun my-js-hook nil
    (make-local-variable 'company-transformers)
    (push 'my-company-transformer company-transformers))

  (add-hook 'web-mode-hook 'my-js-hook)
  (add-hook 'web-mode-hook #'lsp-javascript-typescript-enable)

  (remove-hook 'web-mode-hook #'lsp-javascript-flow-enable)
  )

(defconfig emoji
  (use-package emojify
    :init (setq emojify-emoji-styles '(unicode github))
    :config
    (global-emojify-mode)
    (ms/bind "ie" 'emojify-insert-emoji)
    ))

(defconfig deadgrep
  (ms/use-package deadgrep "Wilfred/deadgrep"
                  :config
                  (ms/bind "ss" 'deadgrep)
                  (setq deadgrep-max-line-length 180)
                  (general-nmap deadgrep-mode-map
                                "RET" 'deadgrep-visit-result-other-window)
                  ))

(defconfig elisp
  (setq lisp-indent-function 'common-lisp-indent-function)

  (use-package helpful
    :config
    (global-set-key (kbd "C-h f") #'helpful-callable)
    (global-set-key (kbd "C-h v") #'helpful-variable)
    (global-set-key (kbd "C-h k") #'helpful-key)
    (global-set-key (kbd "C-h i") #'counsel-info-lookup-symbol)
    )

  (use-package eros
    :config
    (setq eros-eval-result-duration 20)
    (eros-mode 1)
    (ms/bind-leader-mode
     'emacs-lisp
     "er" 'eval-region
     "ei" 'eros-eval-last-sexp
     "ee" 'eros-eval-defun
     )
    ))

(provide 'theworld)

;;; theworld.el ends here
