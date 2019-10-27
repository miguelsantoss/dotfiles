;;; doom-abyss-theme.el --- inspired by vscode abyss
(require 'doom-themes)

;;
(defgroup doom-abyss-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-abyss-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-abyss-theme
  :type 'boolean)

(defcustom doom-abyss-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-abyss-theme
  :type 'boolean)

(defcustom doom-abyss-comment-bg doom-abyss-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-abyss-theme
  :type 'boolean)

(defcustom doom-abyss-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-abyss-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-abyss
  "A dark theme inspired by vscode abyss"

  ;; name        default   256       16
  ((bg         '("#000c18" nil       nil            ))
   (bg-alt     '("#082050" nil       nil            ))
   (base0      '("#060621" "black"   "black"        ))
   (base1      '("#0d0e27" "#1e1e1e" "brightblack"  ))
   (base2      '("#161934" "#2e2e2e" "brightblack"  ))
   (base3      '("#1e2340" "#262626" "brightblack"  ))
   (base4      '("#2e3656" "#3f3f3f" "brightblack"  ))
   (base5      '("#4c5a7f" "#525252" "brightblack"  ))
   (base6      '("#5e7199" "#6b6b6b" "brightblack"  ))
   (base7      '("#7591b2" "#979797" "brightblack"  ))
   (base8      '("#929bb6" "#dfdfdf" "white"        ))
   (fg-alt     '("#dfdfdf" "#bfbfbf" "brightwhite"  ))
   (fg         '("#bbbbbb" "#2d2d2d" "white"        ))

   (grey       base4)
   (red        '("#dc322f" "#ff6655" "red"          ))
   (orange     '("#ddbb88" "#dd8844" "brightred"    ))
   (green      '("#22aa44" "#99bb66" "green"        ))
   (teal       '("#31958a" "#44b9b1" "brightgreen"  ))
   (yellow     '("#ffeebb" "#ECBE7B" "yellow"       ))
   (blue       '("#6688cc" "#51afef" "brightblue"   ))
   (dark-blue  '("#384887" "#2257A0" "blue"         ))
   (magenta    '("#9966b8" "#c678dd" "magenta"      ))
   (violet     '("#f280d0" "#a9a1e1" "brightmagenta"))
   (cyan       '("#225588" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#2277ff" "#5699AF" "cyan"   ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-lighten base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       dark-blue)
   (doc-comments   (doom-lighten dark-blue 0.25))
   (constants      violet)
   (functions      orange)
   (keywords       cyan)
   (methods        yellow)
   (operators      cyan)
   (type           magenta)
   (strings        green)
   (variables      dark-cyan)
   (numbers        violet)
   (region         "#08286b")
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-abyss-brighter-modeline)
   (-modeline-pad
    (when doom-abyss-padded-modeline
      (if (integerp doom-abyss-padded-modeline) doom-abyss-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-abyss-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground yellow)
   (css-property             :foreground magenta)
   (css-selector             :foreground orange)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-url-face    :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override)   :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)

   ;; outline (affects org-mode)
   ((outline-1 &override) :foreground blue)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground (doom-darken blue 0.2))
   ((outline-5 &override) :foreground (doom-darken green 0.2))
   ((outline-6 &override) :foreground (doom-darken teal 0.2))
   ((outline-7 &override) :foreground (doom-darken blue 0.4))
   ((outline-8 &override) :foreground (doom-darken green 0.4))

   ;; org-mode
   (org-hide :foreground hidden)
   (org-block :background bg)
   (org-block-begin-line :background bg :foreground comments)
   (solaire-org-hide-face :foreground hidden)

   ;; slime
   (slime-early-deprecation-warning-face :strike-through warning)
   (slime-late-deprecation-warning-face :strike-through orange)
   (slime-final-deprecation-warning-face :strike-through red)
   (slime-style-warning-face :underline warning)
   (slime-warning-face :underline orange)
   (slime-error-face :underline error)
   (slime-note-face :underline (doom-lighten yellow 0.1))
   (slime-repl-inputed-output-face :foreground red)
   (sldb-restartable-frame-line-face :foreground green))


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-abyss-theme.el ends here
