;;; doom-redprint-theme.el --- dark variant of Redprint -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: March 8, 2018 (c503ebdacac1)
;; Author: fuxialexander <https://github.com/fuxialexander>
;; Maintainer:
;; Source: https://www.redprinttheme.com
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-redprint-theme nil
  "Options for the `doom-redprint' theme."
  :group 'doom-themes)

(defcustom doom-redprint-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-redprint-theme
  :type 'boolean)

(defcustom doom-redprint-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-redprint-theme
  :type 'boolean)

(defcustom doom-redprint-comment-bg doom-redprint-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-redprint-theme
  :type 'boolean)

(defcustom doom-redprint-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-redprint-theme
  :type '(choice integer boolean))

(eval-and-compile
  (defcustom doom-redprint-region-highlight t
    "Determines the selection highlight style. Can be 'frost, 'snowstorm or t
(default)."
    :group 'doom-redprint-theme
    :type 'symbol))


;;
;;; Theme definition

(def-doom-theme doom-redprint
    "A dark theme inspired by Redprint."
  :family 'doom-redprint
  :background-mode 'dark

  ;; name        default   256       16
  ((bg         '("#C4C1C0" nil       nil            ))
   (bg-alt     '("#A9A6A5" nil       nil            ))
   (base0      '("#160F0C" "#160F0C"   "black"        ))
   (base1      '("#A9A6A5" "#A9A6A5" "brightblack"  ))
   (base2      '("#A9A6A5" "#A9A6A5" "brightblack"  ))
   (base3      '("#160F0C" "#160F0C" "brightblack"  ))
   (base4      '("#160F0C" "#160F0C" "brightblack"  ))
   (base5      '("#160F0C" "#160F0C" "brightblack"  ))
   (base6      '("#5C5348" "#5C5348" "brightblack"  ))
   (base7      '("#5C5348" "#5C5348" "brightblack"  ))
   (base8      '("#1F1C18" "#1F1C18" "white"        ))
   (fg         '("#5C5348" "#5C5348" "white"        ))
   (fg-alt     '("#1F1C18" "#1F1C18" "brightwhite"  ))
   (grey       base4)
   (red        '("#9A8B79" "#9A8B79" "red"          ))
   (orange     '("#CDB9A1" "#CDB9A1" "brightred"    ))
   (green      '("#996862" "#996862" "green"        ))
   (teal       '("#18110D" "#18110D" "brightgreen"  ))
   (yellow     '("#6D5C48" "#6D5C48" "yellow"       ))
   (blue       '("#6E3E35" "#6E3E35" "brightblue"   ))
   (dark-blue  '("#935247" "#935247" "blue"         ))
   (magenta    '("#403E33" "#403E33" "magenta"      ))
   (violet     '("#555244" "#555244" "brightmagenta"))
   (cyan       '("#201611" "#201611" "brightcyan"   ))
   (dark-cyan  '("#18110D" "#18110D" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.2))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-redprint-brighter-comments dark-cyan (doom-lighten base5 0.2)))
   (doc-comments   (doom-lighten (if doom-redprint-brighter-comments dark-cyan base5) 0.25))
   (constants      blue)
   (functions      cyan)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           teal)
   (strings        green)
   (variables      base7)
   (numbers        magenta)
   (region         (pcase doom-redprint-region-highlight
                     (`frost teal)
                     (`snowstorm base7)
                     (_ base4)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-redprint-brighter-modeline)
   (-modeline-pad
    (when doom-redprint-padded-modeline
      (if (integerp doom-redprint-padded-modeline) doom-redprint-padded-modeline 4)))

   (region-fg
    (when (memq doom-redprint-region-highlight '(frost snowstorm))
      base0))

   (modeline-fg     'unspecified)
   (modeline-fg-alt base6)

   (modeline-bg
    (if -modeline-bright
        (doom-blend bg base5 0.2)
      `(,(doom-darken (car bg) 0.1) ,@(cdr base2))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-blend bg base5 0.2)
      base1))
   (modeline-bg-inactive   `(,(doom-darken (car bg) 0.1)   ,@(cdr base2)))
   (modeline-bg-inactive-l `(,(doom-darken (car bg) 0.025) ,@(cdr base2))))


  ;;;; Base theme face overrides
  ((fringe :foreground teal)
   ((line-number &override) :foreground (doom-lighten 'base5 0.2))
   ((line-number-current-line &override) :foreground base7)
   ((font-lock-comment-face &override)
    :background (if doom-redprint-comment-bg (doom-lighten bg 0.05) 'unspecified))
   ((tab-line &override) :background modeline-bg :foreground blue)
   ((tab-line-tab-inactive &override) :foreground dark-blue)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))
   ((region &override) :foreground (or region-fg 'unspecified))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-project-root-dir :foreground base6)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; highlight-symbol
   (highlight-symbol-face :background (doom-lighten base4 0.1) :distant-foreground fg-alt)
   ;;;; highlight-thing
   (highlight-thing :background (doom-lighten base4 0.1) :distant-foreground fg-alt)
   ;;;; ivy
   ((ivy-current-match &override) :foreground (or region-fg 'unspecified) :weight 'semi-bold)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; mic-paren
   ((paren-face-match &override) :foreground bg :background teal :weight 'ultra-bold)
   ((paren-face-mismatch &override) :foreground base7 :background red :weight 'ultra-bold)
   ;;;; org <built-in>
   (org-hide :foreground hidden)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   ;;;; vimish-fold
   ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background base3 :weight 'light)
   ((vimish-fold-fringe &override)  :foreground teal))

  ;;;; Base theme variable overrides-
  ())

;;; doom-redprint-theme.el ends here
