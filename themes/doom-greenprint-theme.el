;;; doom-greenprint-theme.el --- dark variant of Greenprint -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: March 8, 2018 (c503ebdacac1)
;; Author: fuxialexander <https://github.com/fuxialexander>
;; Maintainer:
;; Source: https://www.greenprinttheme.com
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-greenprint-theme nil
  "Options for the `doom-greenprint' theme."
  :group 'doom-themes)

(defcustom doom-greenprint-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-greenprint-theme
  :type 'boolean)

(defcustom doom-greenprint-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-greenprint-theme
  :type 'boolean)

(defcustom doom-greenprint-comment-bg doom-greenprint-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-greenprint-theme
  :type 'boolean)

(defcustom doom-greenprint-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-greenprint-theme
  :type '(choice integer boolean))

(eval-and-compile
  (defcustom doom-greenprint-region-highlight t
    "Determines the selection highlight style. Can be 'frost, 'snowstorm or t
(default)."
    :group 'doom-greenprint-theme
    :type 'symbol))


;;
;;; Theme definition

(def-doom-theme doom-greenprint
    "A dark theme inspired by Greenprint."
  :family 'doom-greenprint
  :background-mode 'dark

  ;; name        default   256       16
  ((bg         '("#5A6855" nil       nil            ))
   (bg-alt     '("#7E8F79" nil       nil            ))
   (base0      '("#94A18F" "#94A18F"   "black"        ))
   (base1      '("#7E8F79" "#7E8F79" "brightblack"  ))
   (base2      '("#7E8F79" "#7E8F79" "brightblack"  ))
   (base3      '("#94A18F" "#94A18F" "brightblack"  ))
   (base4      '("#94A18F" "#94A18F" "brightblack"  ))
   (base5      '("#94A18F" "#94A18F" "brightblack"  ))
   (base6      '("#F4F0E7" "#F4F0E7" "brightblack"  ))
   (base7      '("#F4F0E7" "#F4F0E7" "brightblack"  ))
   (base8      '("#F4F0E7" "#F4F0E7" "white"        ))
   (fg         '("#F4F0E7" "#F4F0E7" "white"        ))
   (fg-alt     '("#F4F0E7" "#F4F0E7" "brightwhite"  ))
   (grey       base4)
   (red        '("#96A890" "#96A890" "red"          ))
   (orange     '("#96A890" "#96A890" "brightred"    ))
   (green      '("#AFA988" "#AFA988" "green"        ))
   (teal       '("#ACB8A3" "#ACB8A3" "brightgreen"  ))
   (yellow     '("#C2C7B5" "#C2C7B5" "yellow"       ))
   (blue       '("#DECDAC" "#DECDAC" "brightblue"   ))
   (dark-blue  '("#DECDAC" "#DECDAC" "blue"         ))
   (magenta    '("#C6BB9A" "#C6BB9A" "magenta"      ))
   (violet     '("#C6BB9A" "#C6BB9A" "brightmagenta"))
   (cyan       '("#ACB8A3" "#ACB8A3" "brightcyan"   ))
   (dark-cyan  '("#ACB8A3" "#ACB8A3" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.2))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-greenprint-brighter-comments dark-cyan (doom-lighten base5 0.2)))
   (doc-comments   (doom-lighten (if doom-greenprint-brighter-comments dark-cyan base5) 0.25))
   (constants      blue)
   (functions      cyan)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           teal)
   (strings        green)
   (variables      base7)
   (numbers        magenta)
   (region         (pcase doom-greenprint-region-highlight
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
   (-modeline-bright doom-greenprint-brighter-modeline)
   (-modeline-pad
    (when doom-greenprint-padded-modeline
      (if (integerp doom-greenprint-padded-modeline) doom-greenprint-padded-modeline 4)))

   (region-fg
    (when (memq doom-greenprint-region-highlight '(frost snowstorm))
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
    :background (if doom-greenprint-comment-bg (doom-lighten bg 0.05) 'unspecified))
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

;;; doom-greenprint-theme.el ends here
