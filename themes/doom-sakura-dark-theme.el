;;; doom-sakura-dark-theme.el --- port of anAcc22's sakura.nvim (dark) theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Echinoidea
;; Maintainer: Echinoidea
;; Source: https://github.com/rijupaul/sakura-dark.nvim
;;
;;; Commentary:
;;
;; A low-contrast pink colorscheme inspired by zenbones, rose-pine, and lackluster.
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-sakura-dark-theme nil
  "Options for the `doom-sakura-dark' theme."
  :group 'doom-themes)

(defcustom doom-sakura-dark-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-sakura-dark-theme
  :type 'boolean)

(defcustom doom-sakura-dark-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-sakura-dark-theme
  :type 'boolean)

(defcustom doom-sakura-dark-comment-bg doom-sakura-dark-brighter-comments
  "If non-nil, comments will have a subtle highlight to enhance their legibility."
  :group 'doom-sakura-dark-theme
  :type 'boolean)

(defcustom doom-sakura-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-sakura-dark-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-sakura-dark
    "A low-contrast pink colorscheme for neovim inspired by zenbones, rose-pine, and lackluster."
  :family 'doom-sakura-dark
  :background-mode 'dark

  ;; name        default   256           16
  ;; Base colors from HSLuv(300, 6, 8) -> very dark purple background
  ((bg         '("#1a1719" "black"       "black"  ))
   (fg         '("#cc9fb0" "#cc9fb0"     "brightwhite"  ))

   ;; Off-color variants - slightly lighter background, muted foreground
   (bg-alt     '("#241f23" "black"       "black"        ))
   (fg-alt     '("#a98899" "#a98899"     "white"        ))

   ;; Gradient from bg to fg
   (base0      '("#1a1719" "black"       "black"        ))  ; bg0
   (base1      '("#241f23" "#241f23"     "brightblack"  ))  ; bg1
   (base2      '("#2d262a" "#2d262a"     "brightblack"  ))  ; bg2
   (base3      '("#5c4f55" "#5c4f55"     "brightblack"  ))  ; bg3
   (base4      '("#7a6b72" "#7a6b72"     "brightblack"  ))  ; intermediate
   (base5      '("#998188" "#998188"     "brightblack"  ))  ; fg9
   (base6      '("#b3969e" "#b3969e"     "brightblack"  ))  ; fg8
   (base7      '("#b89ba3" "#b89ba3"     "brightblack"  ))  ; fg1
   (base8      '("#cc9fb0" "#cc9fb0"     "white"        ))  ; fg0

   (grey       base4)

   ;; Theme colors mapped from HSLuv values
   (red        '("#8a4f5f" "#8a4f5f"     "red"          ))  ; er0 - hsluv(7, 55, 50)
   (orange     '("#997356" "#997356"     "brightred"    ))  ; yl0 - hsluv(40, 40, 60)
   (green      '("#6b9977" "#6b9977"     "green"        ))  ; gr0 - hsluv(150, 35, 60)
   (teal       '("#2d5433" "#2d5433"     "brightgreen"  ))  ; gr9 (darker)
   (yellow     '("#9a8066" "#9a8066"     "yellow"       ))  ; yl0 variant
   (blue       '("#7385a6" "#7385a6"     "blue"         ))  ; gb0 - hsluv(260, 35, 60)
   (dark-blue  '("#35476b" "#35476b"     "brightblue"   ))  ; gb9 (darker)
   (magenta    '("#a475a4" "#a475a4"     "magenta"      ))  ; gp0 - hsluv(270, 50, 65)
   (violet     '("#4d3d5c" "#4d3d5c"     "brightmagenta"))  ; gp9 (darker)
   (cyan       '("#b5759f" "#b5759f"     "cyan"         ))  ; sa0 - hsluv(340, 35, 65)
   (dark-cyan  '("#7a5f72" "#7a5f72"     "brightcyan"   ))  ; pi0 - hsluv(310, 15, 60)

   ;; Universal syntax classes
   (highlight      dark-cyan)
   (vertical-bar   (doom-darken base2 0.1))
   (selection      '("#4d3752" "#4d3752"))                   ; vs0 - hsluv(310, 12, 20)
   (builtin        magenta)
   (comments       (if doom-sakura-dark-brighter-comments base3 base3))
   (doc-comments   (doom-lighten base3 0.25))
   (constants      magenta)                                  ; gp1
   (functions      base5)                                    ; fg9
   (keywords       dark-cyan)                                ; pi0/pi1
   (methods        cyan)
   (operators      '("#997a8a" "#997a8a"))                   ; sa2 - hsluv(340, 30, 45)
   (type           cyan)                                     ; sa0
   (strings        blue)                                     ; gb0
   (variables      fg)
   (numbers        magenta)                                  ; gp0
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        orange)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; Extra color variables
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if doom-sakura-dark-brighter-modeline
                                 (doom-darken dark-cyan 0.45)
                               (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if doom-sakura-dark-brighter-modeline
                                 (doom-darken dark-cyan 0.475)
                               `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when doom-sakura-dark-padded-modeline
      (if (integerp doom-sakura-dark-padded-modeline) doom-sakura-dark-padded-modeline 4))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base3)
   ((line-number-current-line &override) :foreground fg :weight 'bold)
   ((font-lock-comment-face &override)
    :background (if doom-sakura-dark-comment-bg (doom-lighten bg 0.05) 'unspecified)
    :slant 'italic)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if doom-sakura-dark-brighter-modeline base8 highlight))

   (font-lock-string-face :foreground strings :slant 'italic)
   (font-lock-type-face :foreground type :slant 'italic :weight 'bold)
   (font-lock-constant-face :foreground constants)
   (font-lock-number-face :foreground numbers)
   (font-lock-boolean-face :foreground constants :weight 'bold)
   (font-lock-keyword-face :foreground keywords)
   (font-lock-function-name-face :foreground functions)
   (font-lock-variable-name-face :foreground variables)

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground dark-cyan)

   ;;;; doom-modeline
   (doom-modeline-bar :background (if doom-sakura-dark-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;;;; elscreen
   (elscreen-tab-other-screen-face :background base2 :foreground base0)

   ;;;; ivy
   (ivy-current-match :background selection :distant-foreground base0 :weight 'normal)

   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)

   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground cyan)
   ((markdown-code-face &override) :background (doom-lighten base2 0.05))

   ;;;; org-mode
   (org-level-1 :foreground cyan :weight 'bold)
   (org-level-2 :foreground magenta :weight 'bold)
   (org-level-3 :foreground blue :weight 'bold)
   (org-level-4 :foreground green :weight 'bold)
   (org-code :foreground blue :slant 'italic)
   (org-verbatim :foreground dark-cyan)

   ;;;; rjsx-mode / js2-mode / typescript-mode
   (rjsx-tag :foreground base6)
   (rjsx-attr :foreground cyan)
   (js2-object-property :foreground base6)
   (typescript-keyword :foreground keywords)

   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))

   ;;;; tree-sitter faces
   (tree-sitter-hl-face:string :foreground (doom-lighten strings 0.1) :slant 'italic)
   (tree-sitter-hl-face:type :foreground (doom-lighten type 0.1) :slant 'italic :weight 'bold)
   (tree-sitter-hl-face:constant :foreground (doom-lighten constants 0.1))
   (tree-sitter-hl-face:number :foreground (doom-lighten numbers 0.15))
   (tree-sitter-hl-face:operator :foreground (doom-lighten operators 0.1))
   (tree-sitter-hl-face:keyword :foreground (doom-lighten keywords 0.15))
   (tree-sitter-hl-face:function :foreground (doom-lighten functions 0.15))
   (tree-sitter-hl-face:variable :foreground variables)
   (tree-sitter-hl-face:property :foreground (doom-lighten base6 0.1))
   (tree-sitter-hl-face:method :foreground (doom-lighten functions 0.15)))

  ;;;; Base theme variable overrides
  ())

;;; doom-sakura-dark-theme.el ends here
