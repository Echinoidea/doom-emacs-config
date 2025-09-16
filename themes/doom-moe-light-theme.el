;;; doom-moe-light-theme.el --- Doom Emacs theme port of Moe Light -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author:
;; Maintainer:
;; Source: Based on moe-light-theme.el
;;
;;; Commentary:
;; An eye-candy light theme for Doom Emacs. Moe, moe, kyun!
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-moe-light-theme nil
  "Options for the `doom-moe-light' theme."
  :group 'doom-themes)

(defcustom doom-moe-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-moe-light-theme
  :type 'boolean)

(defcustom doom-moe-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-moe-light-theme
  :type 'boolean)

(defcustom doom-moe-light-comment-bg doom-moe-light-brighter-comments
  "If non-nil, comments will have a subtle, darker background."
  :group 'doom-moe-light-theme
  :type 'boolean)

(defcustom doom-moe-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line."
  :group 'doom-moe-light-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-moe-light
    "A light theme inspired by Moe Light. Moe, moe, kyun!"
  :family 'doom-moe-light
  :background-mode 'light

  ;; name        default   256       16
  (
   ;; Moe Light palette colors
   (yellow-1 '("#fce94f" "yellow"  "yellow" ))
   (yellow-2 '("#ffd700" "yellow"  "yellow" ))
   (yellow-3 '("#c4a000" "yellow"  "yellow" ))
   (yellow-4 '("#875f00" "yellow"  "yellow" ))
   (orange-1 '("#ffaf5f" "orange"  "orange" ))
   (orange-2 '("#ff8700" "orange"  "orange" ))
   (orange-3 '("#ff5d17" "orange"  "orange" ))
   (orange-4 '("#d75f00" "orange"  "orange" ))
   (orange-5 '("#af5f00" "orange"  "orange" ))
   (magenta-1 '("#ff7bbb" "magenta" "magenta"))
   (magenta-2 '("#ff4ea3" "magenta" "magenta"))
   (magenta-3 '("#ff1f8b" "magenta" "magenta"))
   (green-1 '("#afff00" "green"   "green"  ))
   (green-2 '("#a1db00" "green"   "green"  ))
   (green-3 '("#00af00" "green"   "green"  ))
   (green-4 '("#008700" "green"   "green"  ))
   (green-5 '("#005f00" "green"   "green"  ))
   (blue-1 '("#5fafd7" "blue"    "blue"   ))
   (blue-2 '("#1f5bff" "blue"    "blue"   ))
   (blue-3 '("#005f87" "blue"    "blue"   ))
   (blue-4 '("#005faf" "blue"    "blue"   ))
   (blue-5 '("#0000af" "blue"    "blue"   ))
   (cyan-1 '("#87ffff" "cyan"    "cyan"   ))
   (cyan-2 '("#87d7af" "cyan"    "cyan"   ))
   (cyan-3 '("#00d7af" "cyan"    "cyan"   ))
   (cyan-4 '("#00ac8a" "cyan"    "cyan"   ))
   (cyan-5 '("#18b2b2" "cyan"    "cyan"   ))
   (cyan-6 '("#005f5f" "cyan"    "cyan"   ))
   (purple-1 '("#d18aff" "magenta" "magenta"))
   (purple-2 '("#b218b2" "magenta" "magenta"))
   (purple-3 '("#6c0099" "magenta" "magenta"))
   (red-1 '("#ef2929" "red"     "red"    ))
   (red-2 '("#cc0000" "red"     "red"    ))
   (red-3 '("#a40000" "red"     "red"    ))
   (white-1 '("#eeeeee" "white"   "white"  ))
   (white-2 '("#dadada" "white"   "white"  ))
   (white-3 '("#c6c6c6" "white"   "white"  ))
   (white-4 '("#b2b2b2" "white"   "white"  ))
   (black-1 '("#9e9e9e" "black"   "black"  ))
   (black-2 '("#8a8a8a" "black"   "black"  ))
   (black-3 '("#767676" "black"   "black"  ))
   (black-4 '("#626262" "black"   "black"  ))
   (black-5 '("#5f5f5f" "black"   "black"  ))
   (black-6 '("#3a3a3a" "black"   "black"  ))
   (light-bg '("#ffffd7" "white"   "white"  ))
   (white-0 '("#ffffff" "white"   "white"  ))

   ;; Moe Light highlight colors
   (green-0 '("#d7ffd7" "green"   "green"  ))
   (green-00 '("#d7ff87" "green"   "green"  ))
   (blue-0 '("#afd7ff" "blue"    "blue"   ))
   (blue-00 '("#d7d7ff" "blue"    "blue"   ))
   (blue-000 '("#d4e5ff" "blue"    "blue"   ))
   (yellow-0 '("#ffff87" "yellow"  "yellow" ))
   (yellow-00 '("#ffffaf" "yellow"  "yellow" ))
   (red-0 '("#ff4b4b" "red"     "red"    ))
   (red-00 '("#ffafaf" "red"     "red"    ))
   (red-000 '("#ffd5e5" "red"     "red"    ))
   (magenta-0 '("#ffafd7" "magenta" "magenta"))
   (magenta-00 '("#ffd7ff" "magenta" "magenta"))
   (orange-0 '("#ffaf87" "orange"  "orange" ))
   (orange-00 '("#ffd787" "orange"  "orange" ))
   (orange-000 '("#ffd7af" "orange"  "orange" ))
   (purple-00 '("#e6a8df" "magenta" "magenta"))
   (cyan-0 '("#d7ffd7" "cyan"    "cyan"   ))
   (linum-dark '("#87875f" "grey"    "grey"   ))
   (linum-light '("#d7d7af" "grey"    "grey"   ))

   ;; ui
   (ui-line               white-2)
   (ui-panel-shadow       (doom-darken white-1 0.35))
   (ui-panel-border       white-3)
   (ui-gutter-normal      linum-dark)
   (ui-gutter-active      black-1)
   (ui-selection-bg       blue-0)
   (ui-selection-inactive green-00)
   (ui-selection-border   white-4)
   (ui-guide-normal       (doom-lighten white-4 0.35))
   (ui-guide-active       black-1)
   (ui-org-block          (doom-lighten light-bg 0.05))

   ;; Base colors
   (bg         light-bg)
   (bg-alt     white-1)
   (base0      ui-gutter-normal)
   (base1      ui-gutter-active)
   (base2      ui-selection-bg)
   (base3      ui-org-block)
   (base4      ui-selection-border)
   (base5      ui-guide-normal)
   (base6      ui-guide-normal)
   (base7      ui-panel-shadow)
   (base8      ui-panel-border)
   (fg         black-5)
   (fg-alt     black-1)

   (grey       black-1)
   (red        red-1)
   (orange     orange-2)
   (green      green-3)
   (teal       cyan-3)
   (yellow     yellow-2)
   (blue       blue-1)
   (dark-blue  blue-3)
   (magenta    magenta-2)
   (violet     purple-1)
   (cyan       cyan-1)
   (dark-cyan  cyan-6)

   ;; face categories -- required for all themes
   (highlight      teal)
   (vertical-bar   ui-panel-border)
   (selection      nil)
   (builtin        purple-2)
   (comments       (if doom-moe-light-brighter-comments cyan-5 white-4))
   (doc-comments   yellow-3)
   (constants      blue-2)
   (functions      red-1)
   (keywords       green-3)
   (methods        cyan-3)
   (operators      black-1)
   (type           cyan-5)
   (strings        magenta-3)
   (variables      orange-2)
   (numbers        purple-2)
   (region         ui-selection-bg)
   (error          red-2)
   (warning        orange-2)
   (success        green-2)
   (vc-modified    blue-1)
   (vc-added       green-3)
   (vc-deleted     red-2)

   ;; custom categories
   (hidden     (car bg))
   (-modeline-bright doom-moe-light-brighter-modeline)
   (-modeline-pad
    (when doom-moe-light-padded-modeline
      (if (integerp doom-moe-light-padded-modeline) doom-moe-light-padded-modeline 4)))

   (modeline-fg     black-5)
   (modeline-fg-alt orange-2)

   (modeline-bg
    (if -modeline-bright
        blue-1
      `(,(doom-lighten (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-lighten blue-1 0.1)
      `(,(doom-lighten (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-l `(,(doom-lighten (car bg-alt) 0.1) ,@(cdr bg-alt))))

  ;;;; Base theme face overrides
  ((hl-line :background green-00)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   (diff-removed :foreground vc-deleted)
   (font-lock-comment-face
    :foreground comments
    :background (if doom-moe-light-comment-bg (doom-darken bg 0.05) 'unspecified))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   (font-lock-string-face :foreground strings)
   (font-lock-type-face :foreground type :weight 'bold)
   (font-lock-constant-face :foreground constants)
   (font-lock-number-face :foreground numbers)
   (font-lock-boolean-face :foreground red :weight 'bold)
   (font-lock-keyword-face :foreground keywords :weight 'bold)
   (font-lock-function-name-face :foreground functions)
   (font-lock-variable-name-face :foreground variables)
   (font-lock-builtin-face :foreground builtin)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; company
   (company-tooltip :foreground fg :background bg-alt)
   (company-tooltip-annotation :foreground fg)
   (company-tooltip-selection :background ui-selection-bg)
   (company-tooltip-search :foreground orange :weight 'bold)
   (company-scrollbar-bg :background bg-alt)
   (company-scrollbar-fg :background grey)

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground teal)

   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg modeline-bg) :weight 'normal)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'normal)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'normal)
   (doom-modeline-buffer-project-root :foreground green :weight 'normal)

   ;;;; ivy
   (ivy-current-match :background ui-selection-bg)
   (ivy-minibuffer-match-face-1 :foreground orange :weight 'bold)
   (ivy-minibuffer-match-face-2 :foreground orange :weight 'bold)
   (ivy-minibuffer-match-face-3 :foreground orange :weight 'bold)
   (ivy-minibuffer-match-face-4 :foreground orange :weight 'bold)

   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)

   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground orange)
   ((markdown-code-face &override) :background (doom-darken bg 0.05))

   ;;;; org-mode
   (org-hide :foreground hidden)
   (org-headline-done :foreground grey)
   (org-document-info-keyword :foreground comments)
   (org-level-1 :foreground blue-1 :weight 'bold)
   (org-level-2 :foreground green-2 :weight 'bold)
   (org-level-3 :foreground orange-2 :weight 'bold)
   (org-level-4 :foreground cyan-3 :weight 'bold)
   (org-level-5 :foreground red-2 :weight 'bold)
   (org-level-6 :foreground purple-2 :weight 'bold)
   (org-level-7 :foreground magenta-2 :weight 'bold)
   (org-level-8 :foreground yellow-2 :weight 'bold)
   (org-code :foreground blue-2)
   (org-verbatim :foreground blue-3)
   (org-block :foreground blue-3 :background blue-00)
   (org-block-begin-line :foreground blue-3 :background blue-0)
   (org-block-end-line :foreground white-2 :background white-1)

   ;;;; mic-paren
   ((paren-face-match &override) :foreground fg :background ui-selection-bg :weight 'ultra-bold)

   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;;;; web-mode
   (web-mode-html-tag-face :foreground teal)
   (web-mode-variable-name-face :foreground variables)
   (web-mode-function-name-face :foreground functions)
   (web-mode-html-tag-bracket-face :foreground (doom-darken teal 0.5))
   (web-mode-html-attr-name-face :foreground cyan)

   ;;;; magit
   (magit-section-highlight :background yellow-00)
   (magit-diff-added :foreground green-3 :background green-0)
   (magit-diff-added-highlight :foreground green-3 :background green-00)
   (magit-diff-removed :foreground red-2 :background red-00)
   (magit-diff-removed-highlight :foreground red-2 :background red-00)
   (magit-diff-context :foreground white-4)
   (magit-diff-context-highlight :foreground black-1 :background yellow-00)
   (magit-diff-hunk-heading :foreground white-4 :background white-1)
   (magit-diff-hunk-heading-highlight :foreground white-0 :background black-1)
   (magit-branch-local :foreground green-4 :background green-00)
   (magit-branch-remote :foreground blue-3 :background blue-0)
   (magit-tag :foreground purple-3 :background purple-00)

   ;;;; dired
   (dired-directory :foreground blue-1 :weight 'bold)
   (dired-marked :foreground green-2)
   (dired-symlink :foreground magenta-2)
   (dired-header :foreground black-5 :background green-1 :weight 'bold)))

;;; doom-moe-light-theme.el ends here
