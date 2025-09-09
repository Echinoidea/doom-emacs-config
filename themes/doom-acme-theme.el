;;; doom-acme-theme.el --- Doom Emacs theme inspired by Acme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Echinoidea
;; Maintainer: Echinoidea
;; Source: https://github.com/Echinoidea/doom-acme-theme
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-acme-theme nil
  "Options for the `doom-acme' theme."
  :group 'doom-themes)

(defcustom doom-acme-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-acme-theme
  :type 'boolean)

(defcustom doom-acme-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-acme-theme
  :type 'boolean)

(defcustom doom-acme-comment-bg doom-acme-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-acme-theme
  :type 'boolean)

(defcustom doom-acme-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-acme-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-acme
    "Acme text editor theme"
  :family 'doom-acme
  :background-mode 'light

  ;; name        default   256       16
  (
   ;; Acme color palette
   (acme-base         '("#ffffea" "yellow"  "yellow" )) ; DPaleyellow - main background
   (acme-surface      '("#eaffff" "yellow"  "yellow" )) ; DDarkyellow - secondary background
   (acme-white        '("#FFFFFF" "white"   "white"  )) ; DWhite
   (acme-black        '("#000000" "black"   "black"  )) ; DBlack - main text
   (acme-red          '("#FF0000" "red"     "red"    )) ; DRed
   (acme-green        '("#00FF00" "green"   "green"  )) ; DGreen
   (acme-blue         '("#0000FF" "blue"    "blue"   )) ; DBlue
   (acme-cyan         '("#00FFFF" "cyan"    "cyan"   )) ; DCyan
   (acme-magenta      '("#FF00FF" "magenta" "magenta")) ; DMagenta
   (acme-yellow       '("#FFFF00" "yellow"  "yellow" )) ; DYellow
   (acme-paleyellow   '("#ededaa" "yellow"  "yellow" )) ; DYellow
   (acme-darkgreen    '("#448844" "green"   "green"  )) ; DDarkgreen
   (acme-palegreen    '("#AAFFAA" "green"   "green"  )) ; DPalegreen
   (acme-medgreen     '("#88CC88" "green"   "green"  )) ; DMedgreen
   (acme-darkblue     '("#000055" "blue"    "blue"   )) ; DDarkblue
   (acme-palebluegreen '("#AAFFFF" "cyan"   "cyan"   )) ; DPalebluegreen
   (acme-paleblue     '("#0000BB" "blue"    "blue"   )) ; DPaleblue
   (acme-bluegreen    '("#008888" "cyan"    "cyan"   )) ; DBluegreen
   (acme-greygreen    '("#55AAAA" "cyan"    "cyan"   )) ; DGreygreen
   (acme-palegreygreen '("#9EEEEE" "cyan"   "cyan"   )) ; DPalegreygreen
   (acme-yellowgreen  '("#99994C" "yellow"  "yellow" )) ; DYellowgreen
   (acme-medblue      '("#000099" "blue"    "blue"   )) ; DMedblue
   (acme-greyblue     '("#005DBB" "blue"    "blue"   )) ; DGreyblue
   (acme-palegreyblue '("#4993DD" "blue"    "blue"   )) ; DPalegreyblue
   (acme-purpleblue   '("#8888CC" "magenta" "magenta")) ; DPurpleblue

   ;; ui
   (alt-accent            acme-palegreyblue)
   (ui-line               acme-surface)
   (ui-panel-shadow       (doom-darken acme-base 0.15))
   (ui-panel-border       (doom-darken acme-base 0.25))
   (ui-gutter-normal      (doom-darken acme-base 0.20))
   (ui-gutter-active      (doom-darken acme-base 0.30))
   (ui-selection-bg       (doom-darken acme-base 0.10))
   (ui-selection-inactive (doom-darken acme-base 0.05))
   (ui-selection-border   (doom-darken acme-base 0.35))
   (ui-guide-normal       (doom-darken acme-base 0.0))
   (ui-guide-active       (doom-darken acme-base 0.25))
   (ui-org-block          acme-surface)
   (elscreen-bg           (doom-darken acme-base 0.20))
   (elscreen-fg           acme-black)
   ;; vcs
   (vcs-added    acme-darkgreen)
   (vcs-modified acme-medblue)
   (vcs-removed  acme-red)

   (bg         acme-base)
   (bg-alt     acme-surface)
   (base0      ui-gutter-normal)
   (base1      ui-gutter-active)
   (base2      ui-selection-bg)
   (base3      ui-org-block)
   (base4      ui-selection-border)
   (base5      ui-guide-normal)
   (base6      ui-guide-normal)
   (base7      ui-panel-shadow)
   (base8      ui-panel-border)
   (fg         acme-black)
   (fg-alt     (doom-darken acme-black 0.3))

   (grey       (doom-darken acme-base 0.40))
   (red        acme-red)
   (orange     acme-yellowgreen)
   (green      acme-darkgreen)
   (teal       acme-bluegreen)
   (yellow     acme-yellow)
   (blue       acme-medblue)
   (dark-blue  acme-darkblue)
   (magenta    acme-magenta)
   (violet     acme-purpleblue)
   (cyan       acme-cyan)
   (dark-cyan  acme-greyblue)

   ;; face categories -- required for all themes
   (highlight      acme-palegreyblue)
   (vertical-bar   ui-panel-border)
   (selection      nil)
   (builtin        acme-medblue)
   (comments       (if doom-acme-brighter-comments acme-greyblue (doom-darken acme-base 0.40)))
   (doc-comments   (doom-darken (if doom-acme-brighter-comments acme-greyblue (doom-darken acme-base 0.40)) 0.15))
   (constants      acme-purpleblue)
   (functions      acme-darkblue)
   (keywords       acme-greyblue)
   (methods        acme-medblue)
   (operators      (doom-darken acme-black 0.2))
   (type           acme-darkgreen)
   (strings        acme-yellowgreen)
   (variables      acme-black)
   (numbers        acme-purpleblue)
   (region         acme-paleyellow)
   (error          acme-red)
   (warning        acme-yellowgreen)
   (success        acme-darkgreen)
   (vc-modified    vcs-modified)
   (vc-added       vcs-added)
   (vc-deleted     vcs-removed)

   ;; custom categories
   (hidden     (car bg))
   (-modeline-bright doom-acme-brighter-modeline)
   (-modeline-pad
    (when doom-acme-padded-modeline
      (if (integerp doom-acme-padded-modeline) doom-acme-padded-modeline 4)))

   (modeline-fg     acme-black)
   (modeline-fg-alt acme-darkblue)

   (modeline-bg
    (if -modeline-bright
        acme-palegreyblue
      `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken acme-palegreyblue 0.1)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.05) ,@(cdr bg-alt))))

  ;;;; Base theme face overrides
  ((hl-line :background ui-selection-inactive)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   (diff-removed :foreground vcs-removed)
   (font-lock-comment-face
    :foreground comments
    :background (if doom-acme-comment-bg (doom-darken bg 0.02) 'unspecified))
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

   ;;;; LSP
   (lsp-face-highlight-textual :background alt-accent :foreground acme-black)

   ;;;; nav-flash
   (nav-flash-face :background acme-palegreyblue :foreground acme-black)

   ;;;; company
   (company-tooltip :foreground acme-black :background acme-base)
   (company-tooltip-annotation :foreground acme-black)
   (company-tooltip-selection :background acme-surface)
   (company-tooltip-search :foreground acme-darkblue :weight 'bold)
   (company-scrollbar-bg :background acme-base)
   (company-scrollbar-fg :background (doom-darken acme-base 0.3))
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground teal)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg modeline-bg) :weight 'normal)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'normal)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'normal)
   (doom-modeline-buffer-project-root :foreground green :weight 'normal)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background elscreen-bg :foreground elscreen-fg)
   ;;;; ivy
   (ivy-current-match :background (doom-darken acme-base 0.15))
   (ivy-minibuffer-match-face-1 :foreground acme-darkblue :weight 'bold)
   (ivy-minibuffer-match-face-2 :foreground acme-darkblue :weight 'bold)
   (ivy-minibuffer-match-face-3 :foreground acme-darkblue :weight 'bold)
   (ivy-minibuffer-match-face-4 :foreground acme-darkblue :weight 'bold)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground cyan)
   ((markdown-code-face &override) :background acme-surface)
   ;;;; org-mode
   (org-hide :foreground hidden)
   (org-headline-done :foreground (doom-darken acme-base 0.40))
   (org-document-info-keyword :foreground comments)
   (org-macro :foreground (doom-darken acme-black 0.2))
   (org-level-1 :foreground acme-darkblue :weight 'bold)
   (org-level-2 :foreground acme-greyblue :weight 'bold)
   (org-level-3 :foreground acme-medblue :weight 'bold)
   (org-level-4 :foreground acme-darkgreen :weight 'bold)
   (org-code :foreground acme-yellowgreen)
   (org-verbatim :foreground acme-bluegreen)
   ;;;; mic-paren
   ((paren-face-match &override) :foreground fg :background ui-selection-bg :weight 'ultra-bold)
   ;;;; rjsx-mode / js2-mode / typescript-mode / typescript-tsx-mode
   (rjsx-tag :foreground teal)
   (rjsx-tag-bracket-face :foreground (doom-darken teal 0.5))
   (rjsx-attr :foreground cyan)
   (js2-object-property :foreground blue)
   (js2-function-call :foreground functions)
   (js2-function-param :foreground variables)
   (typescript-keyword :foreground keywords)
   (typescript-type :foreground type)
   (typescript-access-modifier :foreground keywords)
   (typescript-primitive-face :foreground type)
   (typescript-class-name :foreground type)
   (typescript-function-name :foreground functions)
   (typescript-method-name :foreground methods)
   ;; TypeScript TSX mode
   (typescript-tsx-tag-face :foreground teal)
   (typescript-tsx-tag-bracket-face :foreground (doom-darken teal 0.5))
   (typescript-tsx-attribute-face :foreground cyan)
   (typescript-tsx-expression-braces-face :foreground (doom-darken acme-black 0.2))
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
   (web-mode-variable-name-face :foreground acme-red)
   (web-mode-function-name-face :foreground acme-purpleblue)
   (web-mode-html-tag-bracket-face :foreground (doom-darken teal 0.5))
   (web-mode-html-attr-name-face :foreground cyan)
   ;;;; tree-sitter faces
   (tree-sitter-hl-face:string :foreground strings)
   (tree-sitter-hl-face:type :foreground type :weight 'bold)
   (tree-sitter-hl-face:constant :foreground constants)
   (tree-sitter-hl-face:number :foreground numbers)
   (tree-sitter-hl-face:operator :foreground operators)
   (tree-sitter-hl-face:keyword :foreground keywords :weight 'bold)
   (tree-sitter-hl-face:function :foreground functions)
   (tree-sitter-hl-face:function.call :foreground functions)
   (tree-sitter-hl-face:variable :foreground variables)
   (tree-sitter-hl-face:property :foreground blue)
   (tree-sitter-hl-face:method :foreground functions)
   (tree-sitter-hl-face:tag :foreground teal)
   (tree-sitter-hl-face:attribute :foreground cyan)
   (tree-sitter-hl-face:constructor :foreground type)
   (tree-sitter-hl-face:punctuation :foreground (doom-darken acme-black 0.2))
   (tree-sitter-hl-face:punctuation.bracket :foreground (doom-darken acme-base 0.3))
   (tree-sitter-hl-face:punctuation.delimiter :foreground (doom-darken acme-black 0.2))))

;;; doom-acme-theme.el ends here
