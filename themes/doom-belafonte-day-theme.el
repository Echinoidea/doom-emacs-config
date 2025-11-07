;;; themes/doom-belafonte-day-theme.el -*- lexical-binding: t; -*-


(require 'doom-themes)


;;
;;; Variables

(defgroup doom-belafonte-day-theme nil
  "Options for the `doom-belafonte-day' theme."
  :group 'doom-themes)

(defcustom doom-belafonte-day-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-belafonte-day-theme
  :type 'boolean)

(defcustom doom-belafonte-day-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-belafonte-day-theme
  :type 'boolean)

(defcustom doom-belafonte-day-comment-bg doom-belafonte-day-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-belafonte-day-theme
  :type 'boolean)

(defcustom doom-belafonte-day-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-belafonte-day-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-belafonte-day
    "A dark theme inspired by Rose Pine"
  :family 'doom-belafonte-day
  :background-mode 'dark


  ;; background            #d4ccb9
  ;; foreground            #45363b
  ;; cursor                #45363b
  ;; selection_background  #958b83
  ;; color0                #20111a
  ;; color8                #5e5252
  ;; color1                #bd100d
  ;; color9                #bd100d
  ;; color2                #858062
  ;; color10               #858062
  ;; color3                #e9a448
  ;; color11               #e9a448
  ;; color4                #416978
  ;; color12               #416978
  ;; color5                #96522b
  ;; color13               #96522b
  ;; color6                #98999c
  ;; color14               #98999c
  ;; color7                #958b83
  ;; color15               #d4ccb9
  ;; selection_foreground #d4ccb9

  ;; name        default   256       16
  (
   ;; Rose Pine main palette
   (bd-base       '("#d4ccb9" "black"   "black"  ))
   (bd-surface    '("#98999c" "black"   "black"  ))
   (bd-overlay    '("#958b83" "grey"    "grey"   ))
   (bd-muted      '("#98999c" "grey"    "grey"   ))
   (bd-subtle     '("#858062" "grey"    "grey"   ))
   (bd-text       '("#45363b" "white"   "white"  ))
   (bd-love       '("#bd100d" "red"     "red"    ))
   (bd-gold       '("#e9a448" "yellow"  "yellow" ))
   (bd-rose       '("#96522b" "cyan"    "cyan"   ))
   (bd-pine       '("#858062" "blue"    "blue"   ))
   (bd-foam       '("#98999c" "cyan"    "cyan"   ))
   (bd-iris       '("#96522b" "magenta" "magenta"))
   (bd-leaf       '("#5e5252" "green"   "green"  ))
   ;; Rose Pine highlight colors
   (bd-highlight-low  '("#958b83" "grey" "grey"))
   (bd-highlight-med  '("#a09188" "grey" "grey"))
   (bd-highlight-high '("#a59693" "grey" "grey"))

   ;; ui
   (alt-accent            (doom-lighten bd-rose 0.4))
   (ui-line               bd-surface)
   (ui-panel-shadow       (doom-darken bd-base 0.35))
   (ui-panel-border       bd-overlay)
   (ui-gutter-normal      bd-muted)
   (ui-gutter-active      bd-subtle)
   (ui-selection-bg       bd-highlight-med)
   (ui-selection-inactive bd-highlight-low)
   (ui-selection-border   bd-highlight-high)
   (ui-guide-normal       (doom-darken bd-muted 0.35))
   (ui-guide-active       bd-subtle)
   (ui-org-block          (doom-darken bd-base 0.10))
   (elscreen-bg           (doom-darken bd-overlay 0.25))
   (elscreen-fg           bd-surface)
   ;; vcs
   (vcs-added    bd-leaf)
   (vcs-modified bd-foam)
   (vcs-removed  bd-love)

   (bg         bd-base)
   (bg-alt     bd-surface)
   (base0      ui-gutter-normal)
   (base1      ui-gutter-active)
   (base2      ui-selection-bg)
   (base3      ui-org-block)
   (base4      ui-selection-border)
   (base5      ui-guide-normal)
   (base6      ui-guide-normal)
   (base7      ui-panel-shadow)
   (base8      ui-panel-border)
   (fg         bd-text)
   (fg-alt     bd-subtle)

   (grey       bd-muted)
   (red        bd-love)
   (orange     bd-gold)
   (green      bd-leaf)
   (teal       bd-pine)
   (yellow     bd-gold)
   (blue       bd-foam)
   (dark-blue  (doom-darken bd-pine 0.2))
   (magenta    bd-iris)
   (violet     (doom-lighten bd-iris 0.2))
   (cyan       bd-rose)
   (dark-cyan  bd-pine)

   ;; face categories -- required for all themes
   (highlight      bd-gold)
   (vertical-bar   ui-panel-border)
   (selection      nil)
   (builtin        bd-foam)
   (comments       (if doom-belafonte-day-brighter-comments bd-pine bd-muted))
   (doc-comments   (doom-lighten (if doom-belafonte-day-brighter-comments bd-pine bd-muted) 0.25))
   (constants      bd-iris)
   (functions      bd-rose)
   (keywords       bd-pine)
   (methods        bd-foam)
   (operators      bd-subtle)
   (type           bd-gold)
   (strings        bd-gold)
   (variables      bd-text)
   (numbers        bd-iris)
   (region         ui-selection-bg)
   (error          bd-love)
   (warning        bd-gold)
   (success        bd-leaf)
   (vc-modified    vcs-modified)
   (vc-added       vcs-added)
   (vc-deleted     vcs-removed)

   ;; custom categories
   (hidden     (car bg))
   (-modeline-bright doom-belafonte-day-brighter-modeline)
   (-modeline-pad
    (when doom-belafonte-day-padded-modeline
      (if (integebd doom-belafonte-day-padded-modeline) doom-belafonte-day-padded-modeline 4)))

   (modeline-fg     bd-text)
   (modeline-fg-alt bd-gold)

   (modeline-bg
    (if -modeline-bright
        (doom-darken bd-pine 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken bd-pine 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg-alt))))

  ;;;; Base theme face overrides
  ((hl-line :background bd-highlight-low)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   (diff-removed :foreground vcs-removed)
   (font-lock-comment-face
    :foreground comments
    :background (if doom-belafonte-day-comment-bg (doom-lighten bg 0.05) 'unspecified))
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
   (lsp-face-highlight-textual :background alt-accent :foreground bd-base)


   ;;;; nav-flash
   (nav-flash-face :background bd-rose :foreground bd-base )

   ;;;; company
   (company-tooltip :foreground bd-text :background bd-base)
   (company-tooltip-annotation :foreground bd-text)
   (company-tooltip-selection :background bd-surface)
   (company-tooltip-search :foreground bd-gold :weight 'bold)
   (company-scrollbar-bg :background bd-base)
   (company-scrollbar-fg :background bd-muted)
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
   (ivy-current-match :background bd-subtle)
   (ivy-minibuffer-match-face-1 :foreground bd-gold :weight 'bold)
   (ivy-minibuffer-match-face-2 :foreground bd-gold :weight 'bold)
   (ivy-minibuffer-match-face-3 :foreground bd-gold :weight 'bold)
   (ivy-minibuffer-match-face-4 :foreground bd-gold :weight 'bold)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground cyan)
   ((markdown-code-face &override) :background (doom-lighten bd-base 0.05))
   ;;;; org-mode
   (org-hide :foreground hidden)
   (org-headline-done :foreground bd-muted)
   (org-document-info-keyword :foreground comments)
   (org-macro :foreground bd-subtle)
   (org-level-1 :foreground cyan :weight 'bold)
   (org-level-2 :foreground magenta :weight 'bold)
   (org-level-3 :foreground blue :weight 'bold)
   (org-level-4 :foreground green :weight 'bold)
   (org-code :foreground yellow)
   (org-verbatim :foreground teal)
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
   (typescript-tsx-expression-braces-face :foreground bd-subtle)
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
   (web-mode-variable-name-face :foreground bd-love)
   (web-mode-function-name-face :foreground bd-iris)
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
   (tree-sitter-hl-face:punctuation :foreground bd-subtle)
   (tree-sitter-hl-face:punctuation.bracket :foreground bd-muted)
   (tree-sitter-hl-face:punctuation.delimiter :foreground bd-subtle)))

;;; doom-belafonte-day-theme.el ends here
