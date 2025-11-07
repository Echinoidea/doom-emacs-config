;;; themes/doom-everforest-light-hard-theme.el -*- lexical-binding: t; -*-

;; Author: Echinoidea <https://github.com/echinoidea>
;; Maintainer:
;; Source: https://github.com/sainnhe/everforest
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;; Compiler pacifier
(defvar modeline-bg)


;; everforest light hard colors -- use these
;; bg_dim #f2efdf
;; bg0 #fffbef
;; bg1 #f8f5e4
;; bg2 #f2efdf
;; bg3 #edeada
;; bg4 #e8e5d5
;; bg5 #bec5b2
;; bg_red #ffe7de
;; bg_yellow #fef2d5
;; bg_blue #ecf5ed
;; bg_purple #fceced
;; bg_visual #f0f2d4
;; fg #5c6a72
;; red #f85552
;; yellow #dfa000
;; green #8da101
;; blue #3a94c5
;; purple #df69ba
;; aqua #35a77c
;; orange #f57d26
;; grey0 #a6b0a0
;; grey1 #939f91
;; grey2 #829181
;; statusline1 #935259
;; statusline2 #708089
;; statusline3 #e66868
;;
;;; Variables

(defgroup doom-everforest-light-hard-theme nil
  "Options for doom-everforest-light-hard."
  :group 'doom-themes)

(defcustom doom-everforest-light-hard-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-everforest-light-hard-theme
  :type 'boolean)

(defcustom doom-everforest-light-hard-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-everforest-light-hard-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-everforest-light-hard
    "Green based light color scheme; designed to be warm and soft."

  ;; name        gui       256       16
  ((bg         '("#fffbef" "#fffbef" nil          )) ; bg0
   (bg-alt     '("#f8f5e4" "#f8f5e4" nil          )) ; bg1
   (bg-alt2    '("#f2efdf" "#f2efdf" "white"      )) ; bg2 (for region, selection etc.)

   (base0      '("#fffbef" "white"   "white"      )) ; bg0
   (base1      '("#f8f5e4" "#f8f5e4" "brightwhite")) ; bg1
   (base2      '("#f2efdf" "#f2efdf" "brightwhite")) ; bg2
   (base3      '("#edeada" "#edeada" "brightwhite")) ; bg3
   (base4      '("#e8e5d5" "#e8e5d5" "brightwhite")) ; bg4
   (base5      '("#bec5b2" "#bec5b2" "brightwhite")) ; bg5
   (base6      '("#939f91" "#939f91" "brightblack")) ; grey1
   (base7      '("#829181" "#829181" "brightblack")) ; grey2
   (base8      '("#5c6a72" "#5c6a72" "black"))       ; fg
   (fg         '("#5c6a72" "#5c6a72" "black"))       ; fg
   (fg-alt     '("#708089" "#708089" "black"))       ; statusline2

   (grey       '("#a6b0a0" "#a6b0a0" "brightblack"))   ; grey0
   (red        '("#f85552" "#f85552" "red"))           ; red
   (magenta    '("#df69ba" "#df69ba" "magenta"))       ; purple
   (violet     '("#df69ba" "#df69ba" "brightmagenta")) ; purple
   (orange     '("#f57d26" "#f57d26" "orange"))        ; orange
   (yellow     '("#dfa000" "#dfa000" "yellow"))        ; yellow
   (teal       '("#35a77c" "#35a77c" "green"))         ; aqua
   (green      '("#8da101" "#8da101" "green"))         ; green
   (dark-green '("#ecf5ed" "#ecf5ed" "green"))         ; bg_blue (light green bg)
   (blue       '("#3a94c5" "#3a94c5" "brightblue"))    ; blue
   (dark-blue  '("#ecf5ed" "#ecf5ed" "blue"))          ; bg_blue
   (cyan       '("#35a77c" "#35a77c" "brightcyan"))    ; aqua
   (dark-cyan  '("#3a94c5" "#3a94c5" "cyan"))          ; blue

   ;; face categories
   (highlight      yellow)
   (vertical-bar   base4)
   (selection      bg-alt2)
   (builtin        orange)
   (comments       (if doom-everforest-light-hard-brighter-comments magenta grey))
   (doc-comments   (if doom-everforest-light-hard-brighter-comments (doom-darken magenta 0.2) (doom-darken grey 0.15)))
   (constants      violet)
   (functions      cyan)
   (keywords       red)
   (methods        cyan)
   (operators      cyan)
   (type           yellow)
   (strings        green)
   (variables      cyan)
   (numbers        violet)
   (region         bg-alt2)
   (error          red)
   (warning        yellow)
   (success        green)

   (vc-modified    (doom-lighten blue 0.15))
   (vc-added       (doom-lighten green 0.15))
   (vc-deleted     (doom-lighten red 0.15))

   ;; custom categories
   (-modeline-pad
    (when doom-everforest-light-hard-padded-modeline
      (if (integerp doom-everforest-light-hard-padded-modeline)
          doom-everforest-light-hard-padded-modeline
        4)))

   (org-quote `(,(doom-darken (car bg) 0.05) "#f8f5e4")))


  ;;;; Base theme face overrides
  ((button :foreground cyan :underline t :bold t)
   (cursor :background fg)
   (font-lock-variable-name-face :foreground cyan :italic t)
   (hl-line :background bg-alt)
   (isearch :foreground base0 :background orange)
   (lazy-highlight
    :background yellow :foreground base0 :distant-foreground base0
    :weight 'bold)
   ((line-number &override) :foreground base5)
   ((line-number-current-line &override) :background bg-alt2 :foreground fg :bold t)
   (minibuffer-prompt :foreground cyan)
   (mode-line
    :background bg-alt2 :foreground (doom-darken fg-alt 0.25)
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color base3)))
   (mode-line-inactive
    :background bg :foreground base5
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color base2)))

   ;; vimish-fold
   ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background bg-alt2 :weight 'light)
   ((vimish-fold-mouse-face &override) :foreground fg :background yellow :weight 'light)
   ((vimish-fold-fringe &override) :foreground magenta :background magenta)
   ;;;; company
   (company-preview-common :foreground cyan)
   (company-tooltip-common :foreground cyan)
   (company-tooltip-common-selection :foreground cyan)
   (company-tooltip-annotation :foreground cyan)
   (company-tooltip-annotation-selection :foreground cyan)
   (company-scrollbar-bg :background bg-alt)
   (company-scrollbar-fg :background cyan)
   (company-tooltip-selection :background bg-alt2)
   (company-tooltip-mouse :background bg-alt2 :foreground nil)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground keywords)
   ;;;; dired
   (dired-directory :foreground cyan)
   (dired-marked :foreground yellow)
   (dired-symlink :foreground cyan)
   (dired-header :foreground cyan)
   ;;;; doom-emacs
   (+workspace-tab-selected-face :background dark-green :foreground fg)
   ;;;; doom-modeline
   (doom-modeline-bar :background dark-green)
   (doom-modeline-buffer-file :inherit 'bold :foreground fg)
   (doom-modeline-buffer-major-mode :foreground green :bold t)
   (doom-modeline-buffer-modified :inherit 'bold :foreground yellow)
   (doom-modeline-buffer-path :inherit 'bold :foreground green)
   (doom-modeline-error :background bg)
   (doom-modeline-info :bold t :foreground cyan)
   (doom-modeline-panel :background dark-green :foreground fg)
   (doom-modeline-project-dir :bold t :foreground cyan)
   (doom-modeline-warning :foreground red :bold t)
   ;;;; doom-themes
   (doom-themes-neotree-file-face :foreground fg)
   (doom-themes-neotree-hidden-file-face :foreground (doom-darken fg-alt 0.25))
   (doom-themes-neotree-media-file-face :foreground (doom-darken fg-alt 0.25))
   ;;;; ediff <built-in>
   (ediff-fine-diff-A    :background (doom-blend red bg 0.4) :weight 'bold)
   (ediff-current-diff-A :background (doom-blend red bg 0.2))
   ;;;; evil
   (evil-search-highlight-persist-highlight-face :background yellow)
   (evil-ex-substitute-replacement :foreground cyan :inherit 'evil-ex-substitute-matches)
   ;;;; evil-snipe
   (evil-snipe-first-match-face :foreground base0 :background yellow)
   (evil-snipe-matches-face     :foreground yellow :bold t :underline t)
   ;;;; flycheck
   (flycheck-error   :underline `(:style wave :color ,red)    :background base2)
   (flycheck-warning :underline `(:style wave :color ,yellow) :background base2)
   (flycheck-info    :underline `(:style wave :color ,cyan)   :background base2)
   ;;;; helm
   (helm-swoop-target-line-face :foreground magenta :inverse-video t)
   ;;;; highlight-quoted
   (highlight-quoted-symbol :foreground dark-cyan)
   ;;;; highlight-symbol
   (highlight-symbol-face :background (doom-darken base2 0.03) :distant-foreground fg-alt)
   ;;;; highlight-thing
   (highlight-thing :background (doom-darken base2 0.03) :distant-foreground fg-alt)
   ;;;; ivy
   (ivy-current-match :background bg-alt2)
   (ivy-subdir :background nil :foreground cyan)
   (ivy-action :background nil :foreground cyan)
   (ivy-grep-line-number :background nil :foreground cyan)
   (ivy-minibuffer-match-face-1 :background nil :foreground yellow :bold t)
   (ivy-minibuffer-match-face-2 :background nil :foreground red :bold t)
   (ivy-minibuffer-match-highlight :foreground cyan)
   (counsel-key-binding :foreground cyan)
   ;;;; ivy-posframe
   (ivy-posframe :background bg-alt)
   (ivy-posframe-border :background base1)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground dark-cyan)
   ;;;; magit
   (magit-section-heading             :foreground yellow :weight 'bold)
   (magit-branch-current              :underline cyan :inherit 'magit-branch-local)
   (magit-diff-hunk-heading           :background base3 :foreground fg-alt)
   (magit-diff-hunk-heading-highlight :background bg-alt2 :foreground fg)
   (magit-diff-context                :foreground bg-alt :foreground fg-alt)
   ;;;; markdown-mode
   (markdown-blockquote-face :inherit 'italic :foreground cyan)
   (markdown-list-face :foreground red)
   (markdown-url-face :foreground red)
   (markdown-pre-face  :foreground cyan)
   (markdown-link-face :inherit 'bold :foreground cyan)
   ((markdown-code-face &override) :background (doom-darken base1 0.045))
   ;;;; mu4e-view
   (mu4e-header-key-face :foreground red)
   ;;;; neotree
   (neo-root-dir-face   :foreground cyan)
   (doom-neotree-dir-face :foreground cyan)
   (neo-dir-link-face   :foreground cyan)
   (neo-expand-btn-face :foreground magenta)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground yellow)
   ((outline-2 &override) :foreground cyan)
   ((outline-3 &override) :foreground cyan)
   ;;;; org <built-in>
   (org-ellipsis :underline nil :foreground orange)
   (org-tag :foreground yellow :bold nil)
   ((org-quote &override) :inherit 'italic :foreground base7 :background org-quote)
   (org-todo :foreground yellow :bold 'inherit)
   (org-list-dt :foreground yellow)
   ;;;; show-paren
   ((show-paren-match &override) :foreground nil :background base4 :bold t)
   ((show-paren-mismatch &override) :foreground nil :background "red")
   ;;;; which-func
   (which-func :foreground cyan)
   ;;;; which-key
   (which-key-command-description-face :foreground fg)
   (which-key-group-description-face :foreground (doom-darken fg-alt 0.25))
   (which-key-local-map-description-face :foreground cyan)
   ;;;; undo-tree
   (undo-tree-visualizer-active-branch-face :foreground cyan)
   (undo-tree-visualizer-current-face :foreground yellow)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground red)
   (rainbow-delimiters-depth-2-face :foreground yellow)
   (rainbow-delimiters-depth-3-face :foreground cyan)
   (rainbow-delimiters-depth-4-face :foreground red)
   (rainbow-delimiters-depth-5-face :foreground yellow)
   (rainbow-delimiters-depth-6-face :foreground cyan)
   (rainbow-delimiters-depth-7-face :foreground red)
   ;;;; rjsx-mode
   (rjsx-tag :foreground cyan :weight 'semi-bold)
   (rjsx-text :foreground fg)
   (rjsx-attr :foreground violet)
   ;;;; solaire-mode
   (solaire-hl-line-face :background bg-alt2)
   ;;;; swiper
   (swiper-line-face :background bg-alt2)
   ;;;; web-mode
   (web-mode-html-tag-bracket-face :foreground blue)
   (web-mode-html-tag-face         :foreground cyan :weight 'semi-bold)
   (web-mode-html-attr-name-face   :foreground violet)
   (web-mode-json-key-face         :foreground green)
   (web-mode-json-context-face     :foreground cyan))

  ;; --- extra variables --------------------
  ;; ()
  )

;;; doom-everforest-light-hard-theme.el ends here
