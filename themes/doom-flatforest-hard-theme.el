;;; doom-flatforest-hard-hard-theme.el --- Everforest light with flatwhite-style highlighting -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Echinoidea <https://github.com/Echinoidea>
;; Maintainer:
;; Source: https://github.com/sainnhe/everforest
;;
;;; Commentary:
;; Everforest light hard theme with flatwhite-style colored backgrounds for syntax highlighting
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-flatforest-hard-hard-theme nil
  "Options for the `doom-flatforest-hard' theme."
  :group 'doom-themes)

(defcustom doom-flatforest-hard-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-flatforest-hard-hard-theme
  :type 'boolean)

(defcustom doom-flatforest-hard-no-highlight-variables nil
  "If non-nil, removes highlight on variable names"
  :group 'doom-flatforest-hard-hard-theme
  :type 'boolean)

(defcustom doom-flatforest-hard-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-flatforest-hard-hard-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-flatforest-hard
    "A minimal light syntax theme with Everforest colors and flatwhite-style highlighting"

  ;; name        default   256       16
  (
   ;; Everforest light hard palette
   (ef-bg-dim     '("#f2efdf" "white"   "white"  ))
   (ef-bg0        '("#fffbef" "white"   "white"  ))
   (ef-bg1        '("#f8f5e4" "white"   "white"  ))
   (ef-bg2        '("#f2efdf" "grey"    "grey"   ))
   (ef-bg3        '("#edeada" "grey"    "grey"   ))
   (ef-bg4        '("#e8e5d5" "grey"    "grey"   ))
   (ef-bg5        '("#bec5b2" "grey"    "grey"   ))
   (ef-bg-red     '("#ffe7de" "red"     "red"    ))
   (ef-bg-yellow  '("#fef2d5" "yellow"  "yellow" ))
   (ef-bg-blue    '("#ecf5ed" "blue"    "blue"   ))
   (ef-bg-purple  '("#fceced" "magenta" "magenta"))
   (ef-bg-visual  '("#f0f2d4" "cyan"    "cyan"   ))
   (ef-fg         '("#5c6a72" "black"   "black"  ))
   (ef-red        '("#f85552" "red"     "red"    ))
   (ef-yellow     '("#dfa000" "yellow"  "yellow" ))
   (ef-green      '("#8da101" "green"   "green"  ))
   (ef-blue       '("#3a94c5" "blue"    "blue"   ))
   (ef-purple     '("#df69ba" "magenta" "magenta"))
   (ef-aqua       '("#35a77c" "cyan"    "cyan"   ))
   (ef-orange     '("#f57d26" "orange"  "orange" ))
   (ef-grey0      '("#a6b0a0" "grey"    "grey"   ))
   (ef-grey1      '("#939f91" "grey"    "grey"   ))
   (ef-grey2      '("#829181" "grey"    "grey"   ))
   (ef-statusline1 '("#935259" "red"    "red"    ))
   (ef-statusline2 '("#708089" "grey"   "grey"   ))
   (ef-statusline3 '("#e66868" "red"    "red"    ))

   (ef-orange-text     ef-fg)
   (ef-orange-text-sec ef-statusline2)
   (ef-orange-blend    (doom-lighten ef-orange 0.7))

   (ef-red-text        ef-fg)
   (ef-red-text-sec    ef-statusline2)
   (ef-red-blend       ef-bg-red)

   (ef-green-text      ef-fg)
   (ef-green-text-sec  ef-statusline2)
   (ef-green-blend     ef-bg-blue)

   (ef-aqua-text       ef-fg)
   (ef-aqua-text-sec   ef-statusline2)
   (ef-aqua-blend      ef-bg-visual)

   (ef-blue-text       ef-fg)
   (ef-blue-text-sec   ef-statusline2)
   (ef-blue-blend      ef-bg-blue)

   (ef-purple-text     ef-fg)
   (ef-purple-text-sec ef-statusline2)
   (ef-purple-blend    ef-bg-purple)

   (ef-yellow-text     ef-fg)
   (ef-yellow-text-sec ef-statusline2)
   (ef-yellow-blend    ef-bg-yellow)

   (bg         `(,(car ef-bg0) nil       nil            ))
   (bg-alt     `(,(car ef-bg1) nil       nil            ))
   (base0      ef-bg0)
   (base1      ef-bg1)
   (base2      ef-bg2)
   (base3      ef-grey0)
   (base4      ef-grey1)
   (base5      ef-fg)
   (base6      '("#202328"       nil "brightblack"  ))
   (base7      '("#1c1f24"       nil "brightblack"  ))
   (base8      '("#1b2229"       nil "black"        ))
   (fg         `(,(car ef-fg) nil "black"        ))
   (fg-alt     `(,(car ef-grey1) nil "brightblack"  ))

   (grey       ef-grey0)
   (red        ef-red)
   (orange     ef-orange)
   (green      ef-green)
   (teal       ef-aqua)
   (yellow     ef-yellow)
   (blue       ef-blue)
   (dark-blue  (doom-darken ef-blue 0.2))
   (magenta    ef-purple)
   (violet     ef-purple)
   (cyan       ef-aqua)
   (dark-cyan  ef-blue)

   (ef--light-accent (doom-lighten ef-purple 0.85))

   ;; face categories -- required for all themes
   (highlight       ef-blue)
   (vertical-bar   (doom-darken ef-bg2 0.1))
   (selection      ef-bg-visual)
   (builtin        ef-purple)
   (comments       ef-grey0)
   (doc-comments   (doom-darken ef-grey0 0.15))
   (constants      ef-purple)
   (functions      ef-purple)
   (keywords       ef-red)
   (methods        ef-aqua)
   (operators      ef-blue)
   (type           ef-yellow)
   (strings        ef-green)
   (variables      (doom-darken ef-purple 0.36))
   (numbers        ef-yellow)
   (region         `(,(doom-darken (car bg-alt) 0.1) ,@(doom-darken (cdr base0) 0.3)))
   (error          ef-red)
   (warning        ef-yellow)
   (success        ef-green)
   (vc-modified    ef-yellow)
   (vc-added       ef-green)
   (vc-deleted     ef-red)

   ;; custom categories
   (-modeline-bright doom-flatforest-hard-brighter-modeline)
   (-no-highlight-variables doom-flatforest-hard-no-highlight-variables)
   (-modeline-pad
    (when doom-flatforest-hard-padded-modeline
      (if (integerp doom-flatforest-hard-padded-modeline) doom-flatforest-hard-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt ef-grey1)

   (modeline-bg
    (if -modeline-bright
        (doom-darken ef-bg2 0.05)
      ef-bg1))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken ef-bg2 0.1)
      ef-bg2))
   (modeline-bg-inactive (doom-darken ef-bg0 0.1))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base1))))


  ;;;; Base theme face overrides
  ((font-lock-builtin-face :inherit 'italic :foreground fg :extend t)
   ((font-lock-constant-face &override) :foreground ef-purple-text
    :background ef-purple-blend)
   ((font-lock-doc-face &override) :slant 'italic
    :foreground ef-green-text
    :background ef-bg0)
   (font-lock-type-face :foreground ef-yellow-text
                        :background ef-yellow-blend)
   (font-lock-function-name-face :foreground ef-purple-text
                                 :background ef-purple-blend)
   (font-lock-keyword-face :foreground ef-purple-text
                           :background ef-purple-blend)
   (font-lock-string-face :foreground ef-green-text
                          :background ef-green-blend)

   (font-lock-string-face :foreground ef-blue-text :background ef-blue-blend)

   (font-lock-variable-name-face
    :foreground (if -no-highlight-variables fg ef-blue-text)
    :background (if -no-highlight-variables bg ef-blue-blend))
   (font-lock-warning-face              :background ef-red-blend
                                        :foreground ef-red-text)
   (font-lock-negation-char-face        :inherit 'default)
   (font-lock-preprocessor-face         :inherit 'default)
   (font-lock-preprocessor-char-face    :inherit 'default)
   (font-lock-regexp-grouping-backslash :inherit 'default)
   (font-lock-regexp-grouping-construct :inherit 'default)

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground base5)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ((vertical-bar &override) :background base2 :foreground base2)

   ;;;; company
   (company-tooltip            :inherit 'tooltip)
   (company-tooltip-common                           :foreground highlight)
   (company-tooltip-search     :background highlight :foreground bg :distant-foreground fg)
   (company-tooltip-selection  :background selection)
   (company-tooltip-mouse      :background magenta   :foreground bg :distant-foreground fg)
   (company-tooltip-annotation                       :foreground violet)
   (company-scrollbar-bg       :inherit 'tooltip)
   (company-scrollbar-fg       :background highlight)
   (company-preview                                  :foreground comments)
   (company-preview-common     :background base3 :foreground highlight)
   (company-preview-search     :inherit 'company-tooltip-search)
   (company-template-field     :inherit 'match)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground keywords)
   (css-property             :foreground fg)
   (css-selector             :foreground ef-purple-text
                             :background ef-purple-blend)
   ;;;; dired
   (dired-directory :foreground cyan)
   (dired-marked :foreground yellow)
   (dired-symlink :foreground cyan)
   (dired-header :foreground cyan)
   ;;;; elixir-mode
   (elixir-atom-face :foreground ef-aqua-text
                     :background ef-aqua-blend)
   (elixir-attribute-face :foreground violet)
   ;;;; elm-mode
   (elm-function :foreground ef-aqua-text
                 :background ef-aqua-blend)
   (elm-keysword :foreground ef-red)
   (elm-type :foreground ef-blue-text
             :background ef-blue-blend)
   ;;;; enh-ruby-mode
   (enh-ruby-heredoc-delimiter-face :foreground ef-blue-text
                                    :background ef-blue-blend)
   (enh-ruby-op-face :foreground operators)
   (enh-ruby-regexp-delimiter-face :foreground ef-aqua-text
                                   :background ef-aqua-blend)
   (enh-ruby-regexp-face :foreground ef-aqua-text
                         :background ef-aqua-blend)
   (enh-ruby-string-delimiter-face :foreground strings)
   (erm-syn-errline :underline `(:style wave :color ,error))
   (erm-syn-warnline :underline `(:style wave :color ,warning))
   ;;;; flycheck
   (flycheck-error     :underline `(:style wave :color ,red))
   (flycheck-warning   :underline `(:style wave :color ,yellow))
   (flycheck-info      :underline `(:style wave :color ,blue))
   ;;;; git-gutter
   (git-gutter:modified :foreground vc-modified)
   (git-gutter:added    :foreground vc-added)
   (git-gutter:deleted  :foreground vc-deleted)
   ;;;; git-gutter+
   (git-gutter+-modified :foreground vc-modified :background nil)
   (git-gutter+-added    :foreground vc-added :background nil)
   (git-gutter+-deleted  :foreground vc-deleted :background nil)
   ;;;; git-gutter-fringe
   ((git-gutter-fr:modified &override) :foreground vc-modified)
   ((git-gutter-fr:added    &override) :foreground vc-added)
   ((git-gutter-fr:deleted  &override) :foreground vc-deleted)
   ;;;; haskell-mode
   (haskell-type-face :foreground ef-blue-text
                      :background ef-blue-blend)
   (haskell-constructor-face :foreground ef-purple-text
                             :background ef-purple-blend)
   (haskell-operator-face :foreground operators)
   (haskell-keyword-face :foreground keywords)
   ;;;; helm
   (helm-selection :inherit 'bold
                   :background selection
                   :distant-foreground bg
                   :extend t)
   ;;;; highlight-quoted
   (highlight-quoted-symbol :foreground dark-cyan)
   ;;;; highlight-symbol
   (highlight-symbol-face :background (doom-darken base2 0.03) :distant-foreground fg-alt)
   ;;;; highlight-thing
   (highlight-thing :background (doom-darken base2 0.03) :distant-foreground fg-alt)
   ;;;; hl-todo
   (hl-todo :foreground red :weight 'bold)
   ;;;; ivy
   (ivy-current-match :background ef-purple-blend
                      :distant-foreground bg-alt
                      :extend t)
   (ivy-minibuffer-match-face-1 :foreground ef-purple-text
                                :background ef-purple-blend)
   (ivy-minibuffer-match-face-2 :foreground ef-blue-text
                                :background ef-blue-blend
                                :weight 'bold)
   (ivy-minibuffer-match-face-3 :foreground ef-red-text
                                :background ef-red-blend
                                :weight 'bold)
   (ivy-minibuffer-match-face-4 :foreground ef-green-text
                                :background ef-green-blend
                                :weight 'bold)
   (ivy-minibuffer-match-highlight :foreground violet)
   (ivy-virtual :foreground fg)
   ;;;; js2-mode
   (js2-function-param :foreground ef-blue-text
                       :background ef-blue-blend)
   (js2-function-call :foreground ef-purple-text
                      :background ef-purple-blend)
   (js2-object-property :foreground fg)
   (js2-jsdoc-tag :foreground doc-comments)
   (js2-external-variable :foreground ef-red-text
                          :background ef-red-blend)
   ;;;; lsp-mode
   (lsp-face-highlight-read :background (doom-blend highlight bg 0.3) :foreground fg)
   (lsp-face-highlight-textual :inherit 'lsp-face-highlight-read)
   (lsp-face-highlight-write :inherit 'lsp-face-highlight-read)
   (lsp-headerline-breadcrumb-separator-face :foreground fg-alt)
   ;;;; magit
   (magit-bisect-bad        :foreground red)
   (magit-bisect-good       :foreground green)
   (magit-bisect-skip       :foreground orange)
   (magit-blame-date        :foreground ef-orange-text-sec)
   (magit-blame-heading     :foreground ef-orange-text-sec :background base3 :extend t)
   (magit-branch-current    :foreground ef-blue)
   (magit-branch-local      :foreground ef-aqua)
   (magit-branch-remote     :foreground green)
   (magit-cherry-equivalent :foreground violet)
   (magit-cherry-unmatched  :foreground cyan)
   (magit-diff-added             :foreground ef-green-text-sec
                                 :background ef-green-blend
                                 :extend t)
   (magit-diff-added-highlight   :foreground ef-green-text
                                 :background ef-green-blend
                                 :weight 'bold
                                 :extend t)
   (magit-diff-base              :foreground ef-yellow-text-sec
                                 :background ef-yellow-blend
                                 :extend t)
   (magit-diff-base-highlight    :foreground ef-yellow-text
                                 :background ef-yellow-blend
                                 :weight 'bold
                                 :extend t)
   (magit-diff-context           :foreground fg-alt :extend t)
   (magit-diff-context-highlight :background bg-alt
                                 :foreground fg-alt
                                 :extend t)
   (magit-diff-file-heading           :foreground fg
                                      :weight 'bold
                                      :extend t)
   (magit-diff-file-heading-selection :foreground magenta
                                      :background dark-blue
                                      :weight 'bold
                                      :extend t)
   (magit-diff-hunk-heading           :foreground ef-purple-text-sec
                                      :background ef-purple-blend
                                      :extend t)
   (magit-diff-hunk-heading-selection :foreground ef-purple-text-sec
                                      :background ef-purple-blend
                                      :extend t)
   (magit-diff-hunk-heading-highlight :foreground ef-purple-blend
                                      :background ef-purple-text-sec
                                      :weight 'bold
                                      :extend t)

   (magit-diff-removed                :foreground ef-red-text-sec
                                      :background ef-red-blend
                                      :extend t)
   (magit-diff-removed-highlight      :foreground ef-red-text
                                      :background ef-red-blend
                                      :weight 'bold
                                      :extend t)

   (magit-diff-lines-heading          :foreground yellow
                                      :background red
                                      :extend t)
   (magit-diffstat-added              :foreground ef-green)
   (magit-diffstat-removed            :foreground ef-red)
   (magit-dimmed                      :foreground comments)
   (magit-hash                        :foreground fg-alt)
   (magit-header-line :background ef-blue-blend
                      :foreground ef-blue-text
                      :weight 'bold
                      :box `(:line-width 3 :color ,ef-blue-blend))
   (magit-log-author :foreground ef-orange-text-sec)
   (magit-log-date   :foreground ef-blue-text-sec)
   (magit-log-graph  :foreground comments)
   (magit-process-ng :inherit 'error)
   (magit-process-ok :inherit 'success)
   (magit-reflog-amend :foreground magenta)
   (magit-reflog-checkout :foreground blue)
   (magit-reflog-cherry-pick :foreground green)
   (magit-reflog-commit :foreground green)
   (magit-reflog-merge :foreground green)
   (magit-reflog-other :foreground cyan)
   (magit-reflog-rebase :foreground magenta)
   (magit-reflog-remote :foreground cyan)
   (magit-reflog-reset :inherit 'error)
   (magit-refname :foreground comments)
   (magit-section-heading :foreground blue
                          :weight 'bold
                          :extend t)
   (magit-section-heading-selection :foreground orange
                                    :weight 'bold
                                    :extend t)
   (magit-section-highlight :inherit 'hl-line)
   (magit-sequence-drop :foreground red)
   (magit-sequence-head :foreground blue)
   (magit-sequence-part :foreground orange)
   (magit-sequence-stop :foreground green)
   (magit-signature-bad :inherit 'error)
   (magit-signature-error :inherit 'error)
   (magit-signature-expired :foreground orange)
   (magit-signature-good :inherit 'success)
   (magit-signature-revoked :foreground magenta)
   (magit-signature-untrusted :foreground yellow)
   (magit-tag :foreground yellow)
   (magit-filename :foreground violet)
   (magit-section-secondary-heading :foreground violet
                                    :weight 'bold
                                    :extend t)
   ;;;; makefile-*-mode
   (makefile-targets :foreground ef-purple-text
                     :background ef-purple-blend)
   ;;;; markdown-mode
   (markdown-header-face           :inherit 'bold
                                   :foreground ef-purple-text
                                   :background ef-purple-blend)
   (markdown-header-delimiter-face :inherit 'markdown-header-face)
   (markdown-metadata-key-face     :foreground ef-green-text
                                   :background ef-green-blend)
   (markdown-list-face             :foreground fg
                                   :inherit 'bold)
   (markdown-link-face             :foreground ef-blue-text
                                   :background ef-blue-blend)
   (markdown-url-face              :foreground ef-blue-text
                                   :background ef-blue-blend)
   (markdown-italic-face           :inherit 'italic
                                   :foreground fg)
   (markdown-bold-face             :inherit 'bold
                                   :foreground fg)
   (markdown-markup-face           :foreground fg
                                   :inherit 'bold)
   (markdown-blockquote-face       :inherit 'italic
                                   :foreground doc-comments)
   (markdown-pre-face              :foreground fg)
   (markdown-code-face             :background ef-orange-blend
                                   :foreground ef-orange-text
                                   :extend t)
   (markdown-reference-face        :foreground doc-comments)
   (markdown-inline-code-face      :inherit '(markdown-code-face markdown-pre-face)
                                   :extend nil)
   (markdown-html-attr-name-face     :inherit 'font-lock-variable-name-face)
   (markdown-html-attr-value-face    :inherit 'font-lock-string-face)
   (markdown-html-entity-face        :inherit 'font-lock-variable-name-face)
   (markdown-html-tag-delimiter-face :inherit 'markdown-markup-face)
   (markdown-html-tag-name-face      :inherit 'font-lock-keyword-face)
   ;;;; org-mode
   ((outline-1 &override) :foreground red)
   ((outline-2 &override) :foreground orange)
   (org-ellipsis :underline nil :background bg     :foreground red)
   ((org-block-begin-line &override)
    :background ef-red-blend
    :foreground ef-red-text
    :weight 'semi-bold)
   ((org-block &override)
    :background ef-red-blend
    :foreground ef-red-text)
   ((org-quote &override)
    :background ef-orange-blend
    :foreground ef-orange-text)
   ;;;; racket
   (racket-keyword-argument-face :foreground ef-orange-text
                                 :background ef-orange-blend)
   (racket-selfeval-face :foreground ef-aqua-text
                         :background ef-aqua-blend)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground ef-blue)
   (rainbow-delimiters-depth-2-face :foreground ef-purple)
   (rainbow-delimiters-depth-3-face :foreground ef-green)
   (rainbow-delimiters-depth-4-face :foreground ef-orange)
   (rainbow-delimiters-depth-5-face :foreground ef-aqua)
   (rainbow-delimiters-depth-6-face :foreground ef-red)
   (rainbow-delimiters-depth-7-face :foreground ef-green)
   (rainbow-delimiters-unmatched-face  :foreground red
                                       :weight 'bold
                                       :inverse-video t)
   (rainbow-delimiters-mismatched-face :inherit 'rainbow-delimiters-unmatched-face)
   ;;;; rjsx-mode
   (rjsx-tag  :background ef-purple-blend
              :foreground ef-purple-text)
   (rjsx-text :inherit 'default)
   (rjsx-tag-bracket-face :background bg
                          :foreground fg-alt)
   (rjsx-attr :background bg
              :foreground fg
              :inherit 'italic)
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
   (web-mode-current-element-highlight-face :background dark-blue
                                            :foreground bg)
   (web-mode-css-property-name-face :foreground fg
                                    :inherit 'italic)
   (web-mode-doctype-face           :background bg
                                    :foreground comments)
   (web-mode-html-tag-face          :background ef-purple-blend
                                    :foreground ef-purple-text)
   (web-mode-html-attr-name-face    :background bg
                                    :foreground fg
                                    :inherit 'italic)
   (web-mode-html-attr-value-face   :inherit 'font-lock-string-face)
   (web-mode-html-entity-face       :background ef-orange-blend
                                    :foreground ef-orange-text
                                    :inherit 'italic)
   (web-mode-block-control-face     :background bg
                                    :foreground fg)
   (web-mode-html-tag-bracket-face  :background bg
                                    :foreground fg-alt)
   (web-mode-symbol-face            :foreground ef-blue-text
                                    :background ef-blue-blend)
   (web-mode-string-face            :inherit 'font-lock-string-face)

   (web-mode-variable-name-face            :foreground ef-red-text
                                           :background ef-red-blend)

   (doom-dashboard-menu-title :foreground fg :background ef-bg1)
   (doom-dashboard-menu-desc :foreground fg :background ef-aqua-blend)

   ;;;; wgrep <built-in>
   (wgrep-face :background base1)
   ;;;; which-key
   (which-key-key-face                   :foreground ef-green-text-sec)
   (which-key-group-description-face     :foreground ef-purple-text-sec)
   (which-key-command-description-face   :foreground fg)
   (which-key-local-map-description-face :foreground ef-orange-text-sec)
   (which-key-separator-face             :background bg-alt :foreground comments)
   ;;;; whitespace
   ((whitespace-tab &override)         :background (if (not (default-value 'indent-tabs-mode)) base0 'unspecified))
   ((whitespace-indentation &override) :background (if (default-value 'indent-tabs-mode) base0 'unspecified)))

  ;;;; Base theme variable overrides-
  ()
  )

;;; doom-flatforest-hard-hard-theme.el ends here
