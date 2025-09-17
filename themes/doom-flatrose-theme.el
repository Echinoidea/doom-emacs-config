;;; doom-flatrose-theme.el --- inspired by Atom's Flatwhite Syntax theme with Rose Pine main colors -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Modified from original doom-flatwhite-theme.el
;; Author: JuneKelly <https://github.com/JuneKelly>
;; Maintainer: Echinoidea <https://github.com/Echinoidea>
;; Source: https://github.com/biletskyy/flatwhite-syntax
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-flatrose-theme nil
  "Options for the `doom-flatrose' theme."
  :group 'doom-themes)

(defcustom doom-flatrose-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-flatrose-theme
  :type 'boolean)

(defcustom doom-flatrose-no-highlight-variables nil
  "If non-nil, removes highlight on variable names"
  :group 'doom-flatrose-theme
  :type 'boolean)

(defcustom doom-fn-rp-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-flatrose-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-flatrose
    "A minimal dark syntax theme with Rose Pine main colors"

  ;; name        default   256       16
  (
   ;; Rose Pine main palette
   (rp-base       '("#191724" "black"   "black"  ))
   (rp-surface    '("#1f1d2e" "black"   "black"  ))
   (rp-overlay    '("#26233a" "grey"    "grey"   ))
   (rp-muted      '("#6e6a86" "grey"    "grey"   ))
   (rp-subtle     '("#908caa" "grey"    "grey"   ))
   (rp-text       '("#e0def4" "white"   "white"  ))
   (rp-love       '("#eb6f92" "red"     "red"    ))
   (rp-gold       '("#f6c177" "yellow"  "yellow" ))
   (rp-rose       '("#ebbcba" "cyan"    "cyan"   ))
   (rp-pine       '("#31748f" "blue"    "blue"   ))
   (rp-foam       '("#9ccfd8" "cyan"    "cyan"   ))
   (rp-iris       '("#c4a7e7" "magenta" "magenta"))
   (rp-leaf       '("#95b1ac" "green"   "green"  ))

   (rp-highlight-low  '("#21202e" "grey" "grey"))
   (rp-highlight-med  '("#403d52" "grey" "grey"))
   (rp-highlight-high '("#524f67" "grey" "grey"))

   ;; Adapted flatwhite for dark theme
   (fn-base1           rp-text)
   (fn-base2           rp-subtle)
   (fn-base3           rp-muted)
   (fn-base4           rp-overlay)
   (fn-base5           rp-highlight-low)
   (fn-base6           rp-surface)
   (fn-base7           rp-base)

   (fn-accent          rp-iris)

   (fn-orange-text     rp-base)
   (fn-orange-text-sec rp-subtle)
   (fn-orange          rp-gold)
   (fn-orange-blend    (doom-darken rp-gold 0.7))

   (fn-red-text        rp-base)
   (fn-red-text-sec    rp-subtle)
   (fn-red             rp-love)
   (fn-red-blend       (doom-darken rp-love 0.7))

   (fn-green-text      rp-base)
   (fn-green-text-sec  rp-subtle)
   (fn-green           rp-leaf)
   (fn-green-blend     (doom-darken rp-leaf 0.7))

   (fn-teal-text       rp-base)
   (fn-teal-text-sec   rp-subtle)
   (fn-teal            rp-foam)
   (fn-teal-blend      (doom-darken rp-foam 0.8))

   (fn-blue-text       rp-base)
   (fn-blue-text-sec   rp-subtle)
   (fn-blue            rp-pine)
   (fn-blue-blend      (doom-darken rp-pine 0.7))

   (fn-purple-text     rp-base)
   (fn-purple-text-sec rp-subtle)
   (fn-purple          rp-iris)
   (fn-purple-blend    (doom-darken rp-iris 0.7))

   (bg         `(,(car fn-base7) nil       nil            ))
   (bg-alt     `(,(car fn-base6) nil       nil            ))
   (base0      rp-surface)
   (base1      rp-highlight-low)
   (base2      rp-overlay)
   (base3      rp-muted)
   (base4      rp-subtle)
   (base5      rp-text)
   (base6      '("#202328"       nil "brightblack"  ))
   (base7      '("#1c1f24"       nil "brightblack"  ))
   (base8      '("#1b2229"       nil "black"        ))
   (fg         `(,(car rp-text) nil "white"        ))
   (fg-alt     `(,(car rp-subtle) nil "brightwhite"  ))

   (grey       base3)
   (red        rp-love)
   (orange     rp-gold)
   (green      rp-leaf)
   (teal       rp-foam)
   (yellow     rp-gold)
   (blue       rp-pine)
   (dark-blue  (doom-darken rp-pine 0.2))
   (magenta    rp-iris)
   (violet     rp-iris)
   (cyan       rp-rose)
   (dark-cyan  rp-foam)

   (fn--dark-accent (doom-darken rp-iris 0.85))

   ;; face categories -- required for all themes
   (highlight       rp-pine)
   (vertical-bar   (doom-lighten rp-overlay 0.1))
   (selection      rp-pine)
   (builtin        rp-iris)
   (comments       rp-muted)
   (doc-comments   (doom-lighten rp-muted 0.15))
   (constants      rp-iris)
   (functions      rp-iris)
   (keywords       rp-love)
   (methods        rp-foam)
   (operators      rp-pine)
   (type           rp-gold)
   (strings        rp-leaf)
   (variables      (doom-lighten rp-iris 0.36))
   (numbers        rp-gold)
   (region         `(,(doom-lighten (car bg-alt) 0.1) ,@(doom-lighten (cdr base0) 0.3)))
   (error          rp-love)
   (warning        rp-gold)
   (success        rp-leaf)
   (vc-modified    rp-gold)
   (vc-added       rp-leaf)
   (vc-deleted     rp-love)

   ;; custom categories
   (-modeline-bright doom-flatrose-brighter-modeline)
   (-no-highlight-variables doom-flatrose-no-highlight-variables)
   (-modeline-pad
    (when doom-fn-rp-padded-modeline
      (if (integerp doom-fn-rp-padded-modeline) doom-fn-rp-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt rp-subtle)

   (modeline-bg
    (if -modeline-bright
        (doom-lighten rp-overlay 0.05)
      rp-highlight-low))
   (modeline-bg-l
    (if -modeline-bright
        (doom-lighten rp-overlay 0.1)
      rp-overlay))
   (modeline-bg-inactive (doom-lighten rp-base 0.1))
   (modeline-bg-inactive-l `(,(doom-lighten (car bg-alt) 0.05) ,@(cdr base1))))


  ;;;; Base theme face overrides
  ((font-lock-builtin-face :inherit 'italic :foreground fg :extend t)
   ((font-lock-doc-face &override) :slant 'italic)
   (font-lock-type-face :inherit 'default)
   (font-lock-variable-name-face
    :foreground (if -no-highlight-variables fg fn-blue-text)
    :background (if -no-highlight-variables bg fn-blue-blend))
   (font-lock-warning-face              :background fn-red-blend
                                        :foreground fn-red-text)
   (font-lock-negation-char-face        :inherit 'default)
   (font-lock-preprocessor-face         :inherit 'default)
   (font-lock-preprocessor-char-face    :inherit 'default)
   (font-lock-regexp-grouping-backslash :inherit 'default)
   (font-lock-regexp-grouping-construct :inherit 'default)
   (font-lock-constant-face             :background fn-orange-blend
                                        :foreground fn-orange-text)
   (font-lock-function-name-face        :foreground fg
                                        :weight 'semi-bold)

   (font-lock-keyword-face              :background fn-purple-blend
                                        :foreground fn-purple-text)
   (font-lock-string-face               :background fn-green-blend
                                        :foreground fn-green-text )

   (lazy-highlight :background fn--dark-accent
                   :foreground fn-blue-text
                   :distant-foreground base0
                   :weight 'bold)

   ((line-number &override) :foreground (doom-darken base4 0.15))
   ((line-number-current-line &override) :foreground base8)

   (vertical-border :foreground fn-blue-blend)

   (mode-line
    :background modeline-bg
    :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive
    :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; centaur-tabs
   (centaur-tabs-unselected :background bg-alt :foreground base4)
   ;;;; swiper
   (swiper-line-face    :background fn--dark-accent
                        :foreground fn-blue-text)
   (swiper-match-face-1 :inherit 'unspecified
                        :background base0
                        :foreground fg)
   (swiper-background-match-face-1 :inherit 'unspecified
                                   :background base0
                                   :foreground fg-alt)
   (swiper-match-face-2 :inherit 'unspecified
                        :background fn-orange-blend
                        :foreground fn-orange-text
                        :weight 'bold)
   (swiper-background-match-face-2 :inherit 'unspecified
                                   :background fn-orange-blend
                                   :foreground fn-orange-text-sec
                                   :weight 'bold)
   (swiper-match-face-3 :inherit 'unspecified
                        :background fn-green-blend
                        :foreground fn-green-text
                        :weight 'bold)
   (swiper-background-match-face-3 :inherit 'unspecified
                                   :background fn-green-blend
                                   :foreground fn-green-text-sec
                                   :weight 'bold)
   (swiper-match-face-4 :inherit 'unspecified
                        :background fn-teal-blend
                        :foreground fn-teal-text
                        :weight 'bold)
   (swiper-background-match-face-4 :inherit 'unspecified
                                   :background fn-teal-blend
                                   :foreground fn-teal-text-sec
                                   :weight 'bold)
   ;;;; verticco
   (vertico-current :background fn-purple-blend
                    :box nil
                    :distant-foreground nil
                    :extend t)

   (vertico-group-title :background fn-blue-blend
                        :distant-foreground nil
                        :extend t)


   (vertico-posframe :background fn-base7
                     :distant-foreground nil
                     :extend t)

   (vertico-posframe-border :background rp-iris; or whatever color you prefer
                            :foreground rp-iris)

   ;;;; company
   (company-tooltip            :inherit 'tooltip)
   (company-tooltip-annotation            :foreground fn-purple-text-sec )
   (company-tooltip-annotation-selection  :foreground fn-purple-text )
   (company-tooltip-common                :foreground highlight
                                          :distant-foreground base0
                                          :weight 'bold)
   (company-tooltip-search     :background highlight
                               :foreground bg
                               :distant-foreground fg
                               :weight 'bold)
   (company-tooltip-search-selection :background fn-blue-blend)
   (company-tooltip-selection  :background fn--dark-accent
                               :weight 'bold)
   (company-tooltip-mouse      :background magenta
                               :foreground bg
                               :distant-foreground fg)
   (company-tooltip-annotation :foreground violet
                               :distant-foreground bg)
   (company-scrollbar-bg       :inherit 'tooltip)
   (company-scrollbar-fg       :background highlight)
   (company-preview            :foreground comments)
   (company-preview-common     :background base3
                               :foreground highlight)
   (company-preview-search     :inherit 'company-tooltip-search)
   (company-template-field     :inherit 'match)
   ;;;; clojure
   (clojure-keyword-face :foreground fn-orange-text
                         :background fn-orange-blend)
   ;;;; css-mode <built-in> / scss-mode
   (css-property             :foreground fg
                             :inherit 'italic)
   (css-proprietary-property :foreground fn-orange-text
                             :background fn-orange-blend)
   (css-selector             :foreground fn-purple-text
                             :background fn-purple-blend)
   ;;;; company-box
   (company-box-candidate :foreground fg)
   ;;;; doom-emacs
   (doom-dashboard-banner      :foreground comments)
   (doom-dashboard-menu-title  :foreground fn-purple-text-sec)
   (doom-dashboard-menu-desc   :foreground fn-green-text-sec)
   (doom-dashboard-footer-icon :foreground (doom-darken yellow 0.4))
   (doom-dashboard-loaded      :foreground fn-orange-text)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-path       :foreground fn-blue-text-sec
                                    :bold bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path )
   (doom-modeline-info              :foreground fn-green-text-sec)
   (doom-modeline-project-dir       :foreground fn-purple-text-sec)
   (doom-modeline-evil-insert-state :foreground fn-teal)
   ;;;; diff-mode
   (diff-removed :foreground red
                 :background fn-red-blend)
   ;;;; ediff <built-in>
   (ediff-current-diff-A        :foreground red
                                :background (doom-darken red 0.8))
   (ediff-current-diff-B        :foreground green
                                :background (doom-darken green 0.8))
   (ediff-current-diff-C        :foreground blue
                                :background (doom-darken blue 0.8))
   (ediff-current-diff-Ancestor :foreground teal
                                :background (doom-darken teal 0.8))
   ;;;; elixir
   (elixir-atom-face :foreground fn-blue-text
                     :background fn-blue-blend)
   (elixir-attribute-face :foreground fn-teal-text
                          :background fn-teal-blend)
   ;;;; fill column
   (hl-fill-column-face :foreground fg
                        :background fn--dark-accent)
   ;;;; git-commit
   (git-commit-summary :foreground fg)
   ;;;; highlight-numbers-mode
   (highlight-numbers-number :foreground fn-teal-text
                             :background fn-teal-blend)
   ;;;; highlight-quoted-mode
   (highlight-quoted-symbol :background fn-blue-blend
                            :foreground fn-blue-text)
   (highlight-quoted-quote  :foreground fn-teal-blend
                            :foreground fn-teal-text)
   ;;;; ivy
   (ivy-current-match :background fn-base5
                      :distant-foreground nil
                      :extend t)
   (ivy-minibuffer-match-face-1
    :background nil
    :foreground fg
    :weight 'light)
   (ivy-minibuffer-match-face-2
    :inherit 'ivy-minibuffer-match-face-1
    :foreground fn-orange-text
    :background fn-orange-blend
    :weight 'semi-bold)
   (ivy-minibuffer-match-face-3
    :inherit 'ivy-minibuffer-match-face-2
    :foreground fn-blue-text
    :background fn-blue-blend
    :weight 'semi-bold)
   (ivy-minibuffer-match-face-4
    :inherit 'ivy-minibuffer-match-face-2
    :foreground fn-green-text
    :background fn-green-blend
    :weight 'semi-bold)
   (ivy-minibuffer-match-highlight :foreground bg
                                   :background fn-purple-text-sec)
   (ivy-highlight-face :foreground fn-purple-text)
   (ivy-confirm-face :foreground success)
   (ivy-match-required-face :foreground error)
   (ivy-virtual :inherit 'italic :foreground doc-comments)
   (ivy-modified-buffer :inherit 'bold :foreground vc-modified)
   ;;;; ivy-posframe
   (ivy-posframe               :background base0)
   ;;;; js2-mode
   (js2-function-param    :foreground fg)
   (js2-function-call     :foreground fg )
   (js2-object-property   :foreground fg :inherit 'italic)
   (js2-jsdoc-tag         :foreground doc-comments)
   (js2-external-variable :foreground fg)
   ;;;; lsp-mode
   (lsp-ui-doc-background      :background base0)
   (lsp-face-highlight-read    :background (doom-blend red bg 0.3))
   (lsp-face-highlight-textual :inherit 'lsp-face-highlight-read)
   (lsp-face-highlight-write   :inherit 'lsp-face-highlight-read)
   (lsp-flycheck-info-unnecessary-face :background fn-base4 :foreground fn-teal)

   ;;;; nav-flash
   (nav-flash-face :background rp-rose :foreground rp-base )

   ;;;; magit
   (magit-bisect-bad        :background fn-red-blend
                            :foreground fn-red-text)
   (magit-bisect-good       :background fn-green-blend
                            :foreground fn-green-text)
   (magit-bisect-skip       :background fn-orange-blend
                            :foreground fn-orange-text)
   (magit-blame-date        :background fn-base4
                            :foreground fn-red-text)
   (magit-blame-heading     :background fn-base4
                            :foreground fn-orange-text)
   (magit-branch-current    :background bg-alt
                            :foreground fn-blue-text)
   (magit-branch-local      :background bg-alt
                            :foreground fn-teal-text)
   (magit-branch-remote     :background bg-alt
                            :foreground fn-green-text)
   (magit-cherry-equivalent :background fn-base7
                            :foreground fn-purple-text)
   (magit-cherry-unmatched  :background fn-base7
                            :foreground fn-teal-text)

   (magit-diff-added             :foreground fn-green-text-sec
                                 :background fn-green-blend
                                 :extend t)
   (magit-diff-added-highlight   :foreground fn-green-text
                                 :background fn-green-blend
                                 :weight 'bold :extend t)

   (magit-diff-base              :foreground fn-orange-text-sec
                                 :background fn-orange-blend
                                 :extend t)
   (magit-diff-base-highlight    :foreground fn-orange-text
                                 :background fn-orange-blend
                                 :weight 'bold
                                 :extend t)

   (magit-diff-context           :foreground (doom-lighten fg 0.4)
                                 :background bg
                                 :extend t)
   (magit-diff-context-highlight :foreground fg
                                 :background bg-alt
                                 :extend t)
   (magit-diff-file-heading           :foreground fn-purple-text-sec
                                      :background fn-purple-blend
                                      :weight 'bold
                                      :extend t)
   (magit-diff-file-heading-selection :foreground fn-purple-text
                                      :background fn-purple-blend
                                      :weight 'bold
                                      :extend t)
   (magit-diff-hunk-heading           :foreground fn-purple-text-sec
                                      :background fn-purple-blend
                                      :extend t)
   (magit-diff-hunk-heading-selection :foreground fn-purple-text-sec
                                      :background fn-purple-blend
                                      :extend t)
   (magit-diff-hunk-heading-highlight :foreground fn-purple-blend
                                      :background fn-purple-text-sec
                                      :weight 'bold
                                      :extend t)

   (magit-diff-removed                :foreground fn-red-text-sec
                                      :background fn-red-blend
                                      :extend t)
   (magit-diff-removed-highlight      :foreground fn-red-text
                                      :background fn-red-blend
                                      :weight 'bold
                                      :extend t)

   (magit-diff-lines-heading          :foreground yellow
                                      :background red
                                      :extend t)
   (magit-diffstat-added              :foreground fn-green)
   (magit-diffstat-removed            :foreground fn-red)
   (magit-dimmed                      :foreground comments)
   (magit-hash                        :foreground fg-alt)
   (magit-header-line :background fn-blue-blend
                      :foreground fn-blue-text
                      :weight 'bold
                      :box `(:line-width 3 :color ,fn-blue-blend))
   (magit-log-author :foreground fn-orange-text-sec)
   (magit-log-date   :foreground fn-blue-text-sec)
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
   (makefile-targets :foreground fn-purple-text
                     :background fn-purple-blend)
   ;;;; markdown-mode
   (markdown-header-face           :inherit 'bold
                                   :foreground fn-purple-text
                                   :background fn-purple-blend)
   (markdown-header-delimiter-face :inherit 'markdown-header-face)
   (markdown-metadata-key-face     :foreground fn-green-text
                                   :background fn-green-blend)
   (markdown-list-face             :foreground fg
                                   :inherit 'bold)
   (markdown-link-face             :foreground fn-blue-text
                                   :background fn-blue-blend)
   (markdown-url-face              :foreground fn-blue-text
                                   :background fn-blue-blend)
   (markdown-italic-face           :inherit 'italic
                                   :foreground fg)
   (markdown-bold-face             :inherit 'bold
                                   :foreground fg)
   (markdown-markup-face           :foreground fg
                                   :inherit 'bold)
   (markdown-blockquote-face       :inherit 'italic
                                   :foreground doc-comments)
   (markdown-pre-face              :foreground fg)
   (markdown-code-face             :background fn-orange-blend
                                   :foreground fn-orange-text
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
    :background fn-red-blend
    :foreground fn-red-text
    :weight 'semi-bold)
   ((org-block &override)
    :background fn-red-blend
    :foreground fn-red-text)
   ((org-quote &override)
    :background fn-orange-blend
    :foreground fn-orange-text)
   ;;;; racket
   (racket-keyword-argument-face :foreground fn-orange-text
                                 :background fn-orange-blend)
   (racket-selfeval-face :foreground fn-teal-text
                         :background fn-teal-blend)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground fn-blue)
   (rainbow-delimiters-depth-2-face :foreground fn-purple)
   (rainbow-delimiters-depth-3-face :foreground fn-green)
   (rainbow-delimiters-depth-4-face :foreground fn-orange)
   (rainbow-delimiters-depth-5-face :foreground fn-teal)
   (rainbow-delimiters-depth-6-face :foreground fn-red)
   (rainbow-delimiters-depth-7-face :foreground fn-green)
   (rainbow-delimiters-unmatched-face  :foreground red
                                       :weight 'bold
                                       :inverse-video t)
   (rainbow-delimiters-mismatched-face :inherit 'rainbow-delimiters-unmatched-face)
   ;;;; rjsx-mode
   (rjsx-tag  :background fn-purple-blend
              :foreground fn-purple-text)
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
   (web-mode-html-tag-face          :background fn-purple-blend
                                    :foreground fn-purple-text)
   (web-mode-html-attr-name-face    :background bg
                                    :foreground fg
                                    :inherit 'italic)
   (web-mode-html-attr-value-face   :inherit 'font-lock-string-face)
   (web-mode-html-entity-face       :background fn-orange-blend
                                    :foreground fn-orange-text
                                    :inherit 'italic)
   (web-mode-block-control-face     :background bg
                                    :foreground fn-base1)
   (web-mode-html-tag-bracket-face  :background bg
                                    :foreground fg-alt)
   (web-mode-symbol-face            :foreground fn-blue-text
                                    :background fn-blue-blend)
   (web-mode-string-face            :inherit 'font-lock-string-face)

   (web-mode-variable-name-face            :foreground fn-red-text
                                           :background fn-red-blend)
   ;;;; wgrep <built-in>
   (wgrep-face :background base1)
   ;;;; which-key
   (which-key-key-face                   :foreground fn-green-text-sec)
   (which-key-group-description-face     :foreground fn-purple-text-sec)
   (which-key-command-description-face   :foreground fg)
   (which-key-local-map-description-face :foreground fn-orange-text-sec)
   (which-key-separator-face             :background bg-alt :foreground comments)
   ;;;; whitespace
   ((whitespace-tab &override)         :background (if (not (default-value 'indent-tabs-mode)) base0 'unspecified))
   ((whitespace-indentation &override) :background (if (default-value 'indent-tabs-mode) base0 'unspecified)))

  ;;;; Base theme variable overrides-
  ()


  )

;;; doom-flatrose-theme.el ends here
