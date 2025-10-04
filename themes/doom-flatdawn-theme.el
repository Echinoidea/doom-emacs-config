

;;; doom-flatdawn-theme.el --- inspired by Atom's Flatwhite Syntax theme with Rose Pine Dawn colors -*- lexical-binding: t; no-byte-compile: t; -*-
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

(defgroup doom-flatdawn-theme nil
  "Options for the `doom-flatdawn' theme."
  :group 'doom-themes)

(defcustom doom-flatdawn-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-flatdawn-theme
  :type 'boolean)

(defcustom doom-flatdawn-no-highlight-variables nil
  "If non-nil, removes highlight on variable names"
  :group 'doom-flatdawn-theme
  :type 'boolean)

(defcustom doom-fd-rp-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-flatdawn-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-flatdawn
    "A minimal light syntax theme with Rose Pine Dawn colors"

  ;; name        default   256       16
  (
   ;; Rose Pine Dawn palette
   (rp-base       '("#faf4ed" "white"   "white"  ))
   (rp-surface    '("#fffaf3" "white"   "white"  ))
   (rp-overlay    '("#f2e9e1" "grey"    "grey"   ))
   (rp-muted      '("#9893a5" "grey"    "grey"   ))
   (rp-subtle     '("#797593" "grey"    "grey"   ))
   (rp-text       '("#464261" "black"   "black"  ))
   (rp-love       '("#b4637a" "red"     "red"    ))
   (rp-gold       '("#ea9d34" "yellow"  "yellow" ))
   (rp-rose       '("#d7827e" "cyan"    "cyan"   ))
   (rp-pine       '("#286983" "blue"    "blue"   ))
   (rp-foam       '("#56949f" "cyan"    "cyan"   ))
   (rp-iris       '("#907aa9" "magenta" "magenta"))
   (rp-leaf       '("#6d8f89" "green"   "green"  ))

   (rp-highlight-low  '("#f4ede8" "grey" "grey"))
   (rp-highlight-med  '("#dfdad9" "grey" "grey"))
   (rp-highlight-high '("#cecacd" "grey" "grey"))

   ;; Adapted flatwhite
   (fd-base1           rp-text)
   (fd-base2           rp-subtle)
   (fd-base3           rp-muted)
   (fd-base4           rp-overlay)
   (fd-base5           rp-highlight-low)
   (fd-base6           rp-surface)
   (fd-base7           rp-base)

   (fd-accent          rp-iris)

   (fd-orange-text     rp-text)
   (fd-orange-text-sec rp-subtle)
   (fd-orange          rp-gold)
   (fd-orange-blend    (doom-lighten rp-gold 0.7))

   (fd-red-text        rp-text)
   (fd-red-text-sec    rp-subtle)
   (fd-red             rp-love)
   (fd-red-blend       (doom-lighten rp-love 0.7))

   (fd-green-text      rp-text)
   (fd-green-text-sec  rp-subtle)
   (fd-green           rp-leaf)
   (fd-green-blend     (doom-lighten rp-leaf 0.7))

   (fd-teal-text       rp-text)
   (fd-teal-text-sec   rp-subtle)
   (fd-teal            rp-foam)
   (fd-teal-blend      (doom-lighten rp-foam 0.8))

   (fd-blue-text       rp-text)
   (fd-blue-text-sec   rp-subtle)
   (fd-blue            rp-pine)
   (fd-blue-blend      (doom-lighten rp-pine 0.7))

   (fd-purple-text     rp-text)
   (fd-purple-text-sec rp-subtle)
   (fd-purple          rp-iris)
   (fd-purple-blend    (doom-lighten rp-iris 0.7))

   (bg         `(,(car fd-base6) nil       nil            ))
   (bg-alt     `(,(car fd-base7) nil       nil            ))
   (base0      rp-surface)
   (base1      rp-highlight-low)
   (base2      rp-overlay)
   (base3      rp-muted)
   (base4      rp-subtle)
   (base5      rp-text)
   (base6      '("#202328"       nil "brightblack"  ))
   (base7      '("#1c1f24"       nil "brightblack"  ))
   (base8      '("#1b2229"       nil "black"        ))
   (fg         `(,(car rp-text) nil "black"        ))
   (fg-alt     `(,(car rp-subtle) nil "brightblack"  ))

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

   (fd--light-accent (doom-lighten rp-iris 0.85))

   ;; face categories -- required for all themes
   (highlight       rp-pine)
   (vertical-bar   (doom-darken rp-overlay 0.1))
   (selection      rp-pine)
   (builtin        rp-iris)
   (comments       rp-muted)
   (doc-comments   (doom-darken rp-muted 0.15))
   (constants      rp-iris)
   (functions      rp-iris)
   (keywords       rp-love)
   (methods        rp-foam)
   (operators      rp-pine)
   (type           rp-gold)
   (strings        rp-leaf)
   (variables      (doom-darken rp-iris 0.36))
   (numbers        rp-gold)
   (region         `(,(doom-darken (car bg-alt) 0.1) ,@(doom-darken (cdr base0) 0.3)))
   (error          rp-love)
   (warning        rp-gold)
   (success        rp-leaf)
   (vc-modified    rp-gold)
   (vc-added       rp-leaf)
   (vc-deleted     rp-love)

   ;; custom categories
   (-modeline-bright doom-flatdawn-brighter-modeline)
   (-no-highlight-variables doom-flatdawn-no-highlight-variables)
   (-modeline-pad
    (when doom-fd-rp-padded-modeline
      (if (integerp doom-fd-rp-padded-modeline) doom-fd-rp-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt rp-subtle)

   (modeline-bg
    (if -modeline-bright
        (doom-darken rp-overlay 0.05)
      rp-highlight-low))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken rp-overlay 0.1)
      rp-overlay))
   (modeline-bg-inactive (doom-darken rp-base 0.1))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base1))))


  ;;;; Base theme face overrides
  ((font-lock-builtin-face :inherit 'italic :foreground fg :extend t)
   ((font-lock-doc-face &override) :slant 'italic)
   (font-lock-type-face :inherit 'default)
   (font-lock-variable-name-face
    :foreground (if -no-highlight-variables fg fd-blue-text)
    :background (if -no-highlight-variables bg fd-blue-blend))
   (font-lock-warning-face              :background fd-red-blend
                                        :foreground fd-red-text)
   (font-lock-negation-char-face        :inherit 'default)
   (font-lock-preprocessor-face         :inherit 'default)
   (font-lock-preprocessor-char-face    :inherit 'default)
   (font-lock-regexp-grouping-backslash :inherit 'default)
   (font-lock-regexp-grouping-construct :inherit 'default)
   (font-lock-constant-face             :background fd-orange-blend
                                        :foreground fd-orange-text)
   (font-lock-function-name-face        :foreground fg
                                        :weight 'semi-bold)

   (font-lock-keyword-face              :background fd-purple-blend
                                        :foreground fd-purple-text)
   (font-lock-string-face               :background fd-green-blend
                                        :foreground fd-green-text )

   (lazy-highlight :background fd--light-accent
                   :foreground fd-blue-text
                   :distant-foreground base0
                   :weight 'bold)

   ((line-number &override) :foreground (doom-lighten base4 0.15))
   ((line-number-current-line &override) :foreground base8)

   (vertical-border :foreground fd-blue-blend)

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
   (swiper-line-face    :background fd--light-accent
                        :foreground fd-blue-text)
   (swiper-match-face-1 :inherit 'unspecified
                        :background base0
                        :foreground fg)
   (swiper-background-match-face-1 :inherit 'unspecified
                                   :background base0
                                   :foreground fg-alt)
   (swiper-match-face-2 :inherit 'unspecified
                        :background fd-orange-blend
                        :foreground fd-orange-text
                        :weight 'bold)
   (swiper-background-match-face-2 :inherit 'unspecified
                                   :background fd-orange-blend
                                   :foreground fd-orange-text-sec
                                   :weight 'bold)
   (swiper-match-face-3 :inherit 'unspecified
                        :background fd-green-blend
                        :foreground fd-green-text
                        :weight 'bold)
   (swiper-background-match-face-3 :inherit 'unspecified
                                   :background fd-green-blend
                                   :foreground fd-green-text-sec
                                   :weight 'bold)
   (swiper-match-face-4 :inherit 'unspecified
                        :background fd-teal-blend
                        :foreground fd-teal-text
                        :weight 'bold)
   (swiper-background-match-face-4 :inherit 'unspecified
                                   :background fd-teal-blend
                                   :foreground fd-teal-text-sec
                                   :weight 'bold)
   ;;;; verticco
   (vertico-current :background fd-purple-blend
                    :box nil
                    :distant-foreground nil
                    :extend t)

   (vertico-group-title :background fd-blue-blend
                        :distant-foreground nil
                        :extend t)


   (vertico-posframe :background fd-base7
                     :distant-foreground nil
                     :extend t)

   (vertico-posframe-border :background rp-iris; or whatever color you prefer
                            :foreground rp-iris)

   ;;;; company
   (company-tooltip            :inherit 'tooltip)
   (company-tooltip-annotation            :foreground fd-purple-text-sec )
   (company-tooltip-annotation-selection  :foreground fd-purple-text )
   (company-tooltip-common                :foreground highlight
                                          :distant-foreground base0
                                          :weight 'bold)
   (company-tooltip-search     :background highlight
                               :foreground bg
                               :distant-foreground fg
                               :weight 'bold)
   (company-tooltip-search-selection :background fd-blue-blend)
   (company-tooltip-selection  :background fd--light-accent
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
   (clojure-keyword-face :foreground fd-orange-text
                         :background fd-orange-blend)
   ;;;; css-mode <built-in> / scss-mode
   (css-property             :foreground fg
                             :inherit 'italic)
   (css-proprietary-property :foreground fd-orange-text
                             :background fd-orange-blend)
   (css-selector             :foreground fd-purple-text
                             :background fd-purple-blend)
   ;;;; company-box
   (company-box-candidate :foreground fg)
   ;;;; doom-emacs
   (doom-dashboard-banner      :foreground comments)
   (doom-dashboard-menu-title  :foreground fd-purple-text-sec)
   (doom-dashboard-menu-desc   :foreground fd-green-text-sec)
   (doom-dashboard-footer-icon :foreground (doom-darken yellow 0.4))
   (doom-dashboard-loaded      :foreground fd-orange-text)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-path       :foreground fd-blue-text-sec
                                    :bold bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path )
   (doom-modeline-info              :foreground fd-green-text-sec)
   (doom-modeline-project-dir       :foreground fd-purple-text-sec)
   (doom-modeline-evil-insert-state :foreground fd-teal)
   ;;;; diff-mode
   (diff-removed :foreground red
                 :background fd-red-blend)
   ;;;; ediff <built-in>
   (ediff-current-diff-A        :foreground red
                                :background (doom-lighten red 0.8))
   (ediff-current-diff-B        :foreground green
                                :background (doom-lighten green 0.8))
   (ediff-current-diff-C        :foreground blue
                                :background (doom-lighten blue 0.8))
   (ediff-current-diff-Ancestor :foreground teal
                                :background (doom-lighten teal 0.8))
   ;;;; elixir
   (elixir-atom-face :foreground fd-blue-text
                     :background fd-blue-blend)
   (elixir-attribute-face :foreground fd-teal-text
                          :background fd-teal-blend)
   ;;;; fill column
   (hl-fill-column-face :foreground fg
                        :background fd--light-accent)
   ;;;; git-commit
   (git-commit-summary :foreground fg)
   ;;;; highlight-numbers-mode
   (highlight-numbers-number :foreground fd-teal-text
                             :background fd-teal-blend)
   ;;;; highlight-quoted-mode
   (highlight-quoted-symbol :background fd-blue-blend
                            :foreground fd-blue-text)
   (highlight-quoted-quote  :foreground fd-teal-blend
                            :foreground fd-teal-text)
   ;;;; ivy
   (ivy-current-match :background fd-base5
                      :distant-foreground nil
                      :extend t)
   (ivy-minibuffer-match-face-1
    :background nil
    :foreground fg
    :weight 'light)
   (ivy-minibuffer-match-face-2
    :inherit 'ivy-minibuffer-match-face-1
    :foreground fd-orange-text
    :background fd-orange-blend
    :weight 'semi-bold)
   (ivy-minibuffer-match-face-3
    :inherit 'ivy-minibuffer-match-face-2
    :foreground fd-blue-text
    :background fd-blue-blend
    :weight 'semi-bold)
   (ivy-minibuffer-match-face-4
    :inherit 'ivy-minibuffer-match-face-2
    :foreground fd-green-text
    :background fd-green-blend
    :weight 'semi-bold)
   (ivy-minibuffer-match-highlight :foreground bg
                                   :background fd-purple-text-sec)
   (ivy-highlight-face :foreground fd-purple-text)
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
   (lsp-flycheck-info-unnecessary-face :background fd-base4 :foreground rp-base)

   ;;;; nav-flash
   (nav-flash-face :background rp-rose :foreground rp-base )

   ;;;; magit
   (magit-bisect-bad        :background fd-red-blend
                            :foreground fd-red-text)
   (magit-bisect-good       :background fd-green-blend
                            :foreground fd-green-text)
   (magit-bisect-skip       :background fd-orange-blend
                            :foreground fd-orange-text)
   (magit-blame-date        :background fd-base4
                            :foreground fd-red-text)
   (magit-blame-heading     :background fd-base4
                            :foreground fd-orange-text)
   (magit-branch-current    :background bg-alt
                            :foreground fd-blue-text)
   (magit-branch-local      :background bg-alt
                            :foreground fd-teal-text)
   (magit-branch-remote     :background bg-alt
                            :foreground fd-green-text)
   (magit-cherry-equivalent :background fd-base7
                            :foreground fd-purple-text)
   (magit-cherry-unmatched  :background fd-base7
                            :foreground fd-teal-text)

   (magit-diff-added             :foreground fd-green-text-sec
                                 :background fd-green-blend
                                 :extend t)
   (magit-diff-added-highlight   :foreground fd-green-text
                                 :background fd-green-blend
                                 :weight 'bold :extend t)

   (magit-diff-base              :foreground fd-orange-text-sec
                                 :background fd-orange-blend
                                 :extend t)
   (magit-diff-base-highlight    :foreground fd-orange-text
                                 :background fd-orange-blend
                                 :weight 'bold
                                 :extend t)

   (magit-diff-context           :foreground (doom-darken fg 0.4)
                                 :background bg
                                 :extend t)
   (magit-diff-context-highlight :foreground fg
                                 :background bg-alt
                                 :extend t)
   (magit-diff-file-heading           :foreground fd-purple-text-sec
                                      :background fd-purple-blend
                                      :weight 'bold
                                      :extend t)
   (magit-diff-file-heading-selection :foreground fd-purple-text
                                      :background fd-purple-blend
                                      :weight 'bold
                                      :extend t)
   (magit-diff-hunk-heading           :foreground fd-purple-text-sec
                                      :background fd-purple-blend
                                      :extend t)
   (magit-diff-hunk-heading-selection :foreground fd-purple-text-sec
                                      :background fd-purple-blend
                                      :extend t)
   (magit-diff-hunk-heading-highlight :foreground fd-purple-blend
                                      :background fd-purple-text-sec
                                      :weight 'bold
                                      :extend t)

   (magit-diff-removed                :foreground fd-red-text-sec
                                      :background fd-red-blend
                                      :extend t)
   (magit-diff-removed-highlight      :foreground fd-red-text
                                      :background fd-red-blend
                                      :weight 'bold
                                      :extend t)

   (magit-diff-lines-heading          :foreground yellow
                                      :background red
                                      :extend t)
   (magit-diffstat-added              :foreground fd-green)
   (magit-diffstat-removed            :foreground fd-red)
   (magit-dimmed                      :foreground comments)
   (magit-hash                        :foreground fg-alt)
   (magit-header-line :background fd-blue-blend
                      :foreground fd-blue-text
                      :weight 'bold
                      :box `(:line-width 3 :color ,fd-blue-blend))
   (magit-log-author :foreground fd-orange-text-sec)
   (magit-log-date   :foreground fd-blue-text-sec)
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
   (makefile-targets :foreground fd-purple-text
                     :background fd-purple-blend)
   ;;;; markdown-mode
   (markdown-header-face           :inherit 'bold
                                   :foreground fd-purple-text
                                   :background fd-purple-blend)
   (markdown-header-delimiter-face :inherit 'markdown-header-face)
   (markdown-metadata-key-face     :foreground fd-green-text
                                   :background fd-green-blend)
   (markdown-list-face             :foreground fg
                                   :inherit 'bold)
   (markdown-link-face             :foreground fd-blue-text
                                   :background fd-blue-blend)
   (markdown-url-face              :foreground fd-blue-text
                                   :background fd-blue-blend)
   (markdown-italic-face           :inherit 'italic
                                   :foreground fg)
   (markdown-bold-face             :inherit 'bold
                                   :foreground fg)
   (markdown-markup-face           :foreground fg
                                   :inherit 'bold)
   (markdown-blockquote-face       :inherit 'italic
                                   :foreground doc-comments)
   (markdown-pre-face              :foreground fg)
   (markdown-code-face             :background fd-orange-blend
                                   :foreground fd-orange-text
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
    :background fd-red-blend
    :foreground fd-red-text
    :weight 'semi-bold)
   ((org-block &override)
    :background fd-red-blend
    :foreground fd-red-text)
   ((org-quote &override)
    :background fd-orange-blend
    :foreground fd-orange-text)
   ;;;; racket
   (racket-keyword-argument-face :foreground fd-orange-text
                                 :background fd-orange-blend)
   (racket-selfeval-face :foreground fd-teal-text
                         :background fd-teal-blend)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground fd-blue)
   (rainbow-delimiters-depth-2-face :foreground fd-purple)
   (rainbow-delimiters-depth-3-face :foreground fd-green)
   (rainbow-delimiters-depth-4-face :foreground fd-orange)
   (rainbow-delimiters-depth-5-face :foreground fd-teal)
   (rainbow-delimiters-depth-6-face :foreground fd-red)
   (rainbow-delimiters-depth-7-face :foreground fd-green)
   (rainbow-delimiters-unmatched-face  :foreground red
                                       :weight 'bold
                                       :inverse-video t)
   (rainbow-delimiters-mismatched-face :inherit 'rainbow-delimiters-unmatched-face)
   ;;;; rjsx-mode
   (rjsx-tag  :background fd-purple-blend
              :foreground fd-purple-text)
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
   (web-mode-html-tag-face          :background fd-purple-blend
                                    :foreground fd-purple-text)
   (web-mode-html-attr-name-face    :background bg
                                    :foreground fg
                                    :inherit 'italic)
   (web-mode-html-attr-value-face   :inherit 'font-lock-string-face)
   (web-mode-html-entity-face       :background fd-orange-blend
                                    :foreground fd-orange-text
                                    :inherit 'italic)
   (web-mode-block-control-face     :background bg
                                    :foreground fd-base1)
   (web-mode-html-tag-bracket-face  :background bg
                                    :foreground fg-alt)
   (web-mode-symbol-face            :foreground fd-blue-text
                                    :background fd-blue-blend)
   (web-mode-string-face            :inherit 'font-lock-string-face)

   (web-mode-variable-name-face            :foreground fd-red-text
                                           :background fd-red-blend)
   ;;;; wgrep <built-in>
   (wgrep-face :background base1)
   ;;;; which-key
   (which-key-key-face                   :foreground fd-green-text-sec)
   (which-key-group-description-face     :foreground fd-purple-text-sec)
   (which-key-command-description-face   :foreground fg)
   (which-key-local-map-description-face :foreground fd-orange-text-sec)
   (which-key-separator-face             :background bg-alt :foreground comments)
   ;;;; whitespace
   ((whitespace-tab &override)         :background (if (not (default-value 'indent-tabs-mode)) base0 'unspecified))
   ((whitespace-indentation &override) :background (if (default-value 'indent-tabs-mode) base0 'unspecified)))

  ;;;; Base theme variable overrides-
  ()


  )

;;; doom-flatdawn-theme.el ends here
