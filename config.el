;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "ProggyClean Nerd Font Mono" :size 14))
;; (setq doom-font (font-spec :family "Kirsch Nerd Font Mono" :size 20 ))
;; (setq doom-font (font-spec :family "CozetteCrossedSevenVector" :size 20 ))
;; (setq doom-font (font-spec :family "ZedMono Nerd Font" :size 12))
;; (setq doom-font (font-spec :family "Hack Nerd Font Propo" :size 12))
(setq doom-font (font-spec :family "GoMono Nerd Font" :size 12))

;; (setq doom-font (font-spec :family "" :size 11 :weight 'light))
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:


;; Favorite themes:
;; modus-operandi-tinted
;; purple-haze
;; naga-blue
;; tron-legacy
;; sus-colors (Makes heading font sizes too big though)
;; srcery (gruvbox but kinda nicer)
;; phoenix-dark-pink
;; ef-winter
;; tao-yin
;; ef-spring
;; ef-symbiosis
;; ef-summer
;; ef-reverie
;; ef-owl
;; ef-night
;; ef-dark
;; ef-maris-dark
;; ef-kassio
;; ef-elea-dark
;; ef-eagle
;; ef-duo-dark
;; ef-dream
;; ef-day
;; ef-cyprus
;; ef-cherie
;; ef-bio
;; ef-autumn
;; ef-arbutus
;; wilson
;; ritchie (light and blue)
;; afternoon
;; jazz (really muted gruvboxy)
;; soothe (default, darker, and obsidian )(keywords are highlighted softly which is interesting)
;; organic-green (w98 vibes with light green)
;; doom-winter-is-coming-light (nice bright snowy colors, good on shitty monitor)
;; doom-wilmersdorf (kinda like nord but evil)
;; doom-tomorrow-night (very profesional and slightly muted)
;; doom-tomorrow-day (good pure white light theme)
;; doom-sourcerer (nice greens in Rust, nice bg)
;; doom-rouge (good, slightly blue, good on bad monitor)
;; doom-plain-dark and plain, compare with tao-yin and tao-yang
;; doom-pine (really green and gruvboxy)
;; doom-miramare
;; doom-meltbus (underline instead of cursorline highlight)
;; doom-lantern (muted red)
;; doom-ir-black (solid black theme)
;; doom-flatwhite (certain keywords and strings are highlighted which is actually really nice on a light theme)
;; doom-earl-grey
;; seagreenless (mostly monochrome seagreen, looks nice)
;; nordless (nord but less)
;; nofrills-darkless (purple, highlighted strings)
;; einkless (e-ink style :D)
;; darkless (like tao-yin but with string highlighting)
;; broceliande (neon teal 'less' theme with highlighted strings)

(setq doom-theme 'modus-operandi)

;; Font
;; (setq doom-font (font-spec :family "Aporetic Serif Mono" :size 10))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(after! vterm
  (setq vterm-shell "/sbin/fish"))

;; Themeing
                                        ;(defvar my/theme-file (expand-file-name "last-theme.el" doom-user-dir)
                                        ;  "File to store the last used theme.")
                                        ;
;;; Function to save current theme
                                        ;(defun my/save-current-theme ()
                                        ;  "Save the current doom-theme as the last used theme."
                                        ;  (when (and doom-theme (not (eq doom-theme 'user)))
                                        ;    (with-temp-file my/theme-file
                                        ;      (insert (format "(setq doom-theme '%s)" doom-theme)))))
                                        ;
;;; Add our save function to doom-load-theme-hook
                                        ;(add-hook 'doom-load-theme-hook #'my/save-current-theme)
                                        ;
;;; Load the saved theme file on startup if it exists
                                        ;(when (file-exists-p my/theme-file)
                                        ;  (load my/theme-file nil t))

;; Evil mode
(after! evil
  ;; Redefine 'o' and 'O' to not enter insert mode
  (map! :map evil-normal-state-map
        "o" (cmd! (evil-open-below 1) (evil-normal-state))
        "O" (cmd! (evil-open-above 1) (evil-normal-state)))
  )

;; Org mode
(defun my/org-do-demote ()
  "Make `org-do-demote` interactive so it works on selected region."
  (interactive)
  (org-do-demote))

(defun my/org-do-promote ()
  "Make `org-do-promote` interactive so it works on selected region."
  (interactive)
  (org-do-promote))

(setq org-directory "~/org/")

(after! org
  (setq org-agenda-files '("~/code/" "~/org/" "~/org/roam/daily"))
  (setq org-log-done 'time)

  ;; Demote highlighted headings
  (map! :map org-mode-map
        :localleader
        :prefix "s"
        :desc "Demote heading" ">" #'my/org-do-demote)

  ;; Promote highlighted headings
  (map! :map org-mode-map
        :localleader
        :prefix "s"
        :desc "Promote heading" "<" #'my/org-do-promote)
  )

(after! org-modern
  (global-org-modern-mode))

;; Org mode - PDF
(save-place-mode 1)


(after! org-roam
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("m" "Mood entry" plain
           "%?"
           :if-new (file+head "moods/%<%Y-%m-%d %H-%M>.org" "#+title: Mood log %<%Y-%m-%d %H:%M>\n\n* Mood\n\n* Context")
           :unnarrowed t
           :prepend t
           :node-property "MOOD" "%^{Mood|Happy|Sad|Neutral|Angry|Anxious|Excited}"
           :immediate-finish t))))

;; Configure org-mode to use pdf-tools for PDFs
;; (after! org
;;   (push '("\\.pdf\\'" . pdf-view-mode) org-file-apps)
;;   (add-to-list 'org-file-apps '("\\.pdf::\\([0-9]+\\)\\'" . pdf-view-mode))
;;   (add-to-list 'org-file-apps '("\\.pdf::\\([0-9]+\\)\\'" . pdf-view-mode)))

;; GPTel
(use-package! gptel
  :config
  (require 'auth-source)
  (let ((auth-token (auth-source-pick-first-password :host "api.openai.com" :user "apikey")))
    (when auth-token
      (setq gptel-api-key auth-token))))

(after! gptel
  (setq gptel-model 'gpt-4o))

(after! gptel
  (map! :leader
        :prefix "l"
        :desc "GPTel Open Buffer" "b" #'gptel
        :desc "GPTel Menu" "m" #'gptel-mode
        :desc "GPTel Add" "a" #'gptel-add
        :desc "GPTel Add File" "f" #'gptel-add-file
        :desc "GPTel Rewrite" "r" #'gptel-rewrite
        ))


;; LSP
(after! rustic
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-format-on-save t)
  (setq rustic-format-display-buffer nil))


(setq lsp-clients-typescript-server "/sbin/typescript-language-server")
(setq lsp-clients-typescript-server-args '("--stdio" "--tsserver-path" "/sbin/tsserver"))


;; (use-package! lsp-tailwindcss :after lsp-mode)


                                        ;(defun run-pywal-after-theme-switch ()
                                        ;  "Run the shell script that updates Pywal from cache."
                                        ;  (let ((script-path (expand-file-name "~/.config/polybar/hack/scripts/pywal-from-cache.sh")))
                                        ;    (when (file-executable-p script-path) ; Check if the script is executable
                                        ;      (shell-command script-path)))
                                        ;  (let ((script-path (expand-file-name "~/.config/polybar/hack/launch.sh")))
                                        ;    (when (file-executable-p script-path) ; Check if the script is executable
                                        ;      (shell-command script-path))))
                                        ;
                                        ;(defun theme-magic-and-pywal ()
                                        ;  "Run theme-magic and then update pywal from cache."
                                        ;  (theme-magic-from-emacs)
                                        ;  (run-pywal-after-theme-switch))
                                        ;
                                        ;(add-hook 'doom-load-theme-hook #'theme-magic-and-pywal)
;; (after! lsp-mode
;;   (setq lsp-clients-typescript-server-args '("--stdio")))


(add-hook 'vue-mode-hook #'lsp!)
