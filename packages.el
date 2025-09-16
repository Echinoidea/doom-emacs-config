;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)
(unpin! lsp-mode)


(package! all-the-icons)
(package! gptel)
(package! org-journal)
(package! theme-magic)
(package! saveplace-pdf-view)
(package! listen)
(package! org-roam-ui)
(package! org-modern)
(package! org-super-agenda)
(package! org-modern-indent
  :recipe (:host nil :repo "https://github.com/jdtsmith/org-modern-indent"
           :files ("*.el")
           ))

;; LSP
;; (package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

;; Visuals
;; (package! beacon)
                                        ; (package! solaire-mode)

;; Themes
;; (package! builtin-package :disable t)
(package! doom-theme :disable t)
;; (when (package! lsp-bridge
;;         :recipe (:host github
;;                  :repo "manateelazycat/lsp-bridge"
;;                  :branch "master"
;;                  :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;                  ;; do not perform byte compilation or native compilation for lsp-bridge
;;                  :build (:not compile))))
(package! markdown-mode)
(package! yasnippet)

                                        ; (package! organic-green-theme)
                                        ; (package! soothe-theme)
                                        ; (package! cherry-blossom-theme)
                                        ; (package! espresso-theme)
                                        ; (package! hc-zenburn-theme)
                                        ; (package! jazz-theme)
                                        ; (package! afternoon-theme)
                                        ; (package! anti-zenburn-theme)
;; (package! sublime-themes)
                                        ; (package! darktooth-theme)
(package! tao-theme)
(package! kaolin-themes)
                                        ; (package! apropospriate-theme)
                                        ; (package! ample-theme)
                                        ; (package! moe-theme)
                                        ; (package! amber-glow-theme)
                                        ; (package! berry-theme)
                                        ; (package! ember-twilight-theme)
                                        ; (package! roseline-theme)
                                        ; (package! solarized-gruvbox-theme)
                                        ; (package! stimmung-themes)
                                        ; (package! naysayer-theme)
(package! ef-themes)
(package! almost-mono-themes)
                                        ; (package! badwolf-theme)
                                        ; (package! birds-of-paradise-plus-theme)
                                        ; (package! brutalist-theme)
                                        ; (package! clues-theme)
                                        ; (package! creamsody-theme)
                                        ; (package! phoenix-dark-pink-theme)
                                        ; (package! eziam-themes)
                                        ; (package! faff-theme)
                                        ; (package! forest-blue-theme)
                                        ; (package! gotham-theme)
;; (package! green-phoshor-theme)
                                        ; (package! inkpot-theme)
                                        ; (package! lavender-theme)
                                        ; (package! laguna-theme)
                                        ; (package! metalheart-theme)
                                        ; (package! naga-theme)
                                        ; (package! nordic-night-theme)
;; (package! old-lace-theme)
                                        ; (package! slime-theme)
;; (package! spike-theme)
                                        ; (package! sunburn-theme)
                                        ; (package! suscolors-theme)
                                        ; (package! toxi-theme)
                                        ; (package! tron-legacy-theme)
                                        ; (package! warm-night-theme)
                                        ; (package! purple-haze-theme)

(package! colorless-themes
  :recipe (:host nil :repo "https://git.sr.ht/~lthms/colorless-themes.el"
           :files ("*.el")
           ))

(package! everblush-theme
  :recipe (:host nil :repo "https://github.com/Everblush/doomemacs"
           :files ("*.el")
           ))


;; (package! vue-mode)

(package! kanagawa
  :recipe (:host nil :repo "https://github.com/Fabiokleis/kanagawa-emacs"
           :files ("*.el")
           ))

(package! catppuccin-theme)

(package! kuronami-theme)

(package! inkpot-theme)

(package! spacious-padding)

(package! doric-themes)

(package! standard-themes)

(package! stimmung-themes)

(package! olivetti-mode
  :recipe (:host nil :repo "https://github.com/rnkn/olivetti"
           :files ("*.el")
           ))

(package! doom-two-tone-themes
  :recipe (:host github
           :repo "eliraz-refael/doom-two-tone-themes"
           :files ("doom-two-tone-themes.el" "themes/*.el")))

;; (package! flyover
;;   :recipe (:host github
;;            :repo "konrad1977/flyover"
;;            :files ("flyover.el")))

(package! indent-bars)

;; (package! emacs-application-framework
;;   :recipe (:host github :repo "manateelazycat/emacs-application-framework"
;;            :files ("eaf.el" "src/lisp/*.el")))

(package! tidal)

(package! buffer-box
  :recipe (:host nil :repo "https://github.com/rougier/buffer-box"
           :files ("*.el")))

(package! sudoku)

(package! ewal-doom-themes
  :recipe (:host nil
           :repo "https://github.com/cyruseuros/ewal"
           :branch "master"
           :files ("doom-themes/*.el")
           :depth full))

(package! nethack)
