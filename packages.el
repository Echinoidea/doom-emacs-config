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

;; LSP
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))
(package! aphelesia)

;; Visuals
(package! beacon)

;; Themes
;; (package! builtin-package :disable t)
(package! doom-theme :disable t)

(package! organic-green-theme)
(package! soothe-theme)
(package! cherry-blossom-theme)
(package! espresso-theme)
(package! hc-zenburn-theme)
(package! jazz-theme)
(package! afternoon-theme)
(package! anti-zenburn-theme)
(package! sublime-themes)
(package! darktooth-theme)
(package! tao-theme)
(package! kaolin-themes)
(package! apropospriate-theme)
(package! ample-theme)
(package! moe-theme)
(package! amber-glow-theme)
(package! berry-theme)
(package! ember-twilight-theme)
(package! roseline-theme)
(package! solarized-gruvbox-theme)
(package! stimmung-themes)
(package! naysayer-theme)
(package! ef-themes)
(package! almost-mono-themes)
(package! badwolf-theme)
;; (package! base16-themes)
(package! birds-of-paradise-plus-theme)
(package! brutalist-theme)
(package! clues-theme)
(package! creamsody-theme)
(package! phoenix-dark-pink-theme)
(package! eziam-themes)
(package! faff-theme)
(package! forest-blue-theme)
(package! gotham-theme)
;; (package! green-phoshor-theme)
(package! inkpot-theme)
(package! lavender-theme)
(package! laguna-theme)
(package! metalheart-theme)
(package! naga-theme)
(package! nordic-night-theme)
;; (package! old-lace-theme)
(package! slime-theme)
(package! soft-stone-theme)
;; (package! spike-theme)
(package! srcery-theme)
(package! sunburn-theme)
(package! suscolors-theme)
(package! toxi-theme)
(package! tron-legacy-theme)
(package! warm-night-theme)
(package! purple-haze-theme)
;; (package! nofrills-themes)

;; Use this for non MELPA themes
(package! retro-orange-theme
  :recipe (:host github :repo "emacs-jp/replace-colorthemes"
           :files ("retro-orange-theme.el")))

(package! naga-blue-theme
  :recipe (:host github :repo "amolv06/naga-blue"
           :files ("naga-blue-theme.el")))
