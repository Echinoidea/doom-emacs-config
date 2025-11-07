;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;; ┏━┓╻ ╻┏━┓╺┳╸┏━╸┏┳┓   ┏━╸┏━┓┏┓╻┏━╸╻┏━╸
;; ┗━┓┗┳┛┗━┓ ┃ ┣╸ ┃┃┃   ┃  ┃ ┃┃┗┫┣╸ ┃┃╺┓
;; ┗━┛ ╹ ┗━┛ ╹ ┗━╸╹ ╹   ┗━╸┗━┛╹ ╹╹  ╹┗━┛
;; System Config

;; Disable savehist mode because it is causing CPU consumption
(savehist-mode -1)

(setq shell-file-name (executable-find "bash"))

(after! vterm
  (setq vterm-shell "/sbin/fish"))



;; ┏━╸╻ ╻╻╻     ┏┳┓┏━┓╺┳┓┏━╸
;; ┣╸ ┃┏┛┃┃     ┃┃┃┃ ┃ ┃┃┣╸
;; ┗━╸┗┛ ╹┗━╸   ╹ ╹┗━┛╺┻┛┗━╸
;; Evil Mode
(setq evil-escape-key-sequence "jk")

;; Vertico

(after! vertico
  (setq completion-styles '(flex basic partial-completion))
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  )


;; ┏━┓╻ ╻╻ ╻
;; ┣━┫┃┏┛┗┳┛
;; ╹ ╹┗┛  ╹
;; Avy
;;

;; Make avy goto-char-2 put cursor at END of match instead of after first char
(after! evil
  (defadvice! my/goto-after-char-2 (&rest _)
    :after #'evil-avy-goto-char-2
    (forward-char 2)))

(map! :nv "f" #'evil-avy-goto-char-2)

(map! :leader
      (:prefix-map ("j" . "jump and edit")
       :desc "goto char timer" "j" #'avy-goto-char-2
       :desc "goto line" "l" #'avy-goto-line
       :desc "goto word end" "w" #'avy-goto-word-1
       :desc "goto word start" "b" #'avy-goto-word-0

       (:prefix-map ("w" . "goto word")
        :desc "goto word above" "k" #'avy-goto-word-0-above
        :desc "goto word above" "j" #'avy-goto-word-0-below
        )

       (:prefix-map ("c" . "goto char")
        :desc "goto single char" "c" #'avy-goto-char
        :desc "goto double char" "d" #'avy-goto-char-2
        :desc "")

       (:prefix-map ("l" . "goto line")
        :desc "goto line above" "k" #'avy-goto-line-above
        :desc "goto line below" "j" #'avy-goto-line-below
        )

       ;; Sub-prefix for delete operations
       (:prefix-map ("d" . "delete/kill")
        :desc "kill region" "l" #'avy-kill-region
        :desc "copy region" "w" #'avy-kill-ring-save-region
        :desc "kill whole line" "r" #'avy-kill-whole-line)

       ;; Another sub-prefix for copy operations
       (:prefix-map ("x" . "copy")
        :desc "copy line" "l" #'avy-copy-line
        :desc "copy region" "r" #'avy-copy-region)))

;; ┏━┓┏━┓┏━┓┏━╸┏━┓┏━┓┏━┓┏┓╻┏━╸┏━╸
;; ┣━┫┣━┛┣━┛┣╸ ┣━┫┣┳┛┣━┫┃┗┫┃  ┣╸
;; ╹ ╹╹  ╹  ┗━╸╹ ╹╹┗╸╹ ╹╹ ╹┗━╸┗━╸
;; Appearance
;;

;; alpha / transparency
(set-frame-parameter nil 'alpha-background 100)  ;; Sets transparency for the current frame
(add-to-list 'default-frame-alist '(alpha-background . 100)) ;; Sets transparency for all new frames

;; Font
(setq doom-font (font-spec :family "AporeticSansMonoNerdFont" :size 14)
      doom-serif-font (font-spec :family "Maple Mono NF" :size 14)
      doom-variable-pitch-font (font-spec :family "Maple Mono NF" :size 12))

(after! org
  ;; Add a custom emphasis marker for bold-italic
  (add-to-list 'org-emphasis-alist
               '("=" (:weight bold :slant italic)
                 "<bi>" "</bi>"))
  
  ;; Update the regexp
  (org-set-emph-re 'org-emphasis-alist org-emphasis-alist))

(defun show-face-at-point ()
  "Show the face at point."
  (interactive)
  (message "Face: %s" (get-char-property (point) 'face)))

(defun my/buffer-face-mode-tidal ()
  "Sets font to bigblueterm for tidal mode"
  (interactive)
  (setq buffer-face-mode-face '(:family "BigBlueTerm437 Nerd Font" :height 100))
  (buffer-face-mode))


(defun my/buffer-face-mode-haskell ()
  "Sets font to bigblueterm for tidal mode"
  (interactive)
  (setq buffer-face-mode-face '(:family "GoMono Nerd Font" :height 100))
  (buffer-face-mode))

(add-hook 'tidal-mode-hook 'my/buffer-face-mode-tidal)
(add-hook 'haskell-mode-hook 'my/buffer-face-mode-haskell)

(after! writeroom-mode
  (setq +zen-text-scale 0
        +zen-mixed-pitch-modes nil
        writeroom-mode-line t
        writeroom-width 160))

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-always-show-macro-register t
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding t
        doom-modeline-major-mode-icon t
        doom-modeline-bar-width 0
        doom-modeline-height 25
        doom-modeline-modal nil))


(defadvice! doom-modeline--buffer-file-name-roam-aware-a (orig-fun)
  :around #'doom-modeline-buffer-file-name ; takes no args
  (if (string-match-p (regexp-quote org-roam-directory) (or buffer-file-name ""))
      (replace-regexp-in-string
       "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
       "(\\1-\\2-\\3) "
       (subst-char-in-string ?_ ?  buffer-file-name))
    (funcall orig-fun)))

(setq doom-theme 'doom-homage-black)
(setq display-line-numbers-type 'relative)


(defun my/run-script-after-theme-export (orig-fun &rest args)
  "Run custom shell script after theme export."
  (let ((result (apply orig-fun args)))
    (message "Theme export finished, running postrun script")
    (start-process-shell-command
     "postrun" nil
     "/home/gabriel/.config/wal/postrun")
    result))

(advice-add 'theme-magic-export-from-emacs :around #'my/run-script-after-theme-export)

(use-package! doom-wallust
  :config
  (doom-wallust-initialize)
  
  (map! :leader
        (:prefix ("h r" . "wallust")
         :desc "Browse themes" "b" #'doom-wallust-browse-themes
         :desc "Select theme" "s" #'doom-wallust-select-theme
         :desc "Reload theme" "w" #'doom-wallust-reload-theme)))

;; (defun reload-wallust-theme ()
;;   "Reload the doom-wallust theme after wallust updates it."
;;   (interactive)
;;   (let ((theme-file (expand-file-name "themes/doom-wallust-theme.el" doom-user-dir)))
;;     (load-file theme-file)
;;     (doom/reload-theme)))

;; (map! :leader
;;       :desc "Reload wallust theme"
;;       "h r w" #'reload-wallust-theme)

;; Centaur tabs
(after! centaur-tabs
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-show-new-tab-button nil)
  (setq uniquify-buffer-name-style 'forward) ; or 'reverse, 'post-forward, etc.
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*")
  )

(defun my/centaur-tabs-buffer-tab-label (tab)
  "Return a label for TAB using uniquify-style naming."
  (let ((buffer (car tab)))
    (if (bufferp buffer)
        (buffer-name buffer)  ; This will use the uniquified name
      (format "%s" buffer))))

(advice-add 'centaur-tabs-buffer-tab-label :override #'my/centaur-tabs-buffer-tab-label)

;; Dashboard

;; Custom Doom Dashboard Configuration

;; Your existing splash image configuration
(defun my/set-fancy-splash-image-by-theme ()
  "Set doom fancy splash image for specific loaded themes,
   if there is no corresponding image in $DOOM_DIR/splashes, load default doom ascii art"
  (let* ((theme-name (symbol-name (car custom-enabled-themes)))
         (image-path (expand-file-name
                      (format "~/.config/doom/splashes/%s.jpg" theme-name))))
    (if (file-exists-p image-path)
        (setq fancy-splash-image image-path)
      (setq fancy-splash-image "~/.config/doom/splashes/emacs-cute-splash.png")))) ;; nil = fallback to Doom ASCII, change to svg if you want

;; Set splash when theme is loaded
(add-hook 'doom-load-theme-hook #'my/set-fancy-splash-image-by-theme)

;; Handle daemon mode: wait until a frame is created
(when (daemonp)
  (add-hook 'after-make-frame-functions
            (lambda (_frame)
              (with-selected-frame _frame
                (my/set-fancy-splash-image-by-theme)))))

;; For non-daemon mode, call it directly
(unless (daemonp)
  (my/set-fancy-splash-image-by-theme))

;;;; Color blocks
(defvar my/dashboard-palette-char "██")

(defun my/dashboard-palette ()
  "Display color palette blocks"
  (let ((colors '(("black" . grey)
                  ("red" . red)
                  ("green" . green)
                  ("yellow" . yellow)
                  ("blue" . blue)
                  ("magenta" . magenta)
                  ("cyan" . cyan)
                  ("white" . fg)))
        (char (concat my/dashboard-palette-char " "))
        (result ""))

    (setq result (concat result "\n"))
    (dolist (color-pair colors)
      (let* ((color-name (car color-pair))
             (color-symbol (cdr color-pair))
             (hex-color (doom-color color-symbol)))
        (if hex-color
            (setq result (concat result (propertize char 'face `(:foreground ,hex-color)))))))
    (setq result (concat result "\n\n"))
    result))

(defun my/dashboard-palette-widget ()
  "Color palette dashboard widget"
  (let ((palette-line (string-trim (my/dashboard-palette))))
    (insert (+doom-dashboard--center
             +doom-dashboard--width
             palette-line)
            "\n")))

(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        my/dashboard-palette-widget
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded
        doom-dashboard-widget-footer))


(defun my/refresh-dashboard-on-theme-change (&rest _)
  "Refresh the dashboard when theme changes."
  (when (and (fboundp '+doom-dashboard-reload)
             (get-buffer +doom-dashboard-name))
    (+doom-dashboard-reload)))

(add-hook 'doom-load-theme-hook #'my/refresh-dashboard-on-theme-change)

;; ╻  ┏━┓┏━┓
;; ┃  ┗━┓┣━┛
;; ┗━╸┗━┛╹
;; LSP
(setq! lsp-auto-execute-action nil)

(after! corfu
  (setq! corfu-auto-delay 0.0))

(add-hook 'lua-mode-hook
          (lambda ()
            (setq-local lsp-disabled-clients '(semgrep-ls))))

;; Configure TypeScript language server BEFORE lsp-mode loads
(setq lsp-clients-typescript-server-path "/run/current-system/sw/bin/typescript-language-server")

(after! lsp-mode
  ;; Make sure ts-ls knows about tree-sitter modes
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda () (list "/run/current-system/sw/bin/typescript-language-server" "--stdio")))
    :activation-fn (lsp-activate-on "typescript" "typescriptreact")
    :priority -1
    :server-id 'ts-ls
    :add-on? t)))

;; nix nil-lsp
(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "nil")
                    :major-modes '(nix-mode)
                    :priority 0
                    :server-id 'nil-ls
                    :initialization-options
                    (lambda ()
                      `(:nil (:flake (:autoArchiveInputs :json-false
                                      :autoEvalInputs :json-false)))))))

(after! lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.direnv\\'"))

(use-package! typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (add-hook 'typescript-ts-mode-hook #'lsp!)
  (add-hook 'tsx-ts-mode-hook #'lsp!))

(add-hook! '(tsx-ts-mode-hook typescript-ts-mode-hook) #'emmet-mode)

(after! lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "ocamllsp")
                    :major-modes '(tuareg-mode)
                    :server-id 'ocaml-lsp)))

(add-hook 'tuareg-mode-hook #'lsp-deferred)

(after! tuareg
  (remove-hook 'tuareg-mode-hook #'opam-switch-mode))

;; (after! lsp-tailwindcss
;;   (setq lsp-tailwindcss-server-path "/run/current-system/sw/bin/tailwindcss-language-server"))

(after! lsp-javascript
  (setq lsp-typescript-npm "/run/current-system/sw/bin/npm"))


(add-hook! '(typescript-ts-mode-hook tsx-ts-mode-hook emacs-lisp-mode-hook) #'rainbow-delimiters-mode)

(after! lua-mode
  (setq lsp-file-watch-threshold 2000)
  )

;; (after! rustic
;;   (setq rustic-lsp-server 'rust-analyzer)
;;   (setq rustic-format-on-save t)
;;   (setq rustic-format-display-buffer nil)
;;   (setq lsp-inlay-hints-mode t))

;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;               (setup-tide-mode))))
;; ;; enable typescript-tslint checker
;; (flycheck-add-mode 'typescript-tslint 'web-mode)

;; Enable diagnostics popups
(add-hook 'flycheck-mode-hook #'flycheck-popup-tip-mode)


;; Code Testing

(defun test-busted ()
  "Run busted tests. Requires .envrc with luaPackages.busted in flake.nix."
  (interactive nil lua-mode)  ; Only available in lua-mode
  (when (envrc--find-env-dir)
    (unless envrc-mode
      (envrc-mode 1))
    (let ((default-directory (file-name-directory (buffer-file-name)))
          (output (shell-command-to-string "eval \"$(direnv export bash)\" && busted spec/")))
      (with-current-buffer (get-buffer-create "*Busted Tests*")
        (erase-buffer)
        (insert output)
        (display-buffer (current-buffer))))))

;; ┏━┓┏━┓┏━╸   ┏┳┓┏━┓╺┳┓┏━╸
;; ┃ ┃┣┳┛┃╺┓   ┃┃┃┃ ┃ ┃┃┣╸
;; ┗━┛╹┗╸┗━┛   ╹ ╹┗━┛╺┻┛┗━╸
;; Org Mode

;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(setq! doom-modeline-enable-word-count t)

(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" " ● %s") project-name))))))

(defadvice! +org-indent--reduced-text-prefixes ()
  :after #'org-indent--compute-prefixes
  (setq org-indent--text-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (when (> org-indent-indentation-per-level 0)
    (dotimes (n org-indent--deepest-level)
      (aset org-indent--text-line-prefixes
            n
            (org-add-props
                (concat (make-string (* n (1- org-indent-indentation-per-level))
                                     ?\s)
                        (if (> n 0)
                            (char-to-string org-indent-boundary-char)
                          "\u200b"))
                nil 'face 'org-indent)))))

(defun my/org-do-demote ()
  "Make `org-do-demote` interactive so it works on selected region."
  (interactive)
  (org-do-demote))

(defun my/org-do-promote ()
  "Make `org-do-promote` interactive so it works on selected region."
  (interactive)
  (org-do-promote))

(setq org-directory "~/org/")

(add-hook 'org-mode-hook #'org-modern-indent-mode 90)


(after! org
  (setq org-agenda-files
        (list "~/org/todo-personal.org"
              "~/org/todo-work.org"
              "~/org/todo-ibegin.org"
              "~/org/todo-midas.org"
              "~/org/todo-school.org"
              "~/org/todo-gamedev.org"
              "~/org/todo-programming.org"
              "~/org/calendar.org"))
  )

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

(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; (after! biblio
;;   (setq org-cite-global-bibliography '("~/org/references.bib"))
;;   )

;; Org mode - PDF
(save-place-mode 1)

;; (org-babel-do-load-languages
;;  'org-babel-load-languages '((C . t)))

(after! org-modern
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-agenda-tags-column 0
   org-ellipsis "...")
  org-modern-star '("✿" "❀" "❁" "❃" "❋" "✾" "✽" "✻")
  )
(setq org-modern-star '("✿" "❀" "❁" "❃" "❋" "✾" "✽" "✻"))

;; org-modern-star '("●" "○" "◎" "◉" "◆" "◇" "◆" "◇")
;; org-modern-star '("✿" "❀" "❁" "❃" "❋" "✾" "✽" "✻")
;; Deft
(after! deft
  (setq deft-directory "~/org/deft/")
  (setq deft-recursive t))


(after! olivetti-mode
  (olivetti-set-width 110))

;; org-agenda
(defun org-agenda-open-hook ()
  "Hook to be run when org-agenda is opened"
  (olivetti-mode))

(add-hook 'org-agenda-mode-hook 'org-agenda-open-hook)

(after! org-agenda
  (setq org-agenda-timegrid-use-ampm t)

  ;; Only show one day of the agenda at a time
  (setq org-agenda-span 1
        org-agenda-start-day "+0d")

  ;; Hide duplicates of the same todo item
  (setq org-agenda-skip-timestamp-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-timestamp-if-deadline-is-shown t)

  ;; Clean time grid - remove the conflicting configuration
  (setq org-agenda-current-time-string "")
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800)
          "" "----------------"))

  ;; Category icons - theme colors will be used
  (when (featurep 'all-the-icons)
    (setq org-agenda-category-icon-alist
          `(("School" ,(list (all-the-icons-faicon "graduation-cap" :height 0.9 :v-adjust 0.1)) nil nil :ascent center)
            ("IBEGIN" ,(list (all-the-icons-faicon "child" :height 0.9 :v-adjust 0.1)) nil nil :ascent center)
            ("MIDAS" ,(list (all-the-icons-faicon "line-chart" :height 0.9 :v-adjust 0.1)) nil nil :ascent center)
            ("Work" ,(list (all-the-icons-faicon "suitcase" :height 0.9 :v-adjust 0.1)) nil nil :ascent center)
            ("Gamedev" ,(list (all-the-icons-faicon "gamepad" :height 0.9 :v-adjust 0.1)) nil nil :ascent center)
            ("Programming" ,(list (all-the-icons-faicon "code" :height 0.9 :v-adjust 0.1)) nil nil :ascent center)
            ("Personal" ,(list (all-the-icons-material "person" :height 0.9 :v-adjust 0.1)) nil nil :ascent center))))

  ;; Enhanced prefix format to show icon + category + content
  (setq org-agenda-prefix-format '(
                                   (agenda . "  %i %-12:c %t ")     ; icon + category + time
                                   (todo . "  %i %-12:c ")          ; icon + category
                                   (tags . "  %i %-12:c ")          ; icon + category
                                   (search . "  %i %-12:c "))))     ; icon + category

;; Enhanced font faces for better visual hierarchy - respects theme colors
(custom-set-faces!
  ;; Done items - strikethrough styling
  '(org-agenda-done :strike-through t)

  ;; Current day emphasis
  '(org-agenda-date-today :weight bold :height 1.1 :underline nil)

  ;; Regular dates emphasis
  '(org-agenda-date :weight bold :height 1.05)

  ;; Weekend dates
  '(org-agenda-date-weekend :weight bold :height 1.05 :slant italic)

  ;; Categories
  '(org-agenda-structure :weight bold :height 0.9)

  ;; Current time indicator
  '(org-agenda-current-time :weight bold))

;; org-super-agenda with enhanced styling
(org-super-agenda-mode t)
(after! org-super-agenda
  (setq org-super-agenda-groups
        '(;; Overdue items - highest priority
          (:name " Overdue"
           :scheduled past
           :order 1
           :face (:weight bold))

          ;; Today's scheduled items
          (:name " Today"
           :time-grid t
           :date today
           :scheduled today
           :order 2
           :face (:weight bold))

          ;; Work items
          (:name " Work"
           :file-path "work"
           :order 3
           :face (:weight semi-bold))

          ;; Personal items
          (:name " Personal"
           :file-path "personal"
           :order 4
           :face (:weight semi-bold))

          ;; IBEGIN items
          (:name " IBEGIN"
           :file-path "ibegin"
           :order 5
           :face (:weight semi-bold))

          ;; MIDAS items
          (:name " MIDAS"
           :file-path "midas"
           :order 6
           :face (:weight semi-bold))

          ;; School items
          (:name " School"
           :file-path "school"
           :order 7
           :face (:weight semi-bold))

          ;; Programming items
          (:name " Programming"
           :file-path "programming"
           :order 8
           :face (:weight semi-bold))

          ;; Gamedev items
          (:name " Gamedev"
           :file-path "gamedev"
           :order 9
           :face (:weight semi-bold)))))

;; Additional styling for super-agenda headers
(custom-set-faces!
  '(org-super-agenda-header :height 1.1 :weight bold :underline nil :extend t))


(with-eval-after-load 'org (global-org-modern-mode))

;; To-do management
;; Quick TODO creation functions
;; Quick TODO creation functions
(defvar my/todo-categories
  '("personal" "work" "ibegin" "midas" "school" "gamedev" "programming")
  "List of available TODO categories")

(defvar my/org-dir "~/org/"
  "Directory where org files are stored")

(defun my/get-todo-file (category)
  "Get the full path to the TODO file for CATEGORY."
  (expand-file-name (concat "todo-" category ".org") my/org-dir))

(defun my/ensure-todo-file-exists (category)
  "Ensure the TODO file for CATEGORY exists with proper header."
  (let ((file-path (my/get-todo-file category)))
    (unless (file-exists-p file-path)
      (with-temp-file file-path
        (insert (format "#+TITLE: %s TODOs
#+CATEGORY: %s
#+STARTUP: overview
#+TODO: TODO DOING | DONE CANCELLED

* Tasks
"
                        (capitalize category)
                        (capitalize category)))))))

(defun my/quick-todo (category title &optional deadline effort tags)
  "Create a quick TODO item in CATEGORY with TITLE, optional DEADLINE and EFFORT."
  (interactive)
  (my/ensure-todo-file-exists category)
  (let ((file-path (my/get-todo-file category)))
    (with-current-buffer (find-file-noselect file-path)
      (goto-char (point-max))
      (insert "\n** TODO " title)
      (when deadline
        (insert "\n   DEADLINE: " deadline))
      (when effort
        (insert "\n   :PROPERTIES:\n   :Effort: " effort "\n   :END:"))
      (when tags
        (org-set-tags tags))
      (save-buffer)
      (message "TODO added to %s" category))))

(defun my/dmenu-quick-todo ()
  "Create a TODO via dmenu interface."
  (interactive)
  (let* ((category (completing-read "Category: " my/todo-categories))
         (title (read-string "TODO title: "))
         (has-deadline (y-or-n-p "Add deadline? "))
         (deadline (when has-deadline
                     (format "<%s>" (org-read-date))))
         (has-effort (y-or-n-p "Add time estimate? "))
         (effort (when has-effort
                   (read-string "Time estimate (e.g., 2h, 30m): ")))
         (has-tags (y-or-n-p "Add tags? "))
         (tags (when has-tags
                 (split-string (read-string "Tags (space separated): ") " " t))))
    (my/quick-todo category title deadline effort tags)))

(defun my/quick-todo-at-point ()
  "Convert current line to a TODO item with prompts."
  (interactive)
  (let* ((current-line (thing-at-point 'line t))
         (title (string-trim current-line))
         (category (completing-read "Category: " my/todo-categories)))
    (kill-whole-line)
    (my/quick-todo category title)))

;; Simplified function for dmenu/external calls
(defun my/quick-todo-simple (category title)
  "Create a simple TODO item for dmenu/external calls."
  (my/ensure-todo-file-exists category)
  (let ((file-path (my/get-todo-file category)))
    (with-current-buffer (find-file-noselect file-path)
      (goto-char (point-max))
      (insert "\n** TODO " title)
      (save-buffer)
      (message "TODO added to %s" category))))
(defun my/mark-todo-done ()
  "Mark current TODO as DONE and add completion timestamp."
  (interactive)
  (when (org-at-heading-p)
    (org-todo 'done)
    (org-add-planning-info 'closed (org-current-effective-time))
    (save-buffer)
    (message "TODO marked as DONE")))

;; Quick refile function
(defun my/quick-refile-todo ()
  "Quickly refile current TODO to another category."
  (interactive)
  (when (org-at-heading-p)
    (let* ((category (completing-read "Move to category: " my/todo-categories))
           (target-file (my/get-todo-file category)))
      (my/ensure-todo-file-exists category)
      (org-refile nil nil (list nil target-file nil nil))
      (message "TODO moved to %s" category))))

;; Yasnippet integration
(defun my/org-todo-snippet ()
  "Insert a TODO template with prompts."
  (interactive)
  (let* ((title (read-string "TODO title: "))
         (has-deadline (y-or-n-p "Add deadline? "))
         (deadline-str (if has-deadline
                           (format "\n   DEADLINE: <%s>" (org-read-date))
                         ""))
         (has-effort (y-or-n-p "Add time estimate? "))
         (effort-str (if has-effort
                         (let ((effort (read-string "Time estimate: ")))
                           (format "\n   :PROPERTIES:\n   :Effort: %s\n   :END:" effort))
                       "")))
    (insert (format "** TODO %s%s%s" title deadline-str effort-str))
    (org-set-tags-command))) ; Prompt for tags

;; Function to create TODO with deadline
(defun my/quick-todo-with-deadline (category title deadline-str)
  "Create a TODO item with a deadline."
  (my/ensure-todo-file-exists category)
  (let ((file-path (my/get-todo-file category)))
    (with-current-buffer (find-file-noselect file-path)
      (goto-char (point-max))
      (insert "\n** TODO " title)
      (insert "\n   DEADLINE: <" deadline-str ">")
      (save-buffer)
      (message "TODO added to %s with deadline %s" category deadline-str))))

;; RSS
(add-hook! 'elfeed-search-mode-hook #'elfeed-update)


;; ╻╺┳┓┏━╸
;; ┃ ┃┃┣╸
;; ╹╺┻┛┗━╸
;; IDE

(after! treemacs
  (treemacs-tag-follow-mode 1))

(after! treemacs
  (setq treemacs-indent-guide-mode t))

;; ┏━╸┏━┓╺┳╸┏━╸╻
;; ┃╺┓┣━┛ ┃ ┣╸ ┃
;; ┗━┛╹   ╹ ┗━╸┗━╸
;; gptel

(after! gptel
  (setq gptel-default-mode 'org-mode))

;; (setq auth-sources '("~/.gnupg"))
;; 
(after! auth-source
  (setq auth-sources '("~/.authinfo.gpg")))

(after! gptel
  (gptel-make-ollama "Ollama"
    :host "172.16.108.90:11434"
    :stream t
    :models '(gemma3:1b gemma3 codellama llama2-uncensored llava)))

(after! gptel
  (map! :leader
        :prefix "l"
        :desc "GPTel Open Buffer" "b" #'gptel
        :desc "GPTel Menu" "m" #'gptel-mode
        :desc "GPTel Add" "a" #'gptel-add
        :desc "GPTel Add File" "f" #'gptel-add-file
        :desc "GPTel Rewrite" "r" #'gptel-rewrite
        ))

(set-popup-rule! "^\\*eww\\*" :ignore t)

;; ┏━╸╻ ╻╻ ╻
;; ┣╸ ┃╻┃┃╻┃
;; ┗━╸┗┻┛┗┻┛
;; Eww

;; Wrapper function to open eww in the current frame instead of popup
(defun eww-open-in-frame (&optional url)
  (interactive)
  (set-popup-rule! "^\\*eww\\*" :ignore t)
  (if url
      (eww url)
    (call-interactively #'eww))
  )

(set-popup-rule! "^\\*eww\\*"
  :side 'right
  :size 0.5
  :quit nil
  :ttl 0)


;;;; Tidal

(after! tidal
  (setq tidal-interpreter "/usr/bin/ghci")
  (setq tidal-boot-script-path "/usr/share/haskell-tidal/BootTidal.hs"))

(defun tidal-run-region-fixed ()
  "Run tidal-eval-multiple-lines instead of eval-region."
  (interactive)
  (tidal-eval-multiple-lines)
  (set-mark-command t) ;; unmark the region
  )

(after! colorful-mode
  (global-colorful-mode))

(after! dimmer-mode
  (dimmer-configure-which-key)
  (dimmer-configure-org)
  (dimmer-configure-posframe)
  (dimmer-mode t))


;; ┏━╸╻ ╻┏┓╻┏━╸╺┳╸╻┏━┓┏┓╻┏━┓
;; ┣╸ ┃ ┃┃┗┫┃   ┃ ┃┃ ┃┃┗┫┗━┓
;; ╹  ┗━┛╹ ╹┗━╸ ╹ ╹┗━┛╹ ╹┗━┛
;; Functions

;; Org roam node to quartz 4 content to digital-garden
;; (defun org-roam-export-to-quartz ()
;;   "Export current org-roam buffer to Quartz 4 formatted markdown."
;;   (interactive)
;;   (unless (derived-mode-p 'org-mode)
;;     (error "This function can only be called from an org-mode buffer"))

;;   (let* ((node (org-roam-node-at-point))
;;          (title (or (org-roam-node-title node)
;;                     (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
;;          (tags (org-roam-node-tags node))
;;          (content (org-roam--export-content-to-markdown))
;;          (filename (org-roam--sanitize-filename title))
;;          (output-dir "~/code/web/digital-garden/content/")
;;          (output-file (expand-file-name (concat filename ".md") output-dir)))

;;     ;; Ensure output directory exists
;;     (unless (file-directory-p output-dir)
;;       (make-directory output-dir t))

;;     ;; Write the formatted markdown file
;;     (with-temp-file output-file
;;       (insert (org-roam--format-quartz-frontmatter title tags))
;;       (insert "\n")
;;       (insert content))

;;     (message "Exported to: %s" output-file)))

;; (defun org-roam--export-content-to-markdown ()
;;   "Export the content of current org buffer to markdown."
;;   (save-excursion
;;     (goto-char (point-min))
;;     ;; Skip past any org-mode metadata/properties
;;     (when (re-search-forward "^\\*\\|^[^#:].*$" nil t)
;;       (beginning-of-line)
;;       (let ((content-start (point)))
;;         (org-export-as 'md nil nil nil '(:with-toc nil :with-tags nil))))))

;; (defun org-roam--format-quartz-frontmatter (title tags)
;;   "Format the YAML frontmatter for Quartz 4."
;;   (let ((frontmatter "---\n")
;;         (yaml-title (format "title: %s\n" title))
;;         (yaml-draft "draft: false\n")
;;         (yaml-tags (if tags
;;                        (concat "tags:\n"
;;                                (mapconcat (lambda (tag)
;;                                             (format "  - %s" tag))
;;                                           tags "\n") "\n")
;;                      "")))
;;     (concat frontmatter yaml-title yaml-draft yaml-tags "---\n")))

;; (defun org-roam--sanitize-filename (title)
;;   "Sanitize title for use as filename."
;;   (let ((clean-title (replace-regexp-in-string "[^a-zA-Z0-9-_\\. ]" "" title)))
;;     (replace-regexp-in-string "\\s-+" "-" clean-title)))

;; ;; Alternative version that uses org-export-as directly
;; (defun org-roam-export-to-quartz-alt ()
;;   "Alternative export function using org-export-as directly."
;;   (interactive)
;;   (unless (derived-mode-p 'org-mode)
;;     (error "This function can only be called from an org-mode buffer"))

;;   (let* ((node (org-roam-node-at-point))
;;          (title (or (org-roam-node-title node)
;;                     (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
;;          (tags (org-roam-node-tags node))
;;          (filename (org-roam--sanitize-filename title))
;;          (output-dir "~/code/web/digital-garden/content/")
;;          (output-file (expand-file-name (concat filename ".md") output-dir)))

;;     ;; Ensure output directory exists
;;     (unless (file-directory-p output-dir)
;;       (make-directory output-dir t))

;;     ;; Export to markdown string
;;     (let ((markdown-content (org-export-as 'md nil nil nil '(:with-toc nil :with-tags nil))))
;;       (with-temp-file output-file
;;         (insert (org-roam--format-quartz-frontmatter title tags))
;;         (insert "\n")
;;         (insert markdown-content)))

;;     (message "Exported to: %s" output-file)))

;; (setq tidal-boot-script-path "~/tidal/BootTidal.hs")

