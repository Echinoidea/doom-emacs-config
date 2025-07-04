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


;; Disable savehist mode because it is causing CPU consumption
(savehist-mode -1)

(setq evil-escape-key-sequence "jk")

(setq doom-font (font-spec :family "AporeticSansMono Nerd Font" :size 14)
      doom-serif-font (font-spec :family "GoMono Nerd Font" :size 14)
      doom-variable-pitch-font (font-spec :family "Latin Modern Roman" :size 12))

(after! writeroom-mode
  (setq +zen-text-scale 0
        +zen-mixed-pitch-modes nil
        writeroom-mode-line t
        writeroom-width 160))

(after! treemacs
  (treemacs-tag-follow-mode 1))

;; Enable diagnostics popups
(after! flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-popup-tip-mode))

(after! gptel
  (setq gptel-default-mode 'org-mode))

(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; (setq fancy-splash-image "/home/gabriel/xfce/95-small.jpg")

                                        ; (add-to-list 'default-frame-alist '(width . 100))
                                        ; (add-to-list 'default-frame-alist '(height . 40))
                                        ;

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
                                        ;
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

(use-package! spacious-padding
  :ensure t
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 4
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :fringe-width 0))
  (spacious-padding-mode 1))

(after! cdlatex
  (map! :map cdlatex-mode-map
        :i "TAB" #'cdlatex-tab)

  (setq cdlatex-math-symbol-alist ; expand when prefixed with `
        '((?e ("\\varepsilon" "\\epsilon"))
          (?f ("\\varphi" "\\phi"))
          (?0 ("\\varnothing" "\\emptyset"))
          (?> ("\\to" "\\implies"))
          (?= ("\\iff" "\\equiv"))
          (?| ("\\mid" "\\vert"))
          (?: ("\\coloneqq")))
        cdlatex-math-modify-alist ; modify text with '
        ;; key mathcmd textcmd type rmdot it
        '((?b "\\mathbb" nil t nil nil)
          (?c "\\mathcal" nil t nil nil)
          (?f "\\mathbf" nil t nil nil)
          (?m "\\mathrm" nil t nil nil)
          (?r "\\mathrel" nil t nil nil)
          (?s "\\mathsf" nil t nil nil)
          (?o "\\operatorname" nil t nil nil))
        cdlatex-command-alist ; expand with <TAB>
        ;; keyword docstring replace hook args textflag mathflag
        '(("eqn" "Insert an EQUATION* environment template" "" cdlatex-environment ("equation*") t nil)
          ("aln" "Insert an ALIGN* environment template" "" cdlatex-environment ("align*") t nil)
          ("sum" "Insert \\sum\\limits_{}^{}" "\\sum\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
          ("prod" "Insert \\prod\\limits_{}^{}" "\\prod\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
          ("bun" "Insert \\bigcup\\limits_{}^{}" "\\bigcup\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
          ("bin" "Insert \\bigcap\\limits_{}^{}" "\\bigcap\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
          ("lim" "Insert \\lim_\\limits{{} \to {}}" "\\lim_\\limits{{?} \to {}}" cdlatex-position-cursor nil nil t)
          ("sr" "Insert {}^2" "{?}^2" cdlatex-position-cursor nil nil t)
          ("cb" "Insert {}^3" "{?}^3" cdlatex-position-cursor nil nil t)

          ("op" "Insert \\operatorname{}()" "\\operatorname{?}()" cdlatex-position-cursor nil nil t))))


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


(setq doom-theme 'doom-cyan-charcoal)
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

(with-eval-after-load 'org (global-org-modern-mode))

(after! biblio
  (setq org-cite-global-bibliography '("~/org/references.bib"))
  )


;; Org mode - PDF
(save-place-mode 1)

(setq auth-sources '("~/.authinfo.gpg"))

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

(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)))

(add-hook 'vue-mode-hook #'lsp!)
