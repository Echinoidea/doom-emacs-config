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

;; ┏━┓╻ ╻┏━┓╺┳╸┏━╸┏┳┓   ┏━╸┏━┓┏┓╻┏━╸╻┏━╸
;; ┗━┓┗┳┛┗━┓ ┃ ┣╸ ┃┃┃   ┃  ┃ ┃┃┗┫┣╸ ┃┃╺┓
;; ┗━┛ ╹ ┗━┛ ╹ ┗━╸╹ ╹   ┗━╸┗━┛╹ ╹╹  ╹┗━┛
;; System Config

;; Disable savehist mode because it is causing CPU consumption
(savehist-mode -1)

(after! vterm
  (setq vterm-shell "/sbin/fish"))

;; ┏━╸╻ ╻╻╻     ┏┳┓┏━┓╺┳┓┏━╸
;; ┣╸ ┃┏┛┃┃     ┃┃┃┃ ┃ ┃┃┣╸
;; ┗━╸┗┛ ╹┗━╸   ╹ ╹┗━┛╺┻┛┗━╸
;; Evil Mode
(setq evil-escape-key-sequence "jk")

(after! evil
  ;; Redefine 'o' and 'O' to not enter insert mode
  (map! :map evil-normal-state-map
        "o" (cmd! (evil-open-below 1) (evil-normal-state))
        "O" (cmd! (evil-open-above 1) (evil-normal-state)))
  )

;; ┏━┓┏━┓┏━┓┏━╸┏━┓┏━┓┏━┓┏┓╻┏━╸┏━╸
;; ┣━┫┣━┛┣━┛┣╸ ┣━┫┣┳┛┣━┫┃┗┫┃  ┣╸
;; ╹ ╹╹  ╹  ┗━╸╹ ╹╹┗╸╹ ╹╹ ╹┗━╸┗━╸
;; Appearance
;;
;; Font
(setq doom-font (font-spec :family "AporeticSansMono Nerd Font" :size 14)
      doom-serif-font (font-spec :family "GoMono Nerd Font" :size 14)
      doom-variable-pitch-font (font-spec :family "Latin Modern Roman" :size 12))

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

(setq doom-theme 'kaolin-dark)
(setq display-line-numbers-type 'relative)

;; ╻  ┏━┓┏━┓
;; ┃  ┗━┓┣━┛
;; ┗━╸┗━┛╹
;; LSP
(setq lsp-auto-execute-action nil)

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-format-on-save t)
  (setq rustic-format-display-buffer nil)
  (setq lsp-inlay-hints-mode t))


(setq lsp-clients-typescript-server "/sbin/typescript-language-server")
(setq lsp-clients-typescript-server-args '("--stdio" "--tsserver-path" "/sbin/tsserver"))


(add-hook 'vue-mode-hook #'lsp!)

;; Enable diagnostics popups
(after! flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-popup-tip-mode))

;; ┏━┓┏━┓┏━╸   ┏┳┓┏━┓╺┳┓┏━╸
;; ┃ ┃┣┳┛┃╺┓   ┃┃┃┃ ┃ ┃┃┣╸
;; ┗━┛╹┗╸┗━┛   ╹ ╹┗━┛╺┻┛┗━╸
;; Org Mode

(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

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

(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)))

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


;; ┏━╸╻ ╻┏┓╻┏━╸╺┳╸╻┏━┓┏┓╻┏━┓
;; ┣╸ ┃ ┃┃┗┫┃   ┃ ┃┃ ┃┃┗┫┗━┓
;; ╹  ┗━┛╹ ╹┗━╸ ╹ ╹┗━┛╹ ╹┗━┛
;; Functions

;; Org roam node to quartz 4 content to digital-garden
(defun org-roam-export-to-quartz ()
  "Export current org-roam buffer to Quartz 4 formatted markdown."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "This function can only be called from an org-mode buffer"))

  (let* ((node (org-roam-node-at-point))
         (title (or (org-roam-node-title node)
                    (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
         (tags (org-roam-node-tags node))
         (content (org-roam--export-content-to-markdown))
         (filename (org-roam--sanitize-filename title))
         (output-dir "~/code/web/digital-garden/content/")
         (output-file (expand-file-name (concat filename ".md") output-dir)))

    ;; Ensure output directory exists
    (unless (file-directory-p output-dir)
      (make-directory output-dir t))

    ;; Write the formatted markdown file
    (with-temp-file output-file
      (insert (org-roam--format-quartz-frontmatter title tags))
      (insert "\n")
      (insert content))

    (message "Exported to: %s" output-file)))

(defun org-roam--export-content-to-markdown ()
  "Export the content of current org buffer to markdown."
  (save-excursion
    (goto-char (point-min))
    ;; Skip past any org-mode metadata/properties
    (when (re-search-forward "^\\*\\|^[^#:].*$" nil t)
      (beginning-of-line)
      (let ((content-start (point)))
        (org-export-as 'md nil nil nil '(:with-toc nil :with-tags nil))))))

(defun org-roam--format-quartz-frontmatter (title tags)
  "Format the YAML frontmatter for Quartz 4."
  (let ((frontmatter "---\n")
        (yaml-title (format "title: %s\n" title))
        (yaml-draft "draft: false\n")
        (yaml-tags (if tags
                       (concat "tags:\n"
                               (mapconcat (lambda (tag)
                                            (format "  - %s" tag))
                                          tags "\n") "\n")
                     "")))
    (concat frontmatter yaml-title yaml-draft yaml-tags "---\n")))

(defun org-roam--sanitize-filename (title)
  "Sanitize title for use as filename."
  (let ((clean-title (replace-regexp-in-string "[^a-zA-Z0-9-_\\. ]" "" title)))
    (replace-regexp-in-string "\\s-+" "-" clean-title)))

;; Alternative version that uses org-export-as directly
(defun org-roam-export-to-quartz-alt ()
  "Alternative export function using org-export-as directly."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "This function can only be called from an org-mode buffer"))

  (let* ((node (org-roam-node-at-point))
         (title (or (org-roam-node-title node)
                    (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
         (tags (org-roam-node-tags node))
         (filename (org-roam--sanitize-filename title))
         (output-dir "~/code/web/digital-garden/content/")
         (output-file (expand-file-name (concat filename ".md") output-dir)))

    ;; Ensure output directory exists
    (unless (file-directory-p output-dir)
      (make-directory output-dir t))

    ;; Export to markdown string
    (let ((markdown-content (org-export-as 'md nil nil nil '(:with-toc nil :with-tags nil))))
      (with-temp-file output-file
        (insert (org-roam--format-quartz-frontmatter title tags))
        (insert "\n")
        (insert markdown-content)))

    (message "Exported to: %s" output-file)))


;; ╺┳┓╻┏━┓┏━┓┏┓ ╻  ┏━╸╺┳┓
;;  ┃┃┃┗━┓┣━┫┣┻┓┃  ┣╸  ┃┃
;; ╺┻┛╹┗━┛╹ ╹┗━┛┗━╸┗━╸╺┻┛
;; Disabled

;; (after! cdlatex
;;   (map! :map cdlatex-mode-map
;;         :i "TAB" #'cdlatex-tab)

;;   (setq cdlatex-math-symbol-alist ; expand when prefixed with `
;;         '((?e ("\\varepsilon" "\\epsilon"))
;;           (?f ("\\varphi" "\\phi"))
;;           (?0 ("\\varnothing" "\\emptyset"))
;;           (?> ("\\to" "\\implies"))
;;           (?= ("\\iff" "\\equiv"))
;;           (?| ("\\mid" "\\vert"))
;;           (?: ("\\coloneqq")))
;;         cdlatex-math-modify-alist ; modify text with '
;;         ;; key mathcmd textcmd type rmdot it
;;         '((?b "\\mathbb" nil t nil nil)
;;           (?c "\\mathcal" nil t nil nil)
;;           (?f "\\mathbf" nil t nil nil)
;;           (?m "\\mathrm" nil t nil nil)
;;           (?r "\\mathrel" nil t nil nil)
;;           (?s "\\mathsf" nil t nil nil)
;;           (?o "\\operatorname" nil t nil nil))
;;         cdlatex-command-alist ; expand with <TAB>
;;         ;; keyword docstring replace hook args textflag mathflag
;;         '(("eqn" "Insert an EQUATION* environment template" "" cdlatex-environment ("equation*") t nil)
;;           ("aln" "Insert an ALIGN* environment template" "" cdlatex-environment ("align*") t nil)
;;           ("sum" "Insert \\sum\\limits_{}^{}" "\\sum\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
;;           ("prod" "Insert \\prod\\limits_{}^{}" "\\prod\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
;;           ("bun" "Insert \\bigcup\\limits_{}^{}" "\\bigcup\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
;;           ("bin" "Insert \\bigcap\\limits_{}^{}" "\\bigcap\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
;;           ("lim" "Insert \\lim_\\limits{{} \to {}}" "\\lim_\\limits{{?} \to {}}" cdlatex-position-cursor nil nil t)
;;           ("sr" "Insert {}^2" "{?}^2" cdlatex-position-cursor nil nil t)
;;           ("cb" "Insert {}^3" "{?}^3" cdlatex-position-cursor nil nil t)
;;           ("op" "Insert \\operatorname{}()" "\\operatorname{?}()" cdlatex-position-cursor nil nil t))))


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


;; Keybinding suggestion (optional)
;; (define-key org-mode-map (kbd "C-c C-e q") 'org-roam-export-to-quartz)

;; (use-package! eaf
;;   :config
;;   (setq browse-url-browser-function 'eaf-open-browser)
;;   (defalias 'browse-web #'eaf-open-browser)
;;   (setq eaf-browser-enable-adblocker "true")
;;   (setq eaf-browser-continue-where-left-off t)
;;   (setq eaf-browser-default-search-engine "duckduckgo")
;;   (setq eaf-browse-blank-page-url "https://duckduckgo.com")
;;   (setq eaf-browser-default-zoom "3")
;;   (add-to-list 'load-path "~/.config/emacs/.local/straight/build-30.1/emacs-application-framework/")
;;   (require 'eaf)
;;   (require 'eaf-browser)
;;   (require 'eaf-pdf-viewer)
;;   (require 'eaf-music-player)
;;   (require 'eaf-video-player)
;;   (require 'eaf-js-video-player)
;;   (require 'eaf-image-viewer)
;;   (require 'eaf-rss-reader)
;;   (require 'eaf-markdown-previewer)
;;   (require 'eaf-org-previewer)
;;   (require 'eaf-git)
;;   (require 'eaf-file-manager)
;;   (require 'eaf-mindmap)
;;   (require 'eaf-system-monitor)
;;   (require 'eaf-2048)
;;   (require 'eaf-evil)
;;   (defun eaf-org-open-file (file &optional link))

;;   (require 'eaf-evil)
;;   (define-key key-translation-map (kbd "SPC")
;;               (lambda (prompt)
;;                 (if (derived-mode-p 'eaf-mode)
;;                     (pcase eaf--buffer-app-name
;;                       ("browser" (if  eaf-buffer-input-focus
;;                                      (kbd "SPC")
;;                                    (kbd eaf-evil-leader-key)))
;;                       ("pdf-viewer" (kbd eaf-evil-leader-key))
;;                       ("image-viewer" (kbd eaf-evil-leader-key))
;;                       (_  (kbd "SPC")))
;;                   (kbd "SPC"))))
;;   )


