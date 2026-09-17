;;; init --- Emacs Initialization File -*- lexical-binding: t; -*-
;;; Commentary:
;;;

;;; Code:

(defvar nvm-version "16.14.2")

(defun load-if-exists (f)
  (if (file-exists-p (expand-file-name f))
      (load-file (expand-file-name f))))

(load-if-exists "~/.secrets.el")

(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Don't beep. Don't visible-bell (fails on el capitan). Just blink the modeline on errors.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))

(setq w32-get-true-file-attributes nil)
(setq inhibit-startup-message t)
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(pixel-scroll-precision-mode)

; set default directory and use temporary directory
(setq default-directory "~/")

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix temporary-file-directory)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
(setq tramp-backup-directory-alist backup-directory-alist)
; read process output
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; dash: list/sequence manipulation library, used as a dependency by many packages
(use-package dash
  :ensure t)


;; diminish: hide or abbreviate minor-mode indicators in the mode line
(use-package diminish
  :ensure t
  :config
  (diminish 'subword-mode))


(require 'setup-package)

(use-package better-defaults :ensure t :defer t) ;; a handful of saner built-in defaults (uniquify, no ido, etc.)
(use-package browse-kill-ring :ensure t :defer t) ;; browse and select from the kill-ring
(use-package company :ensure t :defer t) ;; auto-completion framework
(use-package company-go :ensure t :defer t) ;; company backend for Go
(use-package discover :ensure t :defer t) ;; discoverability popups for context-specific commands
(use-package discover-my-major :ensure t :defer t) ;; list keybindings available in the current major mode
(use-package dockerfile-mode :ensure t :defer t) ;; major mode for Dockerfiles
(use-package edn :ensure t :defer t) ;; read/write EDN data (used by clj-refactor)
(use-package expand-region :ensure t :defer t) ;; incrementally expand the selected region by syntax
(use-package find-file-in-project :ensure t :defer t) ;; fuzzy-find files within a project
(use-package flycheck :ensure t :defer t) ;; on-the-fly syntax checking
(use-package flycheck-clojure :ensure t :defer t) ;; flycheck checker for Clojure (via cider)
(use-package flycheck-pos-tip :ensure t :defer t) ;; show flycheck errors in a tooltip at point
(use-package go-mode :ensure t :defer t) ;; major mode for Go
(use-package graphviz-dot-mode :ensure t :defer t) ;; major mode for Graphviz .dot files
(use-package hydra :ensure t :defer t) ;; define bindable chains of commands ("hydras")
(use-package idle-highlight-mode :ensure t :defer t) ;; highlight other occurrences of the symbol at point when idle
(use-package inflections :ensure t :defer t) ;; pluralize/singularize words (used by clj-refactor)
(use-package markdown-mode :ensure t :defer t) ;; major mode for Markdown
(use-package multi-term :ensure t :defer t) ;; manage multiple terminal buffers
(use-package paredit :ensure t :defer t) ;; kept installed for reference; smartparens is used instead
(use-package rainbow-delimiters :ensure t :defer t) ;; color matching parens/brackets by nesting depth
(use-package ripgrep :ensure t :defer t) ;; run ripgrep searches from Emacs
(use-package smart-mode-line :ensure t :defer t) ;; a cleaner, more compact mode line
(use-package smooth-scrolling :ensure t :defer t) ;; keep the cursor away from the window edges when scrolling
(use-package whitespace-cleanup-mode :ensure t :defer t) ;; only clean up whitespace on lines you actually touched
(use-package yaml-mode :ensure t :defer t) ;; major mode for YAML
(use-package yasnippet :ensure t :defer t) ;; snippet expansion
(use-package yasnippet-snippets :ensure t :defer t) ;; a collection of ready-made snippets
(use-package zenburn-theme :ensure t :defer t) ;; the color theme loaded below

(require 'sane-defaults)
(load-theme 'zenburn t)
(sml/setup)
(setq use-package-always-ensure t)

(require 'rainbow-delimiters)
(require 'key-bindings)
(require 'mode-mappings)
(require 'setup-hippie)
(require 'setup-flycheck)
(require 'setup-company)
(require 'setup-yasnippet)
(require 'setup-python)
(require 'setup-typescript)
(require 'setup-org)
(require 'browse-kill-ring)

;; undo-tree: visualize undo history as a tree instead of a linear stack (C-x u)
(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (undo-tree-visualizer-timestamps t))

;; editorconfig: honor .editorconfig files for per-project indentation/whitespace rules
(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;; feature-mode: major mode for Cucumber/Gherkin .feature files
(use-package feature-mode
  :ensure t
  :defer t)

;; nvm: switch Node.js versions from within Emacs (not on MELPA, installed via :vc)
(use-package nvm
  :vc (:url "https://github.com/rejeep/nvm.el" :rev :newest))

;; platformio-mode: PlatformIO (embedded/Arduino) build integration
(use-package platformio-mode
  :ensure t
  :defer t)

;; unicode-fonts: better Unicode glyph coverage via font fallback
(use-package unicode-fonts
   :ensure t
   :config
    (unicode-fonts-setup))

;; exec-path-from-shell: import PATH/env vars from the user's shell
;; (needed since a macOS GUI Emacs doesn't inherit the shell's environment)
(use-package exec-path-from-shell
  :ensure t
  :defer f
  ;:custom
  ;(exec-path-from-shell-arguments '("-l"))
  )

(when is-mac
  ;(setq mac-control-modifier 'meta)
  ;(setq mac-command-modifier 'control)
  (when (window-system)
    (exec-path-from-shell-initialize)
    (add-to-list 'default-frame-alist '(fullscreen . maximized))
    (set-face-attribute 'default nil :font "Monaco-12")
    (if (version< "27.0" emacs-version)
        (set-fontset-font
         "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
      (set-fontset-font
       t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))))

;; envrc: direnv integration, applies a project's .envrc environment per-buffer
(use-package envrc
  :hook (after-init . envrc-global-mode))

;; which-key: show available keybindings after a prefix key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode)

;; lsp-mode: Language Server Protocol client
(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration) (c-mode . lsp))
  :commands lsp
  :ensure t)

;; lsp-ui: UI extras for lsp-mode (sideline diagnostics, peek, imenu)
(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t)

;; treesit-auto: automatically install tree-sitter grammars and remap
;; major modes to their -ts- equivalents when available.
;; python-mode and python-ts-mode are siblings under python-base-mode, not
;; parent/child, so anything hooked to `python-mode' won't fire once remapped -
;; hook `python-base-mode' instead to cover both.
(use-package treesit-auto
  :ensure t
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; lsp-pyright: Python language server support (via basedpyright) for lsp-mode
(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "basedpyright") ;; or basedpyright
  :hook (python-base-mode . (lambda ()
                               (require 'lsp-pyright)
                               (lsp))))  ; or lsp-deferred


;; (use-package ccls
;;   :ensure t
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;          (lambda () (require 'ccls) (lsp))))


;; vertico: vertical completion UI for the minibuffer (replaces ido/helm's UI role)
(use-package vertico
  :ensure t
  :init (vertico-mode)
  :bind (:map vertico-map
              ;; ido/helm-style "go up one directory" in find-file
              ("C-l" . vertico-directory-up)))

;; orderless: completion style that matches space-separated terms in any order
;; (e.g. "set comp" matches "settings/setup-company.el")
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; marginalia: annotations in the minibuffer (file sizes, docstrings, keybindings, ...)
(use-package marginalia
  :ensure t
  :init (marginalia-mode))

;; consult: enhanced versions of common commands (buffer switching, ripgrep/grep, ...)
;; built on top of the standard minibuffer, so it renders through vertico automatically
(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-c p g" . consult-ripgrep)))

;; embark: contextual actions on the thing at point or the minibuffer candidate
;; at point (C-. to act, C-; for the single most likely action, C-h B to see
;; what's available)
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; embark-consult: makes embark's collect/export actions work with consult's
;; commands (e.g. turn a consult-ripgrep session into an editable grep buffer)
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; projectile: project-aware navigation and commands (find file, grep, switch project, ...)
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (projectile-mode +1)
  :custom
  ;; use plain `completing-read', which vertico/orderless/marginalia enhance
  (projectile-completion-system 'default)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;; add-node-modules-path: add a project's node_modules/.bin to exec-path
(use-package add-node-modules-path
  :hook ((typescript-mode . add-node-modules-path)))

;; magit: Git porcelain
(use-package magit
  :ensure t)

;; smerge-mode: resolve merge conflicts (a hydra is layered on top below)
(use-package smerge-mode
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (sm))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

;; multiple-cursors: edit multiple points in a buffer simultaneously
(use-package multiple-cursors
  :config
  (defhydra jog-multiple-cursors-hydra (:hint nil)
  "
     ^Up^            ^Down^        ^Miscellaneous^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("q" nil)))

;; smartparens: structured, balanced-parens editing (slurp/barf/wrap/splice/...)
(use-package smartparens
  :pin "MELPA"
  ;:ensure smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode cider-repl-mode) ;; add `smartparens-mode` to these hooks
  :diminish smartparens-mode
  :config
  ;; load default config
  (require 'smartparens-config)
  ;; use paredit-style keybindings (slurp/barf/wrap/splice/etc.)
  (sp-use-paredit-bindings)
  (define-key smartparens-mode-map (kbd "C-j") 'sp-newline)
  ;; enable smartparens in the minibuffer for `eval-expression', like paredit was
  (defun conditionally-enable-smartparens-mode ()
    (if (eq this-command 'eval-expression)
        (smartparens-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode))

;; Setup environment variables from the user's shell.

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(setq browse-kill-ring-quit-action 'save-and-restore)

(eval-after-load 'js2-mode '(require 'setup-js2-mode))


(global-display-fill-column-indicator-mode)


(show-paren-mode)

(which-key-setup-side-window-right)

(eval-after-load 'clojure-mode '(require 'setup-clojure-mode))

(setq TeX-engine 'xetex)
;(setq linum-format "%4d ") ; Line numbers gutter should be four characters wide
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(setq column-number-mode t)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

(add-hook 'after-init-hook 'yas-global-mode)

;;; whitespace setup
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100)

(add-hook 'go-mode-hook #'lsp-deferred)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(require 'tramp)
(setq tramp-default-proxies-alist (quote (("home\\.geraerts\\.local\\'" "\\`root\\'" "/plink:pi@%h:")
                                          (".*trendminer\\.net" "root" "/ssh:developer@%h:"))))
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

;; rust-mode: major mode for Rust
(use-package rust-mode
  :ensure)

;; rustic: Rust development environment on top of rust-mode (rustfmt, cargo, lsp integration)
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (setq rust-mode-treesitter-derive t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(add-hook 'rust-mode-hook 'lsp-deferred)

(server-start)

(provide 'init)
;;; Local Variables:
;;; byte-compile-warnings: (not free-vars)
;;; init.el ends here
