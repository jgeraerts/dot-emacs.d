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

;; Swap the buffer in the selected window with the one in the given direction,
;; keeping window layout/sizes intact (pairs with windmove's Shift+direction
;; navigation, set up in sane-defaults, by adding Control)
(global-set-key (kbd "S-C-<up>") 'windmove-swap-states-up)
(global-set-key (kbd "S-C-<down>") 'windmove-swap-states-down)
(global-set-key (kbd "S-C-<left>") 'windmove-swap-states-left)
(global-set-key (kbd "S-C-<right>") 'windmove-swap-states-right)

(load-theme 'zenburn t)
(sml/setup)
(setq use-package-always-ensure t)

(require 'rainbow-delimiters)
(require 'key-bindings)
(require 'mode-mappings)
(require 'setup-hippie)
(require 'setup-flycheck)
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

;; eglot: built-in LSP client, used for Python (via basedpyright) and C.
;; Go and Rust are set up in their own sections further down.
(use-package eglot
  :hook ((python-base-mode . eglot-ensure)
         (c-mode . eglot-ensure))
  ;; `M-.' (xref-find-definitions) resolves to the declared/static type, so on
  ;; a Protocol-typed symbol it lands on the protocol itself rather than a
  ;; concrete implementation. `eglot-find-implementation' issues the separate
  ;; LSP "go to implementation" request to get there instead.
  :bind (:map eglot-mode-map
              ("C-c e i" . eglot-find-implementation)
              ("C-c e d" . eglot-find-declaration)
              ("C-c e t" . eglot-find-typeDefinition)
              ("C-c e r" . eglot-rename)
              ("C-c e ?" . xref-find-references)
              ("C-c e a" . eglot-code-actions)
              ("C-c e o" . eglot-code-action-organize-imports)
              ("C-c e f" . eglot-format-buffer)
              ("C-c e q" . eglot-reconnect)
              ("C-c e Q" . eglot-shutdown))
  :config
  (add-to-list 'eglot-server-programs
               '(python-base-mode . ("basedpyright-langserver" "--stdio"))))

;; ruff flags trailing whitespace - strip it automatically on save.
(add-hook 'python-base-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))


;; corfu: in-buffer completion popup (replaces company), driven by
;; completion-at-point-functions - pairs naturally with eglot, which
;; contributes its own capf without needing a company backend bridge
(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  ;:custom
  ;(corfu-auto f)
  ;(corfu-auto-delay 0.2)
  ;(corfu-cycle f)
  )

;; corfu-popupinfo: show documentation for the selected candidate,
;; the corfu equivalent of company's quickhelp popup
(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :init (corfu-popupinfo-mode))

;; cape: extra completion-at-point-functions layered in front of corfu
;; (dabbrev, file, keyword, ...), covering what company's bundled
;; backends used to provide
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :bind (("C-c y" . yas-expand)))

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
         ("C-c p g" . consult-ripgrep)
         ("M-g e" . consult-error)
         ("M-g g" . consult-goto-line)
         ("M-g h" . consult-org-heading)
         ("M-g i" . consult-imenu)
         ("M-g k" . consult-global-mark)
         ("M-g l" . consult-line)
         ("M-g m" . consult-mark)
         ("M-g o" . consult-outline)
         ("M-g I" . consult-imenu-multi)))

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

;; git-gutter: show added/modified/deleted lines in the fringe/margin,
;; and jump between/stage/revert hunks
(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode +1)
  :bind (("C-c v p" . git-gutter:previous-hunk)
         ("C-c v n" . git-gutter:next-hunk)
         ("C-c v s" . git-gutter:stage-hunk)
         ("C-c v r" . git-gutter:revert-hunk)))

;; compile: run shell commands (e.g. `pre-commit run --all-files') in a
;; buffer that parses file:line:col output into jumpable links.
;; Bound to `projectile-compile-project' rather than plain `compile' so it
;; always runs from the project root, regardless of which buffer is current.
(use-package compile
  :ensure nil
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-skip-threshold 2)
  (compilation-always-kill t)
  :bind ("C-c c" . projectile-compile-project)
  :config
  ;; ruff prints the location on its own line, e.g.:
  ;;   --> src/foo/bar.py:88:32
  ;; rather than a leading "file:line:col:", so the default `gnu' regexp
  ;; misses it - add a matcher for ruff's rustc-style "--> file:line:col".
  (add-to-list 'compilation-error-regexp-alist 'ruff)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(ruff "^\\s-*--> \\(.*\\):\\([0-9]+\\):\\([0-9]+\\)$" 1 2 3)))

;; ansi-color: render ANSI color codes in the compilation buffer instead of
;; showing raw escape sequences (pre-commit colors its pass/fail output)
(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

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
  ;; paredit-style bindings steal M-? for `sp-convolute-sexp', shadowing the
  ;; global `xref-find-references' binding used by eglot; give it back.
  (define-key smartparens-mode-map (kbd "M-?") 'xref-find-references)
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

;; treesit-auto remaps `go-mode' to the built-in `go-ts-mode', but the two
;; are sibling modes (not parent/child) so anything hooked to `go-mode' won't
;; fire once remapped - hook both.
(add-hook 'go-mode-hook #'eglot-ensure)
(add-hook 'go-ts-mode-hook #'eglot-ensure)

;; Set up before-save hooks to format buffer and organize imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun eglot-go-install-save-hooks ()
  (add-hook 'before-save-hook #'eglot-format-buffer nil t)
  (add-hook 'before-save-hook #'eglot-code-action-organize-imports nil t))
(add-hook 'go-mode-hook #'eglot-go-install-save-hooks)
(add-hook 'go-ts-mode-hook #'eglot-go-install-save-hooks)

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
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . eglot-code-actions)
              ("C-c C-c r" . eglot-rename)
              ("C-c C-c q" . eglot-reconnect)
              ("C-c C-c Q" . eglot-shutdown))
  :config
  (setq rustic-lsp-client 'eglot)

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

(add-hook 'rust-mode-hook 'eglot-ensure)

(server-start)

(provide 'init)
;;; Local Variables:
;;; byte-compile-warnings: (not free-vars)
;;; init.el ends here
