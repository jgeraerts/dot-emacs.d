;;; init --- Emacs Initialization File
;;; Commentary:
;;;

;;; Code:
;(package-initialize)
(setq w32-get-true-file-attributes nil)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))


;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'appearance)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(require 'setup-package)

(defun init--install-packages ()
  (packages-install
   '(
     add-node-modules-path
     ag
     better-defaults
     browse-kill-ring
     cider
     cider-hydra
     company
     company-flow
     company-go
     discover
     discover-my-major
     dockerfile-mode
     edn
     expand-region
     fill-column-indicator
     find-file-in-project
     flow-minor-mode
     flx-ido
     flycheck
     flycheck-flow
     flycheck-clojure
     flycheck-pos-tip
     go-mode
     graphviz-dot-mode
     helm
     helm-ag
     helm-descbinds
     helm-mt
     helm-projectile
     helm-cider
     helm-cider-history
     hydra
     idle-highlight-mode
     ido-vertical-mode
     inflections
     js2-refactor
     lsp-mode
     lsp-ui
     magit
     markdown-mode
     multi-term
     multiple-cursors
     neotree
     paredit
     projectile
     puppet-mode
     rainbow-delimiters
     react-snippets
     ripgrep
     rjsx-mode
     sbt-mode
     scala-mode
     smart-mode-line
     smartparens
     smex
     smooth-scrolling
     tern
     terraform-mode
     undo-tree
     use-package
     which-key
     whitespace-cleanup-mode
     yaml-mode
     yasnippet
     yasnippet-snippets
     zenburn-theme
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(load-theme 'zenburn t)

(sml/setup)

(require 'neotree)
(require 'sublimity)
(require 'sane-defaults)
(require 'fill-column-indicator) ;; line indicating some edge column
(require 'rainbow-delimiters)
(require 'which-key)
(require 'key-bindings)
(require 'mode-mappings)
(require 'smartparens-config)
(require 'helm-config)
(require 'setup-hippie)
(require 'setup-paredit)
(require 'setup-flycheck)
(require 'setup-company)
(require 'setup-yasnippet)
(require 'setup-python)
(require 'setup-typescript)
(require 'browse-kill-ring)
(require 'restclient)
(require 'smex)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package org-journal
  :ensure t
  :defer t
  :custom
  (org-journal-dir "~/org/journal/")
  (org-journal-date-format "%A, %d %B %Y"))

(use-package feature-mode
  :ensure t
  :defer t)

;; Setup environment variables from the user's shell.
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(setq browse-kill-ring-quit-action 'save-and-restore)


(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'smex '(smex-initialize))
(sublimity-mode 1)

;https://www.emacswiki.org/emacs/FillColumnIndicator#toc11
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda ()
    (if (and
         (not (string-match "^\*.*\*$" (buffer-name)))
         (not (eq major-mode 'dired-mode)))
        (fci-mode 1))))

(require 'projectile)

(with-eval-after-load "projectile"
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(helm-projectile-on)
(global-fci-mode 1)
(show-paren-mode)
(which-key-mode)
(which-key-setup-side-window-right)

(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(eval-after-load 'clojure-mode '(require 'setup-clojure-mode))

(setq TeX-engine 'xetex)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)
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

(eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode))

(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))

(require 'tramp)
(setq tramp-default-proxies-alist (quote (("home\\.geraerts\\.local\\'" "\\`root\\'" "/plink:pi@%h:")
                                     (".*trendminer\\.net" "root" "/ssh:developer@%h:")
                                     )))

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

(server-start)

(provide 'init)
;;; Local Variables:
;;; byte-compile-warnings: (not free-vars)
;;; init.el ends here
