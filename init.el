;;; init --- Emacs Initialization File
;;; Commentary:
;;;

;;; Code:

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


(require 'sane-defaults)
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

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
;(require 'appearance)

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
     hydra
     idle-highlight-mode
     ido-vertical-mode
     inflections
     js2-refactor
     magit
     markdown-mode
     multi-term
     multiple-cursors
     paredit
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
     whitespace-cleanup-mode
     yaml-mode
     yasnippet
     yasnippet-snippets
     zenburn-theme)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(load-theme 'zenburn t)
(sml/setup)
(setq use-package-always-ensure t)

(require 'sublimity)
(require 'fill-column-indicator) ;; line indicating some edge column
(require 'rainbow-delimiters)
(require 'which-key)
(require 'key-bindings)
(require 'mode-mappings)
(require 'smartparens-config)
;(require 'helm-config)
(require 'setup-hippie)
(require 'setup-paredit)
(require 'setup-flycheck)
(require 'setup-company)
(require 'setup-yasnippet)
(require 'setup-python)
(require 'setup-typescript)
(require 'setup-org)
(require 'browse-kill-ring)
(require 'restclient)
(require 'smex)

(use-package dash)

(use-package diminish
  :config
  (diminish subword-mode))

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package feature-mode
  :ensure t
  :defer t)

(use-package nvm
  :ensure t
  :defer t))

(use-package platformio-mode
  :ensure t
  :defer t)

(use-package unicode-fonts
   :ensure t
   :config
    (unicode-fonts-setup))

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :custom
  (exec-path-from-shell-arguments '("-l")))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode)


(use-package lsp-mode :commands lsp
  :ensure t)
(use-package lsp-ui :commands lsp-ui-mode
  :ensure t)
(use-package company-lsp :commands company-lsp
  :ensure t)

(use-package irony
  :ensure t
  :hook ((c-mode . irony-mode))
  :commands (irony-mode))

(use-package irony-eldoc
  :ensure t
  :commands (irony-eldoc))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package helm
  :bind (("C-x b" . 'helm-mini)
         ("C-x C-f" .  'helm-find-files)))

(use-package helm-projectile
  :config (helm-projectile-on))

(use-package helm-ag)
(use-package helm-descbinds)
(use-package helm-mt)

;; Setup environment variables from the user's shell.
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





(global-fci-mode 1)
(show-paren-mode)

(which-key-setup-side-window-right)

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

;(eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode))

(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))

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

(server-start)

(provide 'init)
;;; Local Variables:
;;; byte-compile-warnings: (not free-vars)
;;; init.el ends here
