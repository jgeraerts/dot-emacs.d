;;; init --- Emacs Initialization File
;;; Commentary:
;;;    make flycheck happy


;;; This caused emacs freeze when my laptop is disconnected from the DC
(setq w32-get-true-file-attributes nil)

;;; Code:
;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)    
    (add-to-list 'load-path project)))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


(require 'setup-package)

(defun init--install-packages ()
  (packages-install
    '(better-defaults
      paredit
      idle-highlight-mode
      ido-ubiquitous
      flx-ido
      ido-vertical-mode
      find-file-in-project
      smex
                                        ;scpaste
      company
      cider
      clojure-mode
      clojure-mode-extra-font-locking
      clj-refactor
      rainbow-delimiters
      graphviz-dot-mode
      magit
      slamhound
      projectile
      zenburn-theme
      fill-column-indicator
      expand-region
      powerline
      smart-mode-line
      discover
      discover-my-major
      which-key
      markdown-mode
      flycheck
      company-jedi
      smooth-scrolling
      undo-tree
      whitespace-cleanup-mode)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(require 'sane-defaults)

(require 'powerline)
(require 'fill-column-indicator) ;; line indicating some edge column
(require 'rainbow-delimiters)
(require 'cider)
(require 'which-key)
(require 'flx-ido)
(require 'company)

(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

;https://www.emacswiki.org/emacs/FillColumnIndicator#toc11
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda ()
    (if (and
         (not (string-match "^\*.*\*$" (buffer-name)))
         (not (eq major-mode 'dired-mode)))
        (fci-mode 1))))

(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; flx-ido looks better with ido-vertical-mode
(require 'ido-vertical-mode)
(ido-vertical-mode)
; up-down works better in vertical mode
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(load-theme 'zenburn t)
(prefer-coding-system 'utf-8-unix)
(projectile-global-mode)
(global-linum-mode t)               ; Always show line numbers on left
(global-fci-mode 1)
(powerline-default-theme)
(show-paren-mode)
(which-key-mode)


(add-to-list 'company-backends 'company-jedi)

;; Configure nrepl.el
;(setq nrepl-hide-special-buffers t)
(setq cider-repl-popup-stacktraces-in-repl t)
(setq nrepl-popup-on-error nil) ; Don't popup new buffer for errors (show in nrepl buffer)
(setq cider-repl-history-file "~/.emacs.d/nrepl-history")
(setq nrepl-log-messages t)

(setq TeX-engine 'xetex)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)
(setq linum-format "%4d ") ; Line numbers gutter should be four characters wide
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(setq column-number-mode t)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

(setq cljr-magic-require-namespaces
      '(("io"   . "clojure.java.io")
        ("set"  . "clojure.set")
        ("str"  . "clojure.string")
        ("walk" . "clojure.walk")
        ("zip"  . "clojure.zip")
        ("time" . "clj-time.core")
        ("log"  . "clojure.tools.logging")
        ("json" . "cheshire.core")
        ("jdbc" . "clojure.java.jdbc")
        ("comp" . "com.stuartsierra.component")))


(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(add-hook 'after-init-hook #'global-flycheck-mode)
;; Some default eldoc facilities
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'subword-mode)
;; company mode
(add-hook 'cider-repl-mode-hook 'company-mode)
(add-hook 'cider-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'idle-highlight-mode)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(define-key clojure-mode-map (kbd "C-c M-y") 'cider-namespace-refresh)

;;; whitespace setup
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100)

(eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode))

(provide 'init)
