(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;(add-to-list 'package-archives
;           '("marmalade" . "http://marmalade-repo.org/packages/") t)
;(add-to-list 'package-archives
;             '("melpa" . "http://melpa-stable.milkbox.net/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages
  '(better-defaults
    paredit
    idle-highlight-mode
    ido-ubiquitous
    find-file-in-project
    smex
    ;scpaste
    company
    cider
    clojure-mode
    clojure-mode-extra-font-locking
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
    which-key)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'powerline)
(require 'fill-column-indicator) ;; line indicating some edge column
(require 'rainbow-delimiters)
(require 'cider)
(require 'which-key)

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
(load-theme 'zenburn t)
(prefer-coding-system 'utf-8-unix)
(projectile-global-mode)
(global-linum-mode t)               ; Always show line numbers on left
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(windmove-default-keybindings) ;; Shift+direction
(global-fci-mode 1)
(powerline-default-theme)
(show-paren-mode)
(which-key-mode)
(fset 'yes-or-no-p 'y-or-n-p)  ;; only type `y` instead of `yes`

;; Configure nrepl.el
;(setq nrepl-hide-special-buffers t)
(setq cider-repl-popup-stacktraces-in-repl t)
(setq nrepl-popup-on-error nil) ; Don't popup new buffer for errors (show in nrepl buffer)
(setq cider-repl-history-file "~/.emacs.d/nrepl-history")
(setq nrepl-log-messages t)
; prevent creating lockfiles so that directory timestamps are not
; updated. This triggers lein-test-refresh to do a test run before
; editing a file
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq tab-width 2
      indent-tabs-mode nil)
(setq fill-column 80) ;; M-q should fill at 80 chars, not 75
(setq-default indent-tabs-mode nil)      ;; no tabs!
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
(setq inhibit-startup-message t)
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(setq column-number-mode t)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

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
