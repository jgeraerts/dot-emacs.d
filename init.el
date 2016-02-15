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
    rainbow-delimiters
    graphviz-dot-mode
    magit
    slamhound
    projectile
    zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

(load-theme 'zenburn t)
(prefer-coding-system 'utf-8-unix)
(projectile-global-mode)
(global-linum-mode t)               ; Always show line numbers on left
(setq linum-format "%4d ") ; Line numbers gutter should be four characters wide
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(windmove-default-keybindings) ;; Shift+direction


(require 'cider)
;; Configure nrepl.el
;(setq nrepl-hide-special-buffers t)
(setq cider-repl-popup-stacktraces-in-repl t)
(setq nrepl-popup-on-error nil) ; Don't popup new buffer for errors (show in nrepl buffer)
(setq cider-repl-history-file "~/.emacs.d/nrepl-history")
(setq nrepl-log-messages t)

 
;; Some default eldoc facilities

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'subword-mode)

;; company mode
(add-hook 'cider-repl-mode-hook 'company-mode)
(add-hook 'cider-mode-hook 'company-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(setq TeX-engine 'xetex)

(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

; prevent creating lockfiles so that directory timestamps are not
; updated. This triggers lein-test-refresh to do a test run before
; editing a file
(setq create-lockfiles nil)

(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(define-key clojure-mode-map (kbd "C-c M-y") 'cider-namespace-refresh)
