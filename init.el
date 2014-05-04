
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(load-theme 'zenburn t)


(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages
  '(starter-kit
    starter-kit-lisp
    starter-kit-bindings
    auto-complete
    ac-nrepl
    cider
    clojure-mode
    clojure-test-mode
    rainbow-delimiters
    graphviz-dot-mode
    midje-mode
    magit)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'cider)
 
;; Configure nrepl.el
(setq nrepl-hide-special-buffers t)
(setq cider-repl-popup-stacktraces-in-repl t)
(setq cider-repl-history-file "~/.emacs.d/nrepl-history")
 
;; Some default eldoc facilities

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
;; Repl mode hook
(add-hook 'nrepl-mode-hook 'subword-mode)

;; auto-complete
      ;; (require 'auto-complete-config)
      ;; (add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
      ;; (ac-config-default)

;; Auto completion for NREPL
;; (require 'ac-nrepl)
;;  (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;;  (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
;; (eval-after-load "auto-complete"
;; '(add-to-list 'ac-modes 'nrepl-mode))
;; (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)


(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(setq TeX-engine 'xetex)
