
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa-stable.milkbox.net/packages/"))
(package-initialize)

(load-theme 'zenburn t)


(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages
  '(starter-kit
    starter-kit-lisp
    starter-kit-bindings
    company
    cider
    clojure-mode
    rainbow-delimiters
    graphviz-dot-mode
    midje-mode
    magit
    slamhound
    projectile)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(projectile-global-mode)

(require 'cider)
 
;; Configure nrepl.el
(setq nrepl-hide-special-buffers t)
(setq cider-repl-popup-stacktraces-in-repl t)
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

(define-key clojure-mode-map (kbd "C-c M-y") 'cider-namespace-refresh)

;(require 'sr-speedbar)

