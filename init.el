;;; init --- Emacs Initialization File
;;; Commentary:
;;;


;(package-initialize)
(prefer-coding-system 'utf-8-unix)

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
     better-defaults
     browse-kill-ring
     cider
     clj-refactor
     clojure-mode
     clojure-mode-extra-font-locking
     company
     company-jedi
     dockerfile-mode
     discover
     discover-my-major
     edn
     expand-region
     fill-column-indicator
     find-file-in-project
     flx-ido
     flycheck
     flycheck-pos-tip
     flycheck-clojure
     go-mode
     graphviz-dot-mode
     hydra
     idle-highlight-mode
     ido-ubiquitous
     ido-vertical-mode
     inflections
     magit
     markdown-mode
     multiple-cursors
     paredit
     projectile
     puppet-mode
     rainbow-delimiters
     slamhound
     smart-mode-line
     smex
     smooth-scrolling
     undo-tree
     which-key
     whitespace-cleanup-mode
     yaml-mode
     yasnippet
     zenburn-theme
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(require 'sane-defaults)
(sml/setup)
(require 'fill-column-indicator) ;; line indicating some edge column
(require 'rainbow-delimiters)
(require 'which-key)
(require 'company)
(require 'key-bindings)

(eval-after-load 'ido '(require 'setup-ido))
(require 'setup-hippie)
(require 'setup-paredit)
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

(require 'smex)
(smex-initialize)

;https://www.emacswiki.org/emacs/FillColumnIndicator#toc11
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda ()
    (if (and
         (not (string-match "^\*.*\*$" (buffer-name)))
         (not (eq major-mode 'dired-mode)))
        (fci-mode 1))))

(load-theme 'zenburn t)
(projectile-global-mode)
;(global-linum-mode t)               ; Always show line numbers on left
(global-fci-mode 1)
;(powerline-default-theme)
(show-paren-mode)
(which-key-mode)
(require 'mode-mappings)

(add-to-list 'company-backends 'company-jedi)

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

(add-hook 'after-init-hook #'global-flycheck-mode)
;; Some default eldoc facilities
;;(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;(add-hook 'clojure-mode-hook 'paredit-mode)
;(add-hook 'nrepl-mode-hook 'subword-mode)
;; company mode
;(add-hook 'cider-repl-mode-hook 'company-mode)
;(add-hook 'cider-mode-hook 'company-mode)
;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;(add-hook 'prog-mode-hook 'idle-highlight-mode)



;;; whitespace setup
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100)

(eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode))

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


(provide 'init)
;;; init.el ends here
