(require 'clojure-mode)
(require 'clojure-mode-extra-font-locking)
(require 'clj-refactor)

(defadvice clojure-test-run-tests (before save-first activate)
  (save-buffer))

(defadvice nrepl-load-current-buffer (before save-first activate)
  (save-buffer))

(defun enable-clojure-mode-stuff ()
  (clj-refactor-mode 1))

(add-hook 'clojure-mode-hook 'enable-clojure-mode-stuff)

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


(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

;; Indent and highlight more commands
(put-clojure-indent 'match 'defun)

;; Hide nrepl buffers when switching buffers (switch to by prefixing with space)
(setq nrepl-hide-special-buffers t)

;; Enable error buffer popping also in the REPL:
(setq cider-repl-popup-stacktraces t)

;; Specify history file
(setq cider-history-file "~/.emacs.d/nrepl-history")

;; auto-select the error buffer when it's displayed
(setq cider-auto-select-error-buffer t)

;; Prevent the auto-display of the REPL buffer in a separate window after connection is established
(setq cider-repl-pop-to-buffer-on-connect nil)

;; Pretty print results in repl
(setq cider-repl-use-pretty-printing t)

;; Don't prompt for symbols
(setq cider-prompt-for-symbol nil)

;; Enable eldoc in Clojure buffers
(add-hook 'cider-mode-hook #'eldoc-mode)

;; Some expectations features

;(require 'clj-autotest)

(require 'flycheck-clojure)

(defun my-cider-mode-enable-flycheck ()
  ;; (when (and (s-ends-with-p ".clj" (buffer-file-name))
  ;;            (not (s-ends-with-p "/dev/user.clj" (buffer-file-name))))
  ;;   (flycheck-mode 1))
  )

(add-hook 'cider-mode-hook 'my-cider-mode-enable-flycheck)

(eval-after-load 'flycheck '(add-to-list 'flycheck-checkers 'clojure-cider-eastwood))

;; Make q quit out of find-usages to previous window config

(defadvice cljr-find-usages (before setup-grep activate)
  (window-configuration-to-register ?$))


(provide 'setup-clojure-mode)
