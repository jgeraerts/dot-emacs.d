;;; setup-js2-mode.el --- tweak js2 settings -*- lexical-binding: t; -*-

(setq-default js2-allow-rhino-new-expr-initializer nil)
(setq-default js2-auto-indent-p nil)
(setq-default js2-enter-indents-newline nil)
(setq-default js2-global-externs '("Audio" "history" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location"))
(setq-default js2-idle-timer-delay 0.1)
(setq-default js2-indent-on-enter-key nil)
(setq-default js2-mirror-mode nil)
(setq-default js2-strict-inconsistent-return-warning nil)
(setq-default js2-auto-indent-p t)
(setq-default js2-include-node-externs t)
(setq-default js2-concat-multiline-strings 'eol)
(setq-default js2-rebind-eol-bol-keys nil)
(setq-default js2-basic-offset 2)
(setq-default js2-strict-trailing-comma-warning nil)
(setq-default js-switch-indent-offset 2)
(setq-default  js-indent-level 2)

;; Let flycheck handle parse errors
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning nil)

(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))

(require 'js2-mode)
(require 'js2-refactor)
(require 'rjsx-mode)




(js2r-add-keybindings-with-prefix "C-c C-m")

(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'js2-mode-hook 'add-node-modules-path)
(add-hook 'js2-mode-hook
             (lambda ()
               (add-hook 'xref-backend-functions
                         #'xref-js2-xref-backend nil t)))

(add-hook 'js2-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda () (delete-trailing-whitespace)))))

(add-hook 'js-mode-hook #'smartparens-mode)

;; enable rjsx mode when react is imported in a file


(require 'js2-imenu-extras)
(js2-imenu-extras-setup)

;; https://blog.binchen.org/posts/indent-jsx-in-emacs.html
(defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
  "Workaround 'sgml-mode and follow airbnb component style."
  (let* ((cur-line (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position))))
    (if (string-match "^\\( +\\)\/?> *$" cur-line)
      (let* ((empty-spaces (match-string 1 cur-line)))
        (replace-regexp empty-spaces
                        (make-string (- (length empty-spaces) sgml-basic-offset) 32)
                        nil
                        (line-beginning-position) (line-end-position))))))

(add-to-list 'projectile-globally-ignored-directories "node_modules")

(provide 'setup-js2-mode)
