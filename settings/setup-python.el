;;; -*- lexical-binding: t; -*-

;; Environments are activated via direnv (.envrc), not pyvenv. Restart any
;; active LSP workspace for the buffer when envrc (re)applies its environment,
;; so basedpyright doesn't keep using a stale interpreter after activation.
(defun my-envrc-restart-lsp-workspaces ()
  (when (bound-and-true-p lsp-mode)
    (dolist (workspace (lsp-workspaces))
      (lsp-workspace-restart workspace))))

(add-hook 'envrc-mode-hook #'my-envrc-restart-lsp-workspaces)

(provide 'setup-python)
