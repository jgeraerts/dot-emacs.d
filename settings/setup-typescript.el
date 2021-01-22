(defvar nvm-version)

(use-package typescript-mode
  :ensure t
  :pin "MELPA")

(defun my-typescript-hook ()
  (nvm-use nvm-version)
  (lsp))

(add-hook 'typescript-mode-hook 'my-typescript-hook)

(use-package ng2-mode
  :ensure t)

(provide 'setup-typescript)
