(use-package typescript-mode
  :ensure t
  :pin "MELPA")

(use-package tide
  :ensure t
  :pin "MELPA"
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package ng2-mode
  :ensure t)

(provide 'setup-typescript)
