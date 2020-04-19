(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(provide 'setup-python)
