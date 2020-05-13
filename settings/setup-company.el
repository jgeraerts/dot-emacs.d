;(use-package 'company-tern)

(require 'company)
;(require 'company-tern)
(require 'company-go)

(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

;(add-to-list 'company-backends 'company-tern)
(add-hook 'after-init-hook 'global-company-mode)

(global-set-key (kbd "C-c y") 'company-yasnippet)

(provide 'setup-company)
