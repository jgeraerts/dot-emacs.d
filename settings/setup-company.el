(require 'company)
(require 'company-tern)
(require 'company-jedi)

(add-to-list 'company-backends 'company-jedi)
(add-to-list 'company-backends 'company-tern)
(add-hook 'after-init-hook 'global-company-mode)

(global-set-key (kbd "C-c y") 'company-yasnippet)

(provide 'setup-company)
