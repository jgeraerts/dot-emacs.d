(require 'company)
(require 'company-tern)
(require 'company-jedi)

(add-to-list 'company-backends 'company-jedi)
(add-to-list 'company-backends 'company-tern)
(add-to-list 'company-backends 'company-yasnippet)
(add-hook 'after-init-hook 'global-company-mode)

(provide 'setup-company)
