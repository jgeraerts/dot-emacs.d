;;; keybindings --- setup keybindings
;;; Commentary:
;;; no commentary
;;; Code:
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)


(provide 'key-bindings)
;;; key-bindings.el ends here
