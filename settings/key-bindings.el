;;; keybindings --- setup keybindings -*- lexical-binding: t; -*-
;;; Commentary:
;;; no commentary
;;; Code:
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c g") 'magit-status)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)


(provide 'key-bindings)
;;; key-bindings.el ends here
