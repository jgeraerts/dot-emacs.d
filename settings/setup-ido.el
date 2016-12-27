;;; ido mode setup

(require 'ido)

(ido-mode t)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

(require 'flx-ido)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; flx-ido looks better with ido-vertical-mode
(require 'ido-vertical-mode)
(ido-vertical-mode)
; up-down works better in vertical mode
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)


(provide 'setup-ido)
