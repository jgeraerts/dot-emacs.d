;;; Reduce GC frequency during startup
(setq gc-cons-threshold (* 200 1024 1024))
;(setq package-enable-at-startup nil)  ; breaks with diminish
;;; Log startup stats
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
