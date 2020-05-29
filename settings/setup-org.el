;;; org-mode.el --- org mode configuration/setup
;;; Commentary:
;;; no comments
;;; Code:

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(use-package org
  :ensure org-plus-contrib
  :init
  (setq org-capture-templates
              (quote (("t" "todo" entry (file "~//org/refile.org")
                       "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                      ("r" "respond" entry (file "~//org/refile.org")
                       "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                      ("n" "note" entry (file "~//org/refile.org")
                       "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                      ("j" "Journal" entry (file+datetree "~//org/diary.org")
                       "* %?\n%U\n" :clock-in t :clock-resume t)
                      ("w" "org-protocol" entry (file "~//org/refile.org")
                       "* TODO Review %c\n%U\n" :immediate-finish t)
                      ("m" "Meeting" entry (file "~//org/refile.org")
                       "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                      ("p" "Phone call" entry (file "~//org/refile.org")
                       "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                      ("h" "Habit" entry (file "~//org/refile.org")
                       "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
                      ("l" "Link" entry (file+headline "~/org/links.org" "Links")
                       "* %a %^g\n %?\n %T\n %i"))))
  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/refile.org")
  (setq org-agenda-files (quote ("~/org")))
                                        ;Targets include this file and any file contributing to the agenda - up to 9 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))
  
                                        ; Use full outline paths for refile targets - we file directly with IDO
  (setq org-refile-use-outline-path t)
  
                                        ; Targets complete directly with IDO
  (setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :pin org)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-journal
  :ensure t
  :defer t
  :custom
  (org-journal-dir "~/org/journal/")
  (org-journal-date-format "%A, %d %B %Y"))

(require 'org-protocol)

(provide 'setup-org)
;;; setup-org.el ends here
