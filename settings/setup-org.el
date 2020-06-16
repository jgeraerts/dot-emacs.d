;;; org-mode.el --- org mode configuration/setup
;;; Commentary:
;;; no comments
;;; Code:

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(use-package org
  :ensure org-plus-contrib
  :init
  (setq org-capture-templates
              (quote (("t" "todo" entry (file "~//org/inbox.org")
                       "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                      ("r" "respond" entry (file "~//org/inbox.org")
                       "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                      ("n" "note" entry (file "~//org/inbox.org")
                       "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                      ("j" "Journal" entry (file+datetree "~//org/diary.org")
                       "* %?\n%U\n" :clock-in t :clock-resume t)
                      ("w" "org-protocol" entry (file "~//org/inbox.org")
                       "* TODO Review %c\n%U\n" :immediate-finish t)
                      ("m" "Meeting" entry (file "~//org/inbox.org")
                       "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                      ("p" "Phone call" entry (file "~//org/inbox.org")
                       "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                      ("h" "Habit" entry (file "~//org/inbox.org")
                       "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
                      ("l" "Link" entry (file+headline "~/org/links.org" "Links")
                       "* %a %^g\n %?\n %T\n %i"))))
  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/inbox.org")
  (setq org-agenda-files (quote ("~/org")))
                                        ;Targets include this file and any file contributing to the agenda - up to 9 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))
  
                                        ; Use full outline paths for refile targets - we file directly with IDO
  (setq org-refile-use-outline-path t)
  
                                        ; Targets complete directly with IDO
  (setq org-outline-path-complete-in-steps nil)
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
  
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold))))

  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
  
                                        ; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-completion-use-ido t)
  (setq org-indirect-buffer-display 'current-window)
  (setq org-tag-alist (quote ((:startgroup)
                              ("@errand" . ?e)
                              ("@office" . ?o)
                              ("@home" . ?H)
                              (:endgroup)
                              ("WAITING" . ?w)
                              ("HOLD" . ?h)
                              ("PERSONAL" . ?P)
                              ("WORK" . ?W)
                              ("ORG" . ?O)
                              ("NOTE" . ?n)
                              ("CANCELLED" . ?c)
                              ("FLAGGED" . ??))))
  
                                        ; Allow setting single tags without the menu
  (setq org-fast-tag-selection-single-key (quote expert))

                                        ; For tag searches ignore tasks with scheduled and deadline dates
  (setq org-agenda-tags-todo-honor-ignore-options t)
  (setq org-clock-idle-time 10)
  (setq org-clock-history-length 23)
  ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)
  ;(setq org-agenda-span 'day)
  (setq org-agenda-custom-commands
        (quote (("N" "Notes" tags "NOTE"
                 ((org-agenda-overriding-header "Notes")
                  (org-tags-match-list-sublevels t)))
                ("G" "Agenda"
                 ((agenda "")
                  (tags "REFILE"
                        ((org-agenda-overriding-header "Tasks to Refile")
                         (org-tags-match-list-sublevels nil)))
                  (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header "Project Next Tasks")
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                  (tags-todo "OVERHEAD"
                           ((org-agenda-overriding-header "Overhead tasks")
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep)))))))))
  :config (org-clock-persistence-insinuate)

  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :pin org)

(use-package org-bullets
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-journal
  :ensure t
  :defer t
  :custom
  (org-journal-dir "~/org/journal/")
  (org-journal-date-format "%A, %d %B %Y"))

(require 'org-protocol)

(use-package org-roam
  :ensure t
  :after org
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org/notes/")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(use-package bibtex-completion
  :ensure t)

(use-package helm-bibtex
  :ensure t)

(use-package org-ref
  :ensure t
  :after org)

(use-package org-download
  :ensure t
  :after org)

(provide 'setup-org)
;;; setup-org.el ends here
