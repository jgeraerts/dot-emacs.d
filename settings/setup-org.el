;;; org-mode.el --- org mode configuration/setup
;;; Commentary:
;;; no comments
;;; Code:

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(use-package org
  :ensure t
  :init
  (add-hook 'org-mode-hook 'auto-revert-mode)
  (setq org-capture-templates
              (quote (("t" "todo" entry (file "~/org/inbox.org")
                       "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                      ("n" "note" entry (file "~/org/inbox.org")
                       "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                      ("w" "org-protocol" entry (file "~/org/inbox.org")
                       "* TODO Review [[%:link][%:description]]\n  Captured on%U\n" :immediate-finish t)
                      ("j" "next jira task" entry (file "~/org/inbox.org")
                       "* NEXT [[%:link][%:description]]\n" :clock-in t :clock-resume t)
                      ("m" "Meeting" entry (file "~/org/inbox.org")
                       "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                      ("p" "Phone call" entry (file "~/org/inbox.org")
                       "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                      ("h" "Habit" entry (file "~/org/inbox.org")
                       "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

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
  (setq org-agenda-span 'day)
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

  (setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar")
  (setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2022.0/libexec/plantuml.jar")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (plantuml . t)
     (sqlite . t))) 
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
 
;; Heavily modified based on https://github.com/novoid/title-capitalization.el/blob/master/title-capitalization.el
(defun title-capitalization (str)
  "Convert str to title case"
  (interactive)
  (with-temp-buffer
    (insert str)
    (let* ((beg (point-min))
           (end (point-max))
	   ;; Basic list of words which don't get capitalized according to simplified rules
	   ;; http://karl-voit.at/2015/05/25/elisp-title-capitalization/
           (do-not-capitalize-basic-words '("a" "ago" "an" "and" "as" "at" "but" "by" "for"
                                            "from" "in" "into" "it" "next" "nor" "of" "off"
                                            "on" "onto" "or" "over" "past" "so" "the" "till"
                                            "to" "up" "yet"
                                            "n" "t" "es" "s"))
	   ;; If user has defined 'my-do-not-capitalize-words, append to basic list
           (do-not-capitalize-words (if (boundp 'my-do-not-capitalize-words)
                                        (append do-not-capitalize-basic-words my-do-not-capitalize-words )
                                      do-not-capitalize-basic-words)))
      ;; Go to begin of first word
      (goto-char beg)
      (setq continue t)

      ;; Go through the region, word by word
      (while continue
        (let ((last-point (point)))
          (let ((word (thing-at-point 'word)))
            (if (stringp word)
                ;; Capitalize current word except when it is list member
                (if (and (member (downcase word) do-not-capitalize-words)
                         ;; Always capitalize first word
                         (not (= (point) 1)))
                    (downcase-word 1)

                  ;; If it's an acronym, don't capitalize
                  (if (string= word (upcase word))
                      (progn
                        (goto-char (+ (point) (length word) 1)))
                    (capitalize-word 1)))))

          (skip-syntax-forward "^w" end)

          ;; Break if we are at the end of the buffer
          (when (= (point) last-point)
            (setq continue nil))))

      ;; Always capitalize the last word
      (backward-word 1)

      (let ((word (thing-at-point 'word)))
        (if (and (>= (point) 0)
                 (not (member (or word "s")
                              '("n" "t" "es" "s")))
                 (not (string= word (upcase word))))
            (capitalize-word 1))))

    (buffer-string)))

;; Org export
(use-package ox-reveal :ensure t)
(use-package ox-hugo :ensure f :after ox
  :init
  ;; These functions need to be in :init otherwise they will not be
  ;; callable in an emacs --batch context which for some reason
  ;; can't be found in autoloads if it's under :config
  (defun my/org-roam--extract-note-body (file)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (first (org-element-map (org-element-parse-buffer) 'paragraph
               (lambda (paragraph)
                 (let ((begin (plist-get (first (cdr paragraph)) :begin))
                       (end (plist-get (first (cdr paragraph)) :end)))
                   (buffer-substring begin end)))))))

  ;; Include backlinks in org exported notes not tagged as private or
  ;; draft
  (defun my/org-roam--backlinks-list (file)
    (if (org-roam--org-roam-file-p file)
        (--reduce-from
         (concat acc (format "- [[file:%s][%s]]\n#+begin_quote\n%s\n#+end_quote\n"
                             (file-relative-name (car it) org-roam-directory)
                             (title-capitalization (org-roam-db--get-title (car it)))
                             (my/org-roam--extract-note-body (car it))))
         ""
         (org-roam-db-query
          [:select :distinct [links:source]
                   :from links
                   :left :outer :join tags :on (= links:source tags:file)
                   :where (and (= dest $s1)
                               (or (is tags:tags nil)
                                   (and
                                    (not-like tags:tags '%private%)
                                    (not-like tags:tags '%draft%))))]
          file))
      ""))

  (defun file-path-to-md-file-name (path)
    (let ((file-name (first (last (split-string path "/")))))
      (concat (first (split-string file-name "\\.")) ".md")))

  (defun file-path-to-slug (path)
    (let* ((file-name (car (last (split-string path "/"))))
           (title (first (split-string file-name "\\."))))
      (replace-regexp-in-string (regexp-quote "_") "-" title nil 'literal)))

  (defun org-roam-to-hugo-md-single-file (f)
    ;; Use temporary buffer to prevent a buffer being opened for
    ;; each note file.
    (with-temp-buffer
      (message "Working on: %s" f)
      (insert-file-contents f)

      (goto-char (point-min))
      ;; Add in hugo tags for export. This lets you write the
      ;; notes without littering HUGO_* tags everywhere
      ;; HACK:
      ;; org-export-output-file-name doesn't play nicely with
      ;; temp buffers since it attempts to get the file name from
      ;; the buffer. Instead we explicitely add the name of the
      ;; exported .md file otherwise you would get prompted for
      ;; the output file name on every note.
      (insert
       (format "#+HUGO_BASE_DIR: %s\n#+HUGO_SECTION: notes\n#+HUGO_SLUG: %s\n#+EXPORT_FILE_NAME: %s\n"
               org-roam-publish-path
               (file-path-to-slug f)
               (file-path-to-md-file-name f)))

      ;; If this is a placeholder note (no content in the
      ;; body) then add default text. This makes it look ok when
      ;; showing note previews in the index and avoids a headline
      ;; followed by a headline in the note detail page.
      (if (eq (my/org-roam--extract-note-body f) nil)
          (progn
            (goto-char (point-max))
            (insert "\n/This note does not have a description yet./\n")))

      ;; Add in backlinks because
      ;; org-export-before-processing-hook won't be useful the
      ;; way we are using a temp buffer
      (let ((links (my/org-roam--backlinks-list f)))
        (unless (string= links "")
          (goto-char (point-max))
          (insert (concat "\n* Links to this note\n") links)))

      (org-hugo-export-to-md)))
  
  ;; Fetches all org-roam files and exports to hugo markdown
  ;; files. Adds in necessary hugo properties
  ;; e.g. HUGO_BASE_DIR. Ignores notes tagged as private or draft
  (defun org-roam-to-hugo-md ()
    (interactive)
    ;; Make sure the author is set
    (setq user-full-name "Jo Geraerts")

    (let ((files (mapcan
                  (lambda (x) x)
                  (org-roam-db-query
                   [:select [files:file]
                            :from files
                            :left :outer :join tags :on (= files:file tags:file)
                            :where (or (is tags:tags nil)
                                       (and
                                        (not-like tags:tags '%private%)
                                        (not-like tags:tags '%draft%)))]))))
      (mapc 'org-roam-to-hugo-md-single-file files)))

  (defun org-roam-to-hugo-md-after-save ()
    (unless (eq real-this-command 'org-capture-finalize)
      (save-excursion
        (org-roam-to-hugo-md-single-file (buffer-file-name))))))


(define-minor-mode org-roam-auto-export-mode
  "Toggle auto exporting the Org file using `ox-hugo'."
  :global nil
  :lighter ""
  (if org-roam-auto-export-mode
      ;; When the mode is enabled
      (progn
        (add-hook 'after-save-hook #'org-roam-to-hugo-md-after-save :append :local))
    ;; When the mode is disabled
    (remove-hook 'after-save-hook #'org-roam-to-hugo-md-after-save :local)))


(provide 'setup-org)
;;; setup-org.el ends here
