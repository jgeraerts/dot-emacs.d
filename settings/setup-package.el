;;; setup-package --- Setup package manager -*- lexical-binding: t; -*-
;;; Commentary:
;;;

(require 'package)
(require 'dash)

;;; Code:
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 20)))

(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(provide 'setup-package)
;;; setup-package.el ends here
