;;; init-org.el --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package org
  :ensure nil
  :config
  (setq org-ellipsis " ▼ "
        org-directory talon-org-dir
        org-agenda-files (list (concat talon-org-dir "gtd.org"))
        )

  (setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{[:nonascii:]")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[[:nonascii:]")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-element-update-syntax)
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list))


(use-package plantuml-mode
  :init
  (setq org-plantuml-jar-path (expand-file-name "plantuml.jar" talon-etc-dir))
  (setq plantuml-jar-path (expand-file-name "plantuml.jar" talon-etc-dir)))

(provide 'init-org)

;;; init-org.el ends here
