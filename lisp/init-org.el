;;; init-org.el --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package org
  :ensure nil
  :preface
  (defun hot-expand (str &optional mod)
    "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Org Template" 'fileicon "org")
           :color blue :quit-key "q")
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("C" (hot-expand "<C") "comment")
     ("e" (hot-expand "<e") "example")
     ("E" (hot-expand "<E") "export")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("n" (hot-expand "<n") "note")
     ("o" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("y" (hot-expand "<s" "python :results output") "python")
     ("p" (hot-expand "<s" "perl") "perl")
     ("r" (hot-expand "<s" "ruby") "ruby")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         :map org-mode-map
         ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1)))))

  :config
  (setq org-ellipsis " ▼ "
        org-directory talon-org-dir
        org-agenda-files (list (concat talon-org-dir "gtd.org"))
        org-M-RET-may-split-line `((default . nil)))

  (setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{[:nonascii:]")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[[:nonascii:]")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-element-update-syntax)
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (require 'org-protocol)

  (setq org-capture-templates
        `(
          ("ts" "Study Task" entry
           (file+headline ,(expand-file-name "gtd.org" talon-org-dir) "Tasks")
           "* TODO %^{Brief Description}\tAdded: %U\t:Study:\n%?")
          ("tp" "Project Task" entry
           (file+headline ,(expand-file-name "gtd.org" talon-org-dir) "Tasks")
           "* TODO %^{Brief Description}\tAdded: %U\t:Project:\n%?")
          ("wc" "Work Code Note" entry
           (file+headline ,(expand-file-name "work-note.org" talon-org-dir) "Code")
           "* %^{Brief Description}\tAdded: %U\t:Work:\n%?")
          ("sn" "Study Note" entry
           (file+headline ,(expand-file-name "study-note.org" talon-org-dir) "Note")
           "* %^{Brief Description}\tAdded: %U\t:Study: :Note:\n%?")
          ("ps" "Protocol Text" plain
           (file+function ,(expand-file-name "web.org" talon-org-dir) org-capture-template-goto-link)
           "Added: %U\n\t%:initial" :empty-lines 1 :immediate-finish t :kill-buffer t)
          ("pb" "Protocol Bookmarks" entry
           (file+headline ,(expand-file-name "web.org" talon-org-dir) "Bookmarks")
           "* %:annotation\tAdded: %U" :empty-lines 1 :immediate-finish t :kill-buffer t)
          ))

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
