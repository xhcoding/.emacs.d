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
     ("r" (hot-expand "<s" "rust") "rust")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png"
                      (concat "plantuml :file ../images/"
                              (file-name-base buffer-file-name)
                              (format-time-string "_%Y%H%M%S")))
      "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1)))))

  :config

  (require 'org-habit)

  (setq org-ellipsis " ▼ "
        org-directory talon-org-dir
        org-M-RET-may-split-line `((default . nil)))

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
                               load-language-list)


  ;; gtd 相关配置

  (setq org-agenda-files `(,(expand-file-name "gtd" talon-org-dir)))

  (setq org-default-notes-file (expand-file-name "gtd/inbox.org" talon-org-dir))

  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)"  "|" "DONE(d)" "CANCELLED(c@/!)")
                            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold))))

  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

  (require 'org-protocol)

  (setq org-capture-templates
        `(("t"
           "todo"
           entry
           (file  ,(expand-file-name "gtd/inbox.org" talon-org-dir))
           "* TODO %?\n%U\n")
          ("h"
           "Habit"
           entry
           (file ,(expand-file-name "gtd/inbox.org" talon-org-dir))
           "* TODO %?\n%U\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+2d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: TODO\n:END:\n")
          ))

  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))

  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (defun talon-verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  (setq org-refile-target-verify-function 'talon-verify-refile-target)


  (setq org-agenda-dim-blocked-tasks nil)

  (setq org-agenda-compact-blocks t)

  (defvar  talon-hide-scheduled-and-waiting-next-tasks t)

  (defun talon*is-project-p ()
    "Any task with a todo keyword subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task has-subtask))))


  (defun talon*find-project-task ()
    "Move point to the parent (project) task if any"
    (save-restriction
      (widen)
      (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (goto-char parent-task)
        parent-task)))

  (defun talon*is-project-subtree-p ()
    "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
    (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                (point))))
      (save-excursion
        (talon*find-project-task)
        (if (equal (point) task)
            nil
          t))))

  (defun talon*is-task-p ()
    "Any task with a todo keyword and no subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task (not has-subtask)))))

  (defun talon*is-subproject-p ()
    "Any task which is a subtask of another project"
    (let ((is-subproject)
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (while (and (not is-subproject) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq is-subproject t))))
      (and is-a-task is-subproject)))

  (defun talon*skip-non-stuck-projects ()
    "Skip trees that are not stuck projects"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (talon*is-project-p)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (has-next ))
              (save-excursion
                (forward-line 1)
                (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                  (unless (member "WAITING" (org-get-tags-at))
                    (setq has-next t))))
              (if has-next
                  next-headline
                nil)) ; a stuck project, has subtasks but no next task
          next-headline))))

  (defun talon*skip-non-projects ()
    "Skip trees that are not projects"
    (if (save-excursion (talon*skip-non-stuck-projects))
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((talon*is-project-p)
              nil)
             ((and (talon*is-project-subtree-p) (not (talon*is-task-p)))
              nil)
             (t
              subtree-end))))
      (save-excursion (org-end-of-subtree t))))

  (defun talon*skip-projects-and-habits-and-single-tasks ()
    "Skip trees that are projects, tasks that are habits, single non-project tasks"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((org-is-habit-p)
          next-headline)
         ((and talon-hide-scheduled-and-waiting-next-tasks
               (member "WAITING" (org-get-tags-at)))
          next-headline)
         ((talon*is-project-p)
          next-headline)
         ((and (talon*is-task-p) (not (talon*is-project-subtree-p)))
          next-headline)
         (t
          nil)))))

  (defun talon*skip-non-project-tasks ()
    "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((talon*is-project-p)
          next-headline)
         ((org-is-habit-p)
          subtree-end)
         ((and (talon*is-project-subtree-p)
               (member (org-get-todo-state) (list "NEXT")))
          subtree-end)
         ((not (talon*is-project-subtree-p))
          subtree-end)
         (t
          nil)))))

  (defun talon*skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

  (setq org-agenda-span 'day)
  (setq org-agenda-custom-commands
        '(
          ("o" "Agenda"
           ((agenda "" nil)
            (tags "INBOX"
                  ((org-agenda-overriding-header "Tasks To Refile")
                   (org-tags-match-list-sublevels nil)))
            (tags-todo "-CANCELLED/!"
                       ((org-agenda-overriding-header "Stuck Projects")
                        (org-agenda-skip-function 'talon*skip-non-stuck-projects)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-HOLD-CANCELLED/!"
                       ((org-agenda-overriding-header "Projects")
                        (org-agenda-skip-function 'talon*skip-non-projects)
                        (org-tags-match-list-sublevels 'indented)
                        (org-agenda-sorting-strategy
                         '(category-keep))))

            (tags-todo "-CANCELLED/!NEXT"
                       ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                              (if talon-hide-scheduled-and-waiting-next-tasks
                                                                  ""
                                                                " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'talon*skip-projects-and-habits-and-single-tasks)
                        (org-tags-match-list-sublevels t)
                        (org-agenda-todo-ignore-scheduled talon-hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines talon-hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date talon-hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))


            (tags-todo "-INBOX-CANCELLED-WAITING/!"
                       ((org-agenda-overriding-header (concat "Project Subtasks"
                                    (if talon-hide-scheduled-and-waiting-next-tasks ""
                                      " (including WAITING and SCHEDULED tasks)")))
                  (org-agenda-skip-function 'talon*skip-non-project-tasks)
                  (org-agenda-todo-ignore-scheduled talon-hide-scheduled-and-waiting-next-tasks)
                  (org-agenda-todo-ignore-deadlines talon-hide-scheduled-and-waiting-next-tasks)
                  (org-agenda-todo-ignore-with-date talon-hide-scheduled-and-waiting-next-tasks)
                  (org-agenda-sorting-strategy
                   '(category-keep))))
            (tags "-REFILE/"
                  ((org-agenda-overriding-header "Tasks to Archive")
                   (org-agenda-skip-function 'talon*skip-non-archivable-tasks)
                   (org-tags-match-list-sublevels nil)))

            ))))

  ;; archive
  (setq org-archive-mark-done nil)
  (setq org-archive-location "%s_archive::* Archived Tasks")

  (setq org-tag-alist '((:startgroup)
                        ("@office" . ?o)
                        ("@home" . ?H)
                        (:endgroup)
                        ("WAITING" . ?w)
                        ("HOLD" . ?h)
                        ("CANCELLED" . ?c)
                        ("PERSONAL" . ?P)
                        ("WORK" . ?W)))

  (setq org-fast-tag-selection-single-key 'expert)
  (setq org-agenda-tags-todo-honor-ignore-options t)

  )

(use-package plantuml-mode
  :init
  (setq org-plantuml-jar-path (expand-file-name "plantuml.jar" talon-etc-dir))
  (setq plantuml-jar-path (expand-file-name "plantuml.jar" talon-etc-dir))
  (setq plantuml-default-exec-mode 'jar)
  :config
  (add-to-list 'org-babel-default-header-args:plantuml
               `(:cmdline . ,(concat "-charset utf-8")))
  )

(use-package ox-pandoc
  :init
  (add-to-list 'org-export-backends 'pandoc)
  (setq org-pandoc-options
        '((standalone . t)
          (mathjax . t))))

(use-package org-roam
  :bind (("C-c m f" . org-roam-find-file)
         ("C-c m c" . org-roam-capture)
         ("C-c m i" . org-roam-insert))

  :config
  (setq org-roam-capture-templates
      '(
        ("d" "default" plain (function org-roam-capture--get-point)
         "%?"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+title: ${title}\n#+roam_alias:\n#+roam_tags: \n"))))

(use-package org-roam-server
  :config
  (require 'org-roam-protocol)
  (setq org-roam-directory (expand-file-name "org-roam" talon-org-dir)
        org-roam-server-host "127.0.0.1"
        org-roam-server-port 9000
        org-roam-server-authenticate nil
        org-roam-server-label-truncate t
        org-roam-server-label-truncate-length 60
        org-roam-server-label-wrap-length 20)

  (defun talon/open-roam-browser()
    (interactive)
    (shell-command (format "explorer.exe http://127.0.0.1:%s" org-roam-server-port)))
  )

(provide 'init-org)

;;; init-org.el ends here
