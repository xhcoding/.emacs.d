;;; init-project.el --- emacs  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:



(use-package projectile
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-use-git-grep t)
  :config
  (cond
   ((executable-find "fd")
    (setq projectile-generic-command
          (format "%s . --color=never --type f -0 -H -E .git"
                  "fd")
          projectile-git-command projectile-generic-command
          projectile-git-submodule-command nil
          ;; ensure Windows users get fd's benefits
          projectile-indexing-method 'alien))

   ((executable-find "rg")
    (setq projectile-generic-command
          (concat "rg -0 --files --color=never --hidden"
                  (cl-loop for dir in projectile-globally-ignored-directories
                           concat (format " --glob '!%s'" dir)))
          projectile-git-command projectile-generic-command
          projectile-git-submodule-command nil
          ;; ensure Windows users get rg's benefits
          projectile-indexing-method 'alien)))

  (when IS-WINDOWS
    (setq projectile-git-submodule-command nil
          projectile-enable-caching nil))

  (add-to-list 'projectile-project-root-files-top-down-recurring "configure")
  )

(use-package counsel-projectile
  :hook (counsel-mode . counsel-projectile-mode)
  :bind (("C-x p p" . counsel-projectile-switch-project))
  :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

(use-package treemacs
  :bind (([f8] . treemacs)
         ("M-0" . treemacs-select-window))

  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-is-never-other-window   t
        treemacs-silent-filewatch        t
        treemacs-silent-refresh          t
        treemacs-width                   30
        treemacs-no-png-images           nil))

(use-package treemacs-projectile
  :after projectile
  :bind (:map projectile-command-map
              ("h" . treemacs-projectile)))

(provide 'init-project)

;;; init-project.el ends here
