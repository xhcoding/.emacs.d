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
          projectile-enable-caching nil)))

(provide 'init-project)

;;; init-project.el ends here
