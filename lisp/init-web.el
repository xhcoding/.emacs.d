;;; init-web --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package js2-mode
  :mode "\\.m?js\\'"
  :hook (js2-mode . lsp)
  :interpreter "node"
  :custom (js-indent-level 2)
  :config
  (with-eval-after-load 'projectile
    (push "package.json" projectile-project-root-files)
    (push "node_modules" projectile-globally-ignored-directories))
  )

(provide 'init-web)

;;; init-web.el ends here
