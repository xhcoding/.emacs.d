;;; init-javascript.el --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:


(use-package js
  :ensure nil
  :hook (js-mode . lsp)
  :custom (js-indent-level 2)
  :config
  (with-eval-after-load 'projectile
    (push "package.json" projectile-project-root-files)
    (push "node_modules" projectile-globally-ignored-directories)))

(provide 'init-javascript)

;;; init-javascript.el ends here
