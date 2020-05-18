;;; init-web --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:


(use-package js
  :ensure nil
  :hook ((js-mode . lsp)
         (js-mode . (lambda()
                      (set (make-local-variable 'company-backends)
                           '((company-capf company-ctags company-dabbrev-code))))))
  :custom (js-indent-level 2)
  :config
  (with-eval-after-load 'projectile
    (push "package.json" projectile-project-root-files)
    (push "node_modules" projectile-globally-ignored-directories)))



(provide 'init-web)

;;; init-web.el ends here
