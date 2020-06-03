;;; init-web --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:


(with-eval-after-load 'projectile
  (push "package.json" projectile-project-root-files)
  (push "node_modules" projectile-globally-ignored-directories))

(use-package js2-mode
  :mode "\\.m?js\\'"
  :hook ((js2-mode . lsp)
         (js2-mode . (lambda()
                      (set (make-local-variable 'company-backends)
                           '((company-capf company-ctags company-dabbrev-code))))))
  :interpreter "node"
  :custom (js-indent-level 2)
  :config
  (setq js-indent-level 2
        js-chain-indent t
        js2-highlight-level 3
        js2-highlight-external-variables t)

  (defun talon-js-new-line()
    "Newline and indent."
    (interactive)
    (cond ((talon*inner-comment-p) (c-indent-new-comment-line))
          (t (newline-and-indent))))


  :bind (:map js2-mode-map
              ("RET" . talon-js-new-line)))

(provide 'init-web)

;;; init-web.el ends here
