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

  (defun talon--js-comment-new-line()
    (newline-and-indent)
    (insert "* "))

  (defun talon-js-new-line()
    "Newline and indent."
    (interactive)
    (cond ((talon*inner-comment-p) (talon--js-comment-new-line))
          (t (newline-and-indent))))

  :bind (:map js2-mode-map
              ("RET" . talon-js-new-line)))

(use-package js-comint
  :config
  (add-hook 'js2-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
            (local-set-key (kbd "C-c b") 'js-send-buffer)
            (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go))))

(provide 'init-web)

;;; init-web.el ends here
