;;; init-search.el --- emacs configuration  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package fuz
  :load-path (lambda() (expand-file-name "fuz" talon-ext-dir))
  :config
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod)))


(use-package snails
  :load-path (lambda() (expand-file-name "snails" talon-ext-dir))
  :commands (snails snails-search-point)
  :bind (("C-s" . snails)
         ("C-x C-f" . (lambda()(interactive)(snails '(snails-backend-directory-files))))
         ("C-x C-r" . (lambda()(interactive)(snails '(snails-backend-recentf))))
         ("C-x b" . (lambda()(interactive)(snails '(snails-backend-buffer)))))
  :config
  (setq snails-default-backends
        '(
          snails-backend-buffer
          snails-backend-recentf)
        snails-prefix-backends
        '((">" '(snails-backend-command))
          ("@" '(snails-backend-imenu))
          ("#" '(snails-backend-current-buffer))
          ("!" '(snails-backend-rg))
          ("?" '(snails-backend-projectile))))

  (setq snails-backend-buffer-blacklist
        (append (list
                 snails-tips-buffer
                  " *company-box"
                  ) snails-backend-buffer-blacklist))

  )

(use-package color-rg
  :load-path (lambda() (expand-file-name "color-rg" talon-ext-dir)))

(use-package counsel
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :init
  (when IS-WINDOWS
    (setq ivy-dynamic-exhibit-delay-ms 200)))

(provide 'init-search)

;;; init-search.el ends here
