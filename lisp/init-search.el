;;; init-search.el --- emacs configuration  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package fuz
  :load-path (lambda() (expand-file-name "fuz" talon-ext-dir))
  :config
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod)))

(use-package color-rg
  :load-path (lambda() (expand-file-name "color-rg" talon-ext-dir)))

(use-package snails
  :load-path (lambda() (expand-file-name "snails" talon-ext-dir))
  :commands (snails snails-search-point)
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
                 ) snails-backend-buffer-blacklist)))

(use-package counsel
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :bind (("C-s"   . swiper)
         ("C-x C-r" . counsel-recentf)
         ("C-c r" . counsel-rg))
  :init
  (when IS-WINDOWS
    (setq ivy-dynamic-exhibit-delay-ms 200))
  :config
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never %s %s")
    ))

(use-package counsel-etags
  :bind (("C-]" . counsel-etags-find-tag-at-point)))

(provide 'init-search)

;;; init-search.el ends here
