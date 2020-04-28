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
         ("C-x C-f" . (lambda()(interactive)(snails '(snails-backend-directory-files)))))
  :config (setq snails-default-backends
                '(
                  snails-backend-awesome-tab-group
                  snails-backend-buffer
                  snails-backend-recentf)))

(use-package color-rg
  :load-path (lambda() (expand-file-name "color-rg" talon-ext-dir)))

(provide 'init-search)

;;; init-search.el ends here
