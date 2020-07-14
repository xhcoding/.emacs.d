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

(use-package counsel
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :bind (("C-s"   . swiper))
  :init
  (when IS-WINDOWS
    (setq ivy-dynamic-exhibit-delay-ms 200)))

(use-package counsel-etags
  :bind (("C-]" . counsel-etags-find-tag-at-point)))

(provide 'init-search)

;;; init-search.el ends here
