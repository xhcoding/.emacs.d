;;; init-tramp.el --- tramp  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package tramp
  :config
  (when IS-WINDOWS
    (setq tramp-default-method "plink"))
  )

(provide 'init-tramp)

;;; init-tramp.el ends here
