
;;; init-dap.el --- emacs -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:


(use-package dap-mode
  :config
  (require 'dap-node)
  (dap-mode +1)
  (dap-ui-mode +1)
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  )

(provide 'init-dap)
;;; init-dap.el ends here
