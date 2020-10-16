;;; init-dap.el --- emacs -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:


(use-package dap-mode
  :config
  (dap-mode +1)
  (dap-ui-mode +1)
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  (require 'dap-lldb)
  (when IS-WINDOWS
    (setq dap-lldb-debug-program '("D:\\Project\\llvm-project\\Release\\bin\\lldb-vscode")))

  (defun talon/get-debugged-program()
    (interactive)
    (read-file-name "Debug: "))

  (setq dap-lldb-debugged-program-function #'talon/get-debugged-program)

  )

(provide 'init-dap)
;;; init-dap.el ends here
