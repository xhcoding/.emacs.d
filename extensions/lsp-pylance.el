;;; lsp-pylance.el --- lsp pylance  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(setq lsp-pylance-server-cmd  `("node"
                                ,(expand-file-name "~/.vscode/extensions/ms-python.vscode-pylance-2020.7.4/server/server.bundle.js")
                                "--stdio"))


(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda () lsp-pylance-server-cmd)
                   (lambda ()
                     (and (cl-second lsp-pylance-server-cmd)
                          (file-exists-p (cl-second lsp-pylance-server-cmd)))))
  :major-modes '(python-mode)
  :server-id 'mspylance
  :priority 3
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration (lsp-configuration-section "python"))))
  :notification-handlers (lsp-ht ("pyright/beginProgress" 'ignore)
                                 ("pyright/reportProgress" 'ignore)
                                 ("pyright/endProgress" 'ignore))))

(provide 'lsp-pylance)

;;; lsp-pylance.el ends here
