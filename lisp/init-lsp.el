;;; init-lsp.el --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package lsp-mode
  :diminish
  :init
  (setq read-process-output-max (* 1024 1024))

  (setq lsp-keymap-prefix "C-c l"
        lsp-auto-guess-root t
        lsp-flycheck-live-reporting nil
        lsp-keep-workspace-alive nil
        lsp-prefer-capf t
        lsp-signature-auto-activate t

        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        lsp-enable-symbol-highlighting nil))

(use-package lsp-python-ms
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp)))
  :config
  (when (executable-find "python3")
    (setq lsp-python-ms-python-executable-cmd "python3")))


(use-package ccls
  :hook ((c-mode c++-mode) . (lambda()
                               (require 'ccls)
                               (lsp)))
  :config
  (setq ccls-initialization-options
        `(:cache (:directory ,(expand-file-name "~/Code/ccls_cache"))
                 :compilationDatabaseDirectory "build")))



(provide 'init-lsp)

;;; init-lsp.el ends here
