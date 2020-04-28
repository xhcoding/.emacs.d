;;; init-yasnippet.el --- emacs -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  (add-to-list 'yas-snippet-dirs talon-private-snippets-dir))


(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
