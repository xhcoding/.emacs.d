;;; init-flycheck.el --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-global-modes
        '(not text-mode outline-mode fundamental-mode org-mode
              diff-mode shell-mode eshell-mode term-mode vterm-mode)
        flycheck-emacs-lisp-load-path 'inherit
        ;; Only check while saving and opening files
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-indication-mode 'right-fringe)
   )

(provide 'init-flycheck)

;;; init-flycheck.el ends here
