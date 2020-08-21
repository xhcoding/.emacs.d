;;; init-eaf.el --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package eaf
  :if (file-exists-p (expand-file-name "ELisp/emacs-application-framework" talon-code-dir))
  :load-path (lambda()(expand-file-name "ELisp/emacs-application-framework" talon-code-dir))
  :init
  (when IS-WINDOWS
    (setq eaf-python-command "python"))
  :config
  (eaf-setq eaf-browser-default-zoom  "2")
  (setq eaf-grip-token talon-github-grip-token)
  (defun talon-eaf-open-current-file()
    (interactive)
    (eaf-open (buffer-file-name))))

(provide 'init-eaf)

;;; init-eaf.el ends here
