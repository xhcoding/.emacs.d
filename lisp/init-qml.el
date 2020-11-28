;;; init-qml.el --- qml development -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package qml-mode
  :config
  (defun init-qml-mode()
    (setq-local js-indent-level 4)
    (setq-local company-backends '(company-tabnine)))
  (add-hook 'qml-mode-hook #'init-qml-mode))


(provide 'init-qml)


;;; init-qml.el ends here
