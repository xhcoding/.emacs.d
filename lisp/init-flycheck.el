;; 需要的包
(require-package 'flycheck)

(add-hook 'after-init-hook 'global-flycheck-mode)


(provide 'init-flycheck)
