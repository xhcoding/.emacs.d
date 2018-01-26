

(use-package python-mode
  :defer nil
  :config
  (progn
    (setq python-indent-offset 4)
    (add-to-list 'auto-mode-alist '("\.py\'" . python-mode))
    (require 'lsp-mode)
    (require 'lsp-python)
    (add-hook 'python-mode-hook #'lsp-python-enable)
    (add-hook 'python-mode-hook
	      (lambda ()
		(add-to-list (make-local-variable 'company-backends)
			     '(company-lsp))))))


;; (use-package elpy
;;   :config
;;   (progn
;;     (add-hook 'python-mode-hook 'elpy-enable)
;;     ))

;; (use-package company-jedi
;;   :config
;;   (progn
;;     (add-hook 'python-mode-hook
;; 	      (lambda ()
;; 		(add-to-list (make-local-variable 'company-backends)
;; 			     '(company-jedi))))))

;; (use-package py-autopep8)
(provide 'init-python)
