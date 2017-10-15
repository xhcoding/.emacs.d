
(use-package python-mode
  :defer nil
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\.py\'" . python-mode))))

(use-package elpy
  :init
  (progn
    (add-hook 'python-mode-hook 'elpy-enable)
    ))

(use-package company-jedi
  :init
  (progn
    (add-hook 'python-mode-hook
	      (lambda ()
		(add-to-list (make-local-variable 'company-backends)
			     '(company-jedi))))))

(use-package py-autopep8)
(provide 'init-python)
