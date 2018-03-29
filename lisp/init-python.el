
(setq use-lsp t)

(if use-lsp

    (progn
      (use-package python-mode
	:config
	(progn
	  (setq-default python-indent-offset 4)
	  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
	  (use-package lsp-mode)
	  (use-package lsp-python)
	  (require 'lsp-mode)
	  (require 'lsp-python)
	  (add-hook 'python-mode-hook #'lsp-python-enable)
	  (use-package lsp-ui
	    :config
	    (progn
	      (require 'lsp-ui)
	      (add-hook 'lsp-mode-hook 'lsp-ui-mode)
	      (define-key python-mode-map (kbd "M-.") 'lsp-ui-peek-find-definitions)
	      (define-key python-mode-map (kbd "M-?") 'lsp-ui-peek-find-references)


	      ))
	  (use-package company-lsp
	    :config
	    (add-hook 'python-mode-hook
		  (lambda ()
		    (add-to-list (make-local-variable 'company-backends)
				 '(company-lsp)))))
	  )))
      
  
  (progn
    (use-package python-mode
      :defer nil
      :config
      (progn
	(setq-default python-indent-offset 4)
	(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
	))



    (use-package elpy
      :config
      (progn
	(add-hook 'python-mode-hook 'elpy-enable)
	))

    (use-package company-jedi
      :config
      (progn
	(add-hook 'python-mode-hook
		  (lambda ()
		    (add-to-list (make-local-variable 'company-backends)
				 '(company-jedi))))))

    (use-package py-autopep8)

    ))


(provide 'init-python)
