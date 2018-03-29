;; (use-package meghanada
;;   :config
;;   (progn
;;     (add-hook 'java-mode-hook
;; 	      (lambda ()
;; 		;; meghanada-mode on
;; 		(meghanada-mode t)
;; 		(setq c-basic-offset 4)
;; 		;; use code format
;; 		;;(add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
;; 		)))
;;   :bind
;;   (:map meghanada-mode-map
;; 	("C-c m f" . meghanada-code-beautify)))


(use-package groovy-mode)

(use-package lsp-java
  :config
  (require 'lsp-java)
  (require 'lsp-mode)
  (add-hook 'java-mode-hook #'lsp-java-enable)
  (define-key java-mode-map (kbd "C-M-/") 'company-capf)
  )

;;(use-package lsp-intellij
;;   :config
;;   (require 'lsp-intellij)
;;   (add-hook 'java-mode-hook #'lsp-intellij-enable))

(provide 'init-java)
