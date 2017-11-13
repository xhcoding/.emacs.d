;; (use-package meghanada
;;   :init
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

(provide 'init-java)
