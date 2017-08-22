
(require-package 'plantuml-mode)
(require-package 'flycheck-plantuml)

(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(setq plantuml-jar-path "/home/xhcoding/Tools/plantuml/plantuml.jar")

;; org-mode
(add-to-list
 'org-src-lang-modes '("plantuml" . plantuml))

;; flyfheck
(with-eval-after-load 'flycheck
  (require 'flycheck-plantuml)
  (flycheck-plantuml-setup))

(provide 'init-plantuml)
