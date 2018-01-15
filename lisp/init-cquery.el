(use-package lsp-mode)

(use-package cquery
  :after 'lsp-mode
  :load-path
  "/home/xhcoding/Tools/cquery/emacs"
  :config
  ;; put your config here
  (setq cquery-executable "/home/xhcoding/Tools/cquery/bin/cquery")
  (setq cquery-sem-highlight-method 'font-lock)
  (setq cquery-enable-sem-hightlight t)
  (setq cquery-extra-init-params '(:enableComments 2 :cacheFormat "msgpack"))
  (add-hook 'c-mode-common-hook #'my-cquery//enable)
  )
(provide 'init-cquery)
