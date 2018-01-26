(use-package lsp-mode)

(require 'cquery)
(setq cquery-executable
      "/home/xhcoding/Tools/cquery/build/release/bin/cquery")
(setq cquery-extra-init-params '(:enableComments 2 :cacheFormat "msgpack"))
(setq cquery-sem-highlight-method 'font-lock)

(push 'company-lsp company-backends)


(provide 'init-cquery)
