
;; c和c++的缩进风格
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(provide 'init-indent)
