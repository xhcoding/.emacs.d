;; 需要的包
(require-package 'dired+)
(require-package 'dired-sort)

(after-load 'dired
  (require 'dired+)
  (require 'dired-sort)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))





(provide 'init-dired)
