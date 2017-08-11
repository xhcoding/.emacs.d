;; 需要的包
(require-package 'ivy)
(require-package 'swiper)
(require-package 'counsel)

(ivy-mode t)

(after-load 'ivy
  (setq-default ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(provide 'init-ivy)
