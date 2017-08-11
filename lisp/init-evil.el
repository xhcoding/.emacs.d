
;; 需要的包
(require-package 'evil)
(require-package 'evil-escape)
(require-package 'evil-leader)
(require-package 'evil-surround)
(require-package 'evil-visualstar)
(require-package 'evil-tutor)

(evil-mode t)

;;evil-escape
(evil-escape-mode t)
(setq-default evil-escape-key-sequence "fd")
(setq-default evil-escape-delay 0.2)

;;evil-leader
(global-evil-leader-mode t)
(evil-leader/set-leader "<SPC>")


(provide 'init-evil)
