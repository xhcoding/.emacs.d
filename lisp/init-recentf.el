;; init后打开recentf
(add-hook 'after-init-hook (lambda () (recentf-mode 1)))
;; 显示的最大数量为20
(setq-default recentf-max-menu-items 20)
;; 保存的最大数量为100
(setq-default recentf-max-saved-items 100)
;;
(global-set-key (kbd "C-c C-r") 'recentf-open-files)

(provide 'init-recentf)
