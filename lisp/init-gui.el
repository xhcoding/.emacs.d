;; 关闭工具栏
(tool-bar-mode -1)

;; 关闭菜单栏
(menu-bar-mode -1)

;; 开启行号
;(global-linum-mode t)
;(setq linum-delay t)
;; 开启列号
(column-number-mode t)

;; 关闭滚动条
(scroll-bar-mode -1)

;; 关闭启动动画
(setq inhibit-splash-screen t)
;; 显示启动信息
(setq inhibit-startup-echo-area-message t)

;;修改光标
(setq-default cursor-type 'bar)

;;打开时全屏
(setq initial-frame-alist (quote ((fullscreen . maximized))))

(provide 'init-gui)
