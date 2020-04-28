;;; init-better-default.el --- my emacs configuration -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

;; visible
(setq visible-bell 0)

;; *Message* 里多点 log
(setq message-log-max 8192)

;; 设置编码，真让人头疼
(prefer-coding-system 'utf-8)

;; 关闭重定义警告
(setq ad-redefinition-action 'accept)

;; auto-mode-alist 区分大小写
(setq auto-mode-case-fold nil)

;; 关闭启动动画
(setq inhibit-startup-message t)

;; *scratch* 为 fundaemental-mode
(setq initial-major-mode 'fundamental-mode)

;; *scratch* 里面为空
(setq initial-scratch-message nil)

;; 启动时不要显示 For information about GNU Emacs...
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; 增加 emacs 更新 UI 的时间
(setq idle-update-delay 1)

;; yes 太长了
(fset 'yes-or-no-p 'y-or-n-p)

;; tab 宽度
(setq-default tab-width 4)

;; 高亮当前行
(global-hl-line-mode +1)

;; minibuffer 默认值提示消失
(setq-default minibuffer-eldef-shorten-default t)
(minibuffer-electric-default-mode +1)

(provide 'init-better-default)

;;; init-better-default.el ends here
