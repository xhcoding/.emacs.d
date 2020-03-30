;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Commentary:

;; early-init 在 UI 和 package 初始化之前调用

;;; Code:

;; 设置垃圾回收的阈值，避免在启动的过程中进行垃圾回收
(setq gc-cons-threshold most-positive-fixnum)

;; 不要自动激活 package ，我们自己处理
(setq package-enable-at-startup nil)

;; 不要 resize frame
(setq frame-inhibit-implied-resize t)

;; 忽略掉 X resources, 没啥用
(advice-add #'x-apply-session-resources :override #'ignore)

;; 关闭各种 bar
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))



;;; early-init.el ends here
