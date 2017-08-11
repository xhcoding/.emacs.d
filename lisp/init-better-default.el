;; 需要的包
(require-package 'hungry-delete)
(require-package 'popwin)
(require-package 'window-numbering)


;; 关掉自动备份
(setq make-backup-files nil)

;;自动加载外部文件的修改
(global-auto-revert-mode t)

;;设置yes-no为y-n
(fset 'yes-or-no-p 'y-or-n-p)

;;高亮当前行
(global-hl-line-mode t)   

;; hungry-delete 一下删除所有空格
(global-hungry-delete-mode t)

;; popwin 
(require 'popwin)
(popwin-mode t)

;; 默认搜索当前被选中的或者在光标下的字符串
(defun occur-dwim()
  "Call 'occur' with a sane default"
  (interactive)
  (push (if (region-active-p)
	    (buffer-substring-no-properties
	     (region-beginning)
	     (region-end))
	  (let ((sym (thing-at-point 'symbol)))
	    (when (stringp sym)
	      (regexp-quote sym))))
	regexp-history)
  (call-interactively 'occur))
(global-set-key (kbd "M-s o") 'occur-dwim)

;;配置缩进，缩进所有或者选定的区域
(defun indent-buffer()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if(region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indented selected region."))
      (progn
	(indent-buffer)
	(message "Indented buffer.")))))
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(window-numbering-mode t)


(provide 'init-better-default)
