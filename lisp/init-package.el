
(require 'package)
;;包源
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
			 ("org"   . "http://elpa.emacs-china.org/org/")))

;; 安装指定的包
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be re-download
in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
	(if (boundp 'package-selected-packages)
	    (package-install package nil)
	  (package-install package))
      (progn
	(package-refresh-contents)
	(require-package package min-version t)))))

;; 尝试安装指定包，成功返回non-nil,失败返回nil并打印警告信息
(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case error
      (require-package package min-version no-refresh)
    (error
     (message "Coundn't install optional package `%s': %S" package) nil)))


;;; 设置成nil，启动时不激活包，调用package-initialize时激活
(setq package-enable-at-startup nil)
;; 包初始化
(package-initialize)

(provide 'init-package)
