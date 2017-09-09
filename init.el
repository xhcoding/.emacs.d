;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; 自定义配置的路径
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
;; custom.el文件的路径
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


;; 工具性质的函数
(require 'init-utils)
;; 包管理
(require 'init-package)
;; 主题配置
(require 'init-theme)
;; gui配置
(require 'init-gui)
;; 最近文件
(require 'init-recentf)
;; 括号，引号的配对及括号高亮配置
(require 'init-smartpair)
;; 文件目录配置
(require 'init-dired)
;; 补全配置
(require 'init-completion)
;; 语法检查
(require 'init-flycheck)
;; 对搜索，命令输入的优化
(require 'init-ivy)
;; 优化一些默认的配置
(require 'init-better-default)
;; 代码跳转 gtags ctags  
(require 'init-tags)
;; org配置
(require 'init-org)
;; evil
(require 'init-evil)
;; 空格键作为leader键的键绑定
(require 'init-spc-keybindings)
;; c/c++ 开发设置
(require 'init-cc)
;载入custom.el
(when (file-exists-p custom-file)
  (load custom-file))
