;;; init-const.el --- my emacs configuration -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

;; 定义一些有用的常量
;;
;;我没有 MAC
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; 第三方插件的目录，如 git, 或者单个文件
(defconst talon-ext-dir (expand-file-name "extensions" user-emacs-directory))

;; 一些动态库的目录
(defconst talon-lib-dir (expand-file-name "lib" user-emacs-directory))

(add-to-list 'load-path talon-lib-dir)

;; 一些配置目录
(defconst talon-etc-dir (expand-file-name "etc" user-emacs-directory))

;; org
(defconst talon-org-dir (expand-file-name "~/Documents/Org/"))

(defconst talon-private-snippets-dir (expand-file-name "private/snippets/" talon-org-dir))

;; code dir
(if IS-WINDOWS
    (defconst talon-code-dir (expand-file-name "D:/Code"))
  (defconst talon-code-dir (expand-file-name "~/Code")))


(provide 'init-const)

;;; init-const.el ends here
