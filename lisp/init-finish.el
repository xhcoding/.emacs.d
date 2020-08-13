;;; init-finish.el --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:


;; server
(setq server-name "emacs-server-file")

(unless (server-running-p server-name)
  (server-start))

(toggle-frame-fullscreen)

;; 记录安装的所有包
(with-temp-file (expand-file-name "package.list" user-emacs-directory)
  (insert (mapconcat 'symbol-name package-activated-list "\n"))
  )

;; 将系统输入法切换成英文输入法
(when (and IS-WINDOWS (executable-find "im-select"))
  (shell-command-to-string "im-select 1033")
  )

(provide 'init-finish)

;;; init-finish.el ends here
