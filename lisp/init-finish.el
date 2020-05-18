;;; init-finish.el --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:


;; server
(setq server-name "emacs-server-file")
(server-start)

(toggle-frame-fullscreen)

(with-temp-file (expand-file-name "package.list" user-emacs-directory)
  (insert (mapconcat 'symbol-name package-activated-list "\n"))
  )

(provide 'init-finish)

;;; init-finish.el ends here
