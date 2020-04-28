;;; init-vcs.el --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:


(use-package magit
  :config
    (when IS-WINDOWS
      (setenv "GIT_ASKPASS" "git-gui--askpass")))


(use-package magit-todos)

(provide 'init-vcs)

;;; init-vcs.el ends here
