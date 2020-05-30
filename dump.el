;;; dump.el --- dump.el  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(require 'package)
(package-initialize)


(dolist (package-path (directory-files (expand-file-name "extensions" user-emacs-directory) t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
  (add-to-list 'load-path package-path))


(setq talon-dumped-load-path load-path)

(let ((package-list
       (with-temp-buffer
         (insert-file-contents (expand-file-name "package.list" user-emacs-directory))
         (split-string (buffer-string)))))

  (dolist (package '(
                     "snails"
                     "auto-save"
                     "color-rg"
                     "thing-edit"
                     "awesome-pair"
                     "company-english-helper"
                     "recentf"
                     "smerge-mode"
                     "cc-mode"
                     "whitespace"
                     "org"
                     "python"
                     ))
    (add-to-list 'package-list package))
  (dolist (package package-list)
    (require (intern package)))

  )

(load-theme 'doom-one t t)

(dump-emacs-portable (expand-file-name "emacs.pdump" user-emacs-directory))

;;; dump.el ends here
