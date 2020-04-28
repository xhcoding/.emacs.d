;;; init-custom.el --- my emacs configuration -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (and (file-exists-p custom-file)
           (file-readable-p custom-file))
  (load custom-file))


(provide 'init-custom)

;;; init-custom.el ends here
