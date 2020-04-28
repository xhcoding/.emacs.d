;;; init-chinese.el --- emacs config  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package rime
  :load-path (lambda()(expand-file-name "emacs-rime" talon-ext-dir))
  :init
  (setq rime--module-path (expand-file-name (concat "librime-emacs" module-file-suffix) talon-lib-dir))
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-share-data-dir (expand-file-name "rime-data" talon-etc-dir))
  (rime-user-data-dir (expand-file-name "rime-user" talon-etc-dir))
  :config
  (setq rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdccc"
              :font "微软雅黑"
              :internal-border-width 10))

  (setq rime-disable-predicates
        '(rime-predicate-after-alphabet-char-p
          rime-predicate-hydra-p
          rime-predicate-punctuation-after-ascii-p)))


(provide 'init-chinese)

;;; init-chinese.el ends here
