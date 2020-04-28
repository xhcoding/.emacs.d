;;; init.el --- my emacs configuration -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

;; 只支持 27 以上的版本，用旧版本的意义何在？
(when (version< emacs-version "27.0.50")
  (error "This require Emacs 17.0.50 and above"))

;; 取个名字真难。“刀下生，刀下死”
(defconst talon-version "1.0.0")

(setq user-full-name "xhcoding"
      user-mail-address "xhcoding@163.com")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-accelerate)
(require 'init-const)
(require 'init-functions)
(require 'init-custom)
(require 'init-package)
(require 'init-hydra)
(require 'init-better-default)
(require 'init-ui)
(require 'init-window)
(require 'init-edit)
(require 'init-search)
(require 'init-completion)
(require 'init-chinese)
(require 'init-blog)
(require 'init-vcs)

;; program
(require 'init-flycheck)
(require 'init-project)
(require 'init-yasnippet)
(require 'init-lsp)
(require 'init-cc)
(require 'init-org)
(require 'init-python)
(require 'init-markdown)
(require 'init-others)
(require 'init-finish)


;;; init.el ends here
