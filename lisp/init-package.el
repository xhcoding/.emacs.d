


;; package archives
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
			 ("org"   . "http://elpa.emacs-china.org/org/")))


(package-initialize)

;; use package
(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))

(use-package use-package-chords)
(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)


(use-package elpa-mirror
  :init
  (setq elpamr-default-output-directory "~/planB/myelpa"))


(provide 'init-package)
