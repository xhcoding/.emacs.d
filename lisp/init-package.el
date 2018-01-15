
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
(use-package diminish)
(use-package bind-key)

(setq use-package-verbose t)

(setq use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))



(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)


(use-package elpa-mirror
  :config
  (setq elpamr-default-output-directory "~/Backup/myelpa"))


(provide 'init-package)
