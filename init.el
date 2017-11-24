(setq debug-on-error t)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(setq package-enable-at-startup nil)

;; load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))


;; package manager
(require 'init-package)

;; some useful functions
(require 'init-func)

;; let emacs perform better
(require 'init-better)

;; gui settings
(require 'init-gui)

;; org
(require 'init-org)

;; cpp
(require 'init-cpp)

;; java
(require 'init-java)

;; python
(require 'init-python)

;; asm
(require 'init-asm)

;; vv
(require 'init-vc)

;; latex
(require 'init-tex)

;; keybindings
(require 'init-keys)

;; custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

