;;; init.el --- my emacs configuration -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

;; 只支持 27 以上的版本，用旧版本的意义何在？
(when (version< emacs-version "27.0.50")
  (error "This require Emacs 17.0.50 and above"))

;; 取个名字真难。“刀下生，刀下死”
(defconst talon-version "1.0.0")

;; 增加启动速度的措施，毕竟大家都加了。
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda()
            "恢复默认值"
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 16777216)
            (setq gc-cons-percentage 0.1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   计算启动时间                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar talon-init-time 'nil)
(defun talon-display-benchmark()
  "计算启动时间。"
  (message "Talon loaded %s packages in %.03fs"
           (length package-activated-list)
           (or talon-init-time
               (setq talon-init-time
                     (float-time (time-subtract (current-time) before-init-time))))))
(add-hook 'emacs-startup-hook #'talon-display-benchmark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              定义一些有用的常量                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 定义一些有用的常量
;;
;;我没有 MAC
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; 第三方插件的目录，如 git, 或者单个文件
(defconst talon-ext-dir (expand-file-name "extensions" user-emacs-directory))

;; 一些动态库的目录
(defconst talon-lib-dir (expand-file-name "lib" user-emacs-directory))

(add-to-list 'load-path talon-lib-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             默认值的设置                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; *Message* 里多点 log
(setq message-log-max 8192)

;; 设置编码，真让人头疼
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
;; Windows 的剪贴板不支持 utf-8
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8))

(when IS-WINDOWS
  ;;此句保证中文字体设置有效
  (setq locale-coding-system 'gb18030)
  ;; 确保file-name-coding-system变量的设置不会无效
  (setq w32-unicode-filenames 'nil)
  ;; 设置文件名的编码为gb18030
  (setq file-name-coding-system 'gb18030))

;; 关闭重定义警告
(setq ad-redefinition-action 'accept)

;; auto-mode-alist 区分大小写
(setq auto-mode-case-fold nil)

;; 关闭启动动画
(setq inhibit-startup-message t)

;; *scratch* 为 fundaemental-mode
(setq initial-major-mode 'fundamental-mode)

;; *scratch* 里面为空
(setq initial-scratch-message nil)

;; 启动时不要显示 For information about GNU Emacs...
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; 增加 emacs 更新 UI 的时间
(setq idle-update-delay 1)

;; yes 太长了
(fset 'yes-or-no-p 'y-or-n-p)


;; custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  use-package                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

;; 插件镜像
(setq package-archives
      '(("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
        ("org-cn"   . "http://elpa.emacs-china.org/org/")
        ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")))

;; 初始化 package
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; 安装 use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; use-package 的设置
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; use-package 需要这两个包
(use-package diminish)
(use-package bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                备份设置                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 关闭默认的 backup
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 使用 auto-save
(use-package auto-save
  :defer 1
  :load-path (lambda() (expand-file-name "auto-save" talon-ext-dir))
  :config
  (auto-save-enable)
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Evil                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :demand
  :config
  (evil-mode +1))

(use-package evil-leader
  :demand
  :after evil
  :init
  :config
  (global-evil-leader-mode +1)
  (evil-leader/set-leader "<SPC>"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             最近打开文件                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (evil-leader/set-key
   "fr" 'recentf-open-files)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   which key                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :defer 1
  :config
  (which-key-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   awesome-tab                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package awesome-tab
  :demand
  :load-path (lambda() (expand-file-name "awesome-tab" talon-ext-dir))
  :config
  (awesome-tab-mode +1)
  (setq awesome-tab-height 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   主题设置                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package awesome-tray
  :demand
  :load-path (lambda() (expand-file-name "awesome-tray" talon-ext-dir))
  :config
  (awesome-tray-mode +1))

(use-package lazycat-theme
  :demand
  :load-path (lambda() (expand-file-name "lazycat-theme" talon-ext-dir))
  :commands (lazycat-theme-toggle)
  :config
  (load-theme 'lazycat-dark t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   好看的图标                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package all-the-icons
  :defer 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   好看的字体                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if IS-WINDOWS
    (set-frame-font  "-outline-等距更纱黑体 SC-normal-normal-normal-mono-30-*-*-*-c-*-iso8859-1")
  (set-frame-font  "-outline-Sarasa Term SC-normal-normal-normal-mono-30-*-*-*-c-*-iso8859-1"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   好看的括号                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   配对的括号                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package awesome-pair
  :demand
  :load-path (lambda() (expand-file-name "awesome-pair" talon-ext-dir))
  :hook ((
          c-mode-common
          c-mode
          c++-mode
          java-mode
          haskell-mode
          emacs-lisp-mode
          lisp-interaction-mode
          lisp-mode
          maxima-mode
          ielm-mode
          sh-mode
          makefile-gmake-mode
          php-mode
          python-mode
          js-mode
          go-mode
          qml-mode
          jade-mode
          css-mode
          ruby-mode
          coffee-mode
          rust-mode
          qmake-mode
          lua-mode
          swift-mode
          minibuffer-inactive-mode
          ) . awesome-pair-mode)
  :bind (:map evil-insert-state-map
          ("(" . awesome-pair-open-round)
          ("[" . awesome-pair-open-bracket)
          ("{" . awesome-pair-open-curly)
          ("}" . awesome-pair-close-round)
          ("]" . awesome-pair-close-bracket)
          ("}" . awesome-pair-close-curly)
          ("=" . awesome-pair-equal)
          ("%" . awesome-pair-match-paren)
          ("\"" . awesome-pair-double-quote)
          ("SPC" . awesome-pair-space)
          ("M-o" . awesome-pair-backward-delete)
          ("C-d" . awesome-pair-forward-delete)
          ("C-k" . awesome-pair-kill)
          ("M-\"" . awesome-pair-wrap-double-quote)
          ("M-[" . awesome-pair-wrap-bracket)
          ("M-{" . awesome-pair-wrap-curly)
          ("M-(" . awesome-pair-wrap-round)
          ("M-)" . awesome-pair-unwrap)
          ("M-p" . awesome-pair-jump-right)
          ("M-n" . awesome-pair-jump-left)
          ("M-:" . awesome-pair-jump-out-pair-and-newline)
          ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                      全局搜索                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package fuz
  :load-path (lambda() (expand-file-name "fuz" talon-ext-dir))
  :config
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod)))

(use-package snails
  :demand
  :load-path (lambda() (expand-file-name "snails" talon-ext-dir))
  :commands (snails snails-search-point)
  :bind (("C-s" . snails)
         ([remap execute-extended-command] . (lambda()(interactive)(snails '(snails-backend-command)))))
  :config
  (evil-set-initial-state 'snails-mode 'emacs)
  (evil-leader/set-key
   "bb" #'(lambda()(interactive)(snails '(snails-backend-buffer))))
  )

(use-package color-rg
  :load-path (lambda() (expand-file-name "color-rg" talon-ext-dir))
  :commands (color-rg-search-input)
  :config
  (evil-set-initial-state 'color-rg-mode 'emacs)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                       补全                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :diminish
  :hook (after-init . global-company-mode)
  :commands company-cancel
  :bind (("M-/" . company-complete)
         ("C-M-i" . company-complete)
         :map company-mode-map
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . my-company-yasnippet)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :init
  (setq company-idle-delay 0.2
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes
        '(not comint-mode erc-mode message-mode help-mode gud-mode)
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-backends '((:separate company-capf company-yasnippet))))

(use-package company-posframe
  :hook (company-mode . company-posframe-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Projectile!!!                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :hook (after-init . projectile-mode)
  :config
  (setq projectile-enable-caching (not noninteractive)
        projectile-indexing-method 'alien
        projectile-require-project-root t)
  (evil-leader/set-key
   "pf" #'(lambda()(interactive)(snails '(snails-backend-projectile)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   LSP!!!                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package nox
  :defer 1
  :load-path (lambda() (expand-file-name "nox" talon-ext-dir))
  :config
  (dolist (hook (list
                 'js-mode-hook
                 'rust-mode-hook
                 'python-mode-hook
                 'ruby-mode-hook
                 'java-mode-hook
                 'sh-mode-hook
                 'php-mode-hook
                 'c-mode-common-hook
                 'c-mode-hook
                 'c++-mode-hook
                 'haskell-mode-hook
                 ))
    (add-hook hook '(lambda () (nox-ensure)))))

;;; init.el ends here
