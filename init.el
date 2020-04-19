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
  "计算启动时间."
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

;; 一些配置目录
(defconst talon-etc-dir (expand-file-name "etc" user-emacs-directory))

;; org
(defconst talon-org-dir (expand-file-name "~/Documents/Org/"))

(add-to-list 'load-path talon-lib-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             默认值的设置                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; visible
(setq visible-bell 0)

;; *Message* 里多点 log
(setq message-log-max 8192)

;; 设置编码，真让人头疼
(prefer-coding-system 'utf-8)

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

;; tab 宽度
(setq-default tab-width 4)

;; 高亮当前行
(global-hl-line-mode +1)

;; minibuffer 默认值提示消失
(setq-default minibuffer-eldef-shorten-default t)
(minibuffer-electric-default-mode +1)

;; custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  use-package                                  ;;
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
;;                                备份设置                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default indent-tabs-mode nil)
(use-package whitespace
  :demand
  :ensure nil
  :config
  (defun doom-highlight-non-default-indentation-h ()
    "Highlight whitespace that doesn't match your `indent-tabs-mode' setting.
e.g. If you indent with spaces by default, tabs will be highlighted. If you
indent with tabs, spaces at BOL are highlighted.
Does nothing if `whitespace-mode' is already active or the current buffer is
read-only or not file-visiting."
    (unless (or (eq major-mode 'fundamental-mode)
                buffer-read-only
                (null buffer-file-name))
      (require 'whitespace)
      (set (make-local-variable 'whitespace-style)
           (let ((style (if indent-tabs-mode '(indentation) '(tabs tab-mark))))
             (if whitespace-mode
                 (cl-union style whitespace-active-style)
               style)))
      (cl-pushnew 'face whitespace-style)
      (whitespace-mode +1)))
  (add-hook 'after-change-major-mode-hook #'doom-highlight-non-default-indentation-h 'append)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Evil                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :demand
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode +1)
  ;;(setcdr evil-insert-state-map nil)
  ;; (general-define-key
  ;;  :states 'insert
  ;;  "<ESC>" 'evil-normal-state
  ;;  "M-x" 'execute-extended-command)

  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-escape
  :defer 1
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.15)
  :config
  (evil-escape-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Keybindings                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst talon-leader-key "SPC")

(defconst talon-localleader-key "SPC m")

(use-package general
  :demand
  :config

  (general-define-key
   :states 'insert
   "C-f" 'forward-char
   "C-b" 'backward-char
   "C-n" 'next-line
   "C-p" 'previous-line
   )
  (general-create-definer
    talon-leader-def
    :prefix talon-leader-key)

  ;; 默认的键绑定
  (talon-leader-def
    :keymaps 'normal
    ;; window
    "wq" 'delete-window
    "wv" 'split-window-horizontally
    "ws" 'split-window-vertically
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wh" 'evil-window-left
    "wl" 'evil-window-right
    ;; help
    "hf" 'describe-function
    "hk" 'describe-key
    "hv" 'describe-variable

    ;; buffer
    "bk" 'kill-current-buffer
    )

  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             最近打开文件                                      ;;
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
  (talon-leader-def
    :keymaps 'normal
    "fr" #'(lambda()(interactive)(snails '(snails-backend-recentf)))
    )
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
  (setq awesome-tab-height 100)
  (awesome-tab-mode +1)
  (talon-leader-def
    :keymaps 'normal
    "t" 'awesome-tab-ace-jump)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   主题设置                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package awesome-tray
  :demand
  :load-path (lambda() (expand-file-name "awesome-tray" talon-ext-dir))
  :config

  (defun talon-module-flycheck-info ()
    (when (boundp 'flycheck-last-status-change)
      (pcase flycheck-last-status-change
        (`finished
         (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                (errors (cdr (assq 'error error-counts)))
                (warnings (cdr (assq 'warning error-counts))))
           (concat "["
                   (cond
                    (errors (format "❄:%s" errors))
                    (warnings (format "☁:%s" warnings))
                    (t "☀"))
                   "]"))))))

  (defface talon-module-flycheck-face
    '((((background light))
       :foreground "#cc2444" :bold t)
      (t
       :foreground "#ff2d55" :bold t))
    "Flycheck state face."
    :group 'awesome-tray)

  (add-to-list 'awesome-tray-module-alist
               '("flycheck" . (talon-module-flycheck-info talon-module-flycheck-face)))

  (defun talon-module-rime-info ()
    (rime-lighter))

  (defface talon-module-rime-face
    '((((background light))
       :foreground "#9256B4" :bold t)
      (t
       :foreground "#9256B4" :bold t))
    "Rime module face."
    :group 'awesome-tray)

  (add-to-list 'awesome-tray-module-alist
               '("rime" . (talon-module-rime-info talon-module-rime-face)))

  (defun talon-module-encoding-info ()
    (with-current-buffer (buffer-name)
      (let ((eof (coding-system-eol-type buffer-file-coding-system))
            (sys (coding-system-plist buffer-file-coding-system))
            (info ""))
        (setq info (concat info
                           (pcase eof
                             (0 "LF ")
                             (1 "CRCF ")
                             (2 "CR "))
                           (cond ((memq (plist-get sys :category)
                                        '(coding-category-undecided coding-category-utf-8))
                                  "UTF-8")
                                 (t (upcase (symbol-name (plist-get sys :name)))))))
        info)))

  (defface talon-module-encoding-face
    '((((background light))
       :foreground "#1b2bdd" :bold t)
      (t
       :foreground "#1b2bdd" :bold t))
    "Encoding module face."
    :group 'awesome-tray)

  (add-to-list 'awesome-tray-module-alist
               '("encoding" . (talon-module-encoding-info talon-module-encoding-face)))



  (setq awesome-tray-active-modules '("flycheck" "rime" "evil"  "location" "mode-name" "git" "encoding" "date"))

  (awesome-tray-mode +1))

(use-package lazycat-theme
  :demand
  :load-path (lambda() (expand-file-name "lazycat-theme" talon-ext-dir))
  :commands (lazycat-theme-toggle)
  :config
  (load-theme 'lazycat-light t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   好看的图标                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package all-the-icons
  :defer 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   好看的字体                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if IS-WINDOWS
    (set-frame-font      "-outline-等距更纱黑体 SC-normal-normal-normal-mono-30-*-*-*-c-*-iso8859-1")
  (set-frame-font  "-outline-Sarasa Term SC-normal-normal-normal-mono-30-*-*-*-c-*-iso8859-1"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   好看的括号                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   配对的括号                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package show-paren
  :ensure nil
  :hook (after-init . show-paren-mode))

(use-package awesome-pair
  :demand
  :load-path (lambda() (expand-file-name "awesome-pair" talon-ext-dir))
  :hook ((
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
          ("C-d" . awesome-pair-forward-delete)
          ("C-k" . awesome-pair-kill)
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
  (talon-leader-def
    :keymaps 'normal
    "sb" #'(lambda()(interactive)(snails '(snails-backend-current-buffer)))
    "bb" #'(lambda()(interactive)(snails '(snails-backend-buffer)))
    "f." #'(lambda()(interactive(snails '(snails-backend-directory-files) (file-name-directory (buffer-file-name)))))
    "." #'(lambda()(interactive(snails '(snails-backend-directory-files))))
    )
  )

(use-package color-rg
  :defer 1
  :load-path (lambda() (expand-file-name "color-rg" talon-ext-dir))
  :config
  (evil-set-initial-state 'color-rg-mode 'emacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                       补全                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :diminish
  :hook (after-init . global-company-mode)
  :commands company-cancel
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
        company-backends '(company-capf)))

(use-package company-posframe
  :hook (company-mode . company-posframe-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Projectile!!!                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-use-git-grep t)
  :config
  (cond
   ((executable-find "fd")
    (setq projectile-generic-command
          (format "%s . --color=never --type f -0 -H -E .git"
                  "fd")
          projectile-git-command projectile-generic-command
          projectile-git-submodule-command nil
          ;; ensure Windows users get fd's benefits
          projectile-indexing-method 'alien))

   ((executable-find "rg")
    (setq projectile-generic-command
          (concat "rg -0 --files --color=never --hidden"
                  (cl-loop for dir in projectile-globally-ignored-directories
                           concat (format " --glob '!%s'" dir)))
          projectile-git-command projectile-generic-command
          projectile-git-submodule-command nil
          ;; ensure Windows users get rg's benefits
          projectile-indexing-method 'alien)))

  (when IS-WINDOWS
    (setq projectile-git-submodule-command nil
      projectile-enable-caching nil))

  (talon-leader-def
    :keymaps 'normal
    "sp" #'(lambda()(interactive)(snails '(snails-backend-projectile))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   rime 输入法                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rime
  :defer 1
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

  (dolist (hook (list
                 'c++-mode-hook))
    (add-hook hook '(lambda()(setq-local rime-disable-predicates
                                         '(rime-predicate-evil-mode-p
                                           rime-predicate-space-after-cc-p)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   xref                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package xref
  :ensure nil
  :bind (:map evil-normal-state-map
          ("gd" . xref-find-definitions)
          ("gr" . xref-find-references)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   shackle                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enforce rules for popups
(defvar shackle--popup-window-list nil) ; all popup windows
(defvar-local shackle--current-popup-window nil) ; current popup window
(put 'shackle--current-popup-window 'permanent-local t)

(use-package shackle
  :functions org-switch-to-buffer-other-window
  :commands shackle-display-buffer
  :hook (after-init . shackle-mode)
  :config

  (eval-and-compile
    ;; Add keyword: `autoclose'
    (defun shackle-display-buffer-hack (fn buffer alist plist)
      (let ((window (funcall fn buffer alist plist)))
        (setq shackle--current-popup-window window)

        (when (plist-get plist :autoclose)
          (push (cons window buffer) shackle--popup-window-list))
        window))

    (defun shackle-close-popup-window-hack (&rest _)
      "Close current popup window via `C-g'."
      (setq shackle--popup-window-list
            (cl-loop for (window . buffer) in shackle--popup-window-list
                     if (and (window-live-p window)
                             (equal (window-buffer window) buffer))
                     collect (cons window buffer)))
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p)))
        (let (window buffer)
          (if (one-window-p)
              (progn
                (setq window (selected-window))
                (when (equal (buffer-local-value 'shackle--current-popup-window
                                                 (window-buffer window))
                             window)
                  (winner-undo)))
            (setq window (caar shackle--popup-window-list))
            (setq buffer (cdar shackle--popup-window-list))
            (when (and (window-live-p window)
                       (equal (window-buffer window) buffer))
              (delete-window window)

              (pop shackle--popup-window-list))))))

    (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
    (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack))

  ;; HACK: compatibility issuw with `org-switch-to-buffer-other-window'
  (advice-add #'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window)

  (setq shackle-default-size 0.4
        shackle-default-alignment 'below
        shackle-default-rule nil
        shackle-rules
        '((("*Help*" "*Apropos*") :select t :size 0.3 :align 'below :autoclose t)
          ("*Flycheck errors*" :select t :size 0.3 :align 'below :autoclose t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   snippets                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package yasnippet
  :defer 1
  :config
  (yas-global-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   magit                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :defer 1
  :config
  (setq magit-auto-revert-mode nil)
  (talon-leader-def
    :keymaps 'normal
    "gg" #'magit-status)

  (general-define-key
   :states 'normal
   "zu" 'smerge-keep-upper
   "zl" 'smerge-keep-lower)
  )

(use-package magit-todos
  :defer 1
  :config
  )

(use-package evil-magit
  :defer 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   flycheck                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :defer 1
  :config
  (general-define-key
   :states 'normal
   :keymaps 'flycheck-error-list-mode-map
   "<RET>" 'flycheck-error-list-goto-error
   "q"     'quit-window)
  (global-flycheck-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   LSP!!!                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :commands (lsp)
  :bind (:map lsp-mode-map
          ([remap xref-find-definitions] . lsp-find-definition)
          ([remap xref-find-references] . lsp-find-references)
          )
  :config
  (setq lsp-auto-guess-root t
        lsp-prefer-capf t
        lsp-diagnostic-package :flycheck
        lsp-enable-file-watchers nil))

(use-package company-lsp
  :after (company lsp-mode)
  :init
  (setq company-lsp-cache-candidates nil)
  (add-hook 'lsp-mode-hook
            (lambda()
              (add-to-list (make-local-variable 'company-backends)
                           'company-lsp)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   python                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python
  :ensure nil
  :init
  (setq python-indent-guess-indent-offset-verbose nil
        python-indent-guess-indent-offset nil
        python-indent-offset 4))

(use-package lsp-python-ms
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   C++!!!                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package cc-mode
  :ensure nil
  :mode ("\\.h\\'" . c++-mode)
  :config
  (defun talon-set-c-style()
    "Set current buffer's c-style to my style."
    (interactive)
    (c-add-style "talon-style"
                 '("stroustrup"
                   (indent-tabs-mode . nil)
                   (c-basic-offset . 4)
                   (c-offsets-alist
                    (innamespace . -))
                   ) t))

  (defun talon--after-class-p()
    (save-excursion
      (beginning-of-line)
      (looking-at  "\\(class\\|struct\\)")))

  (defun talon--inner-bracket-p()
    (save-excursion
      (backward-char)
      (looking-at "{}")))

  (defun talon--inner-bracket-ret()
    (when (talon--after-class-p)
      (save-excursion
        (end-of-line)
        (insert ";")))
    (open-line 1)
    (newline-and-indent)
    (save-excursion
      (next-line)
      (c-indent-line)))

  (defun talon-c-new-line()
    "Newline and indent."
    (interactive)
    (cond ((talon--inner-bracket-p) (talon--inner-bracket-ret))
          (t (newline-and-indent))))

  (add-hook 'c++-mode-hook 'talon-set-c-style)
  (general-define-key
   :states 'insert
   :keymaps 'c++-mode-map
   "<RET>" 'talon-c-new-line)
  )


(use-package ccls
  :hook ((c-mode c++-mode) . lsp)
  :config
  (setq ccls-initialization-options
        `(:cache (:directory ,(expand-file-name "~/Code/ccls_cache"))
                 :compilationDatabaseDirectory "build")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   restclient                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

(use-package groovy-mode
  :defer 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   blog                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defconst +my-blog-root-dir
  "~/Blog/"
  "Blog root directory.")

(defconst +my-blog-img-dir
  (expand-file-name "images/" +my-blog-root-dir)
  "Blog's image directory.")

(use-package ox-hugo)

(defun +my-blog*easy-hugo--org-headers (file)
  "Return a draft org mode header string for a new article as FILE."
  (let ((date
         (format-time-string "%Y-%m-%d")))
    (concat
     "#+HUGO_BASE_DIR: ../"
     "\n#+HUGO_SECTION: post"
     "\n#+TITLE: " file
     "\n#+DATE: " date
     "\n#+AUTHOR:"
     "\n#+HUGO_CUSTOM_FRONT_MATTER: :author \"xhcoding\""
     "\n#+HUGO_TAGS: "
     "\n#+HUGO_CATEGORIES: "
     "\n#+HUGO_DRAFT: false"
     "\n\n")))

(use-package easy-hugo
  :defer 1
  :commands (easy-hugo)
  :config
  (setq easy-hugo-basedir (expand-file-name +my-blog-root-dir)
        easy-hugo-postdir "blog"
        easy-hugo-org-header t)
  (evil-set-initial-state 'easy-hugo-mode 'emacs)
  (advice-add #'easy-hugo--org-headers :override #'+my-blog*easy-hugo--org-headers)

  (talon-leader-def
    :keymaps 'normal
    "ob" 'easy-hugo)
  )

(defun +my-blog/publish()
  "Publish my blog."
  (interactive)
  (let ((default-directory +my-blog-root-dir))
    (call-process-shell-command "hugo")
    (setq default-directory (expand-file-name "public" +my-blog-root-dir))
    (call-process-shell-command "git add .")
    (call-process-shell-command "git commit -m \"publish\"")
    (call-process-shell-command "git push")
    (message "publish finished")))

(defun +my-blog/export-all()
  "Export all org to md."
  (interactive)
  (let ((default-directory (expand-file-name "blog" +my-blog-root-dir))
        (files (directory-files (expand-file-name
                                 "blog" +my-blog-root-dir))))
    (seq-each
     (lambda(file)
       (when (and (not (string-equal "." file)) (not (string-equal ".." file)) (not (string-equal "config.toml" file)))
         (with-temp-buffer
           (find-file file)
           (org-hugo-export-to-md))))
     files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   tramp                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tramp
  :ensure nil
  :defer 1
  :config
  (setq tramp-default-method "ssh"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   visual-regexp                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package visual-regexp
  :defer 1
  :commands (vr/query-replace vr/replace)
  :config
  (talon-leader-def
    :keymaps 'normal
    "rr" 'vr/query-replace
    "rR" 'vr/replace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   org                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :ensure nil
  :config
  (setq org-ellipsis " ▼ "
        org-directory talon-org-dir
        org-agenda-files (list (concat talon-org-dir "gtd.org"))
        )

    (setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{[:nonascii:]")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[[:nonascii:]")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-element-update-syntax)
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  )

;; functions
(defun talon-rename-this-file-and-buffer (new-name)
  "Rename both current buffer and file it's visiting to NEW_NAME"
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))


(use-package format-all
  :demand
  )

(use-package websocket)

(use-package eaf
  :demand
  :load-path "D:/Code/Elisp/emacs-application-framework"
  :init
  (setq eaf-python-command "python")
  :config
  (evil-set-initial-state 'eaf-mode 'emacs)
  (eaf-setq eaf-browser-default-zoom  "2")
  (setq eaf-grip-token "ab9d3961d8564bf818d705ba19f83979d9c2b9a7")
  (defun talon-eaf-open-current-file()
    (interactive)
    (eaf-open (buffer-file-name))))


(toggle-frame-fullscreen)
;;; init.el ends here
