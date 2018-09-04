;;; init.el -*- lexical-binding: t; -*-
(require 'package)

(defconst mage-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory)))
(defconst mage-local-dir (concat mage-emacs-dir ".local/"))
(defconst mage-package-dir (concat mage-local-dir "packages/"))
(defconst mage-cache-dir (concat mage-local-dir "cache/"))
(defconst mage-etc-dir (concat mage-local-dir "etc/"))
(defconst mage-ext-dir (concat mage-emacs-dir "site-lisp/"))

(setq user-emacs-directory (file-name-directory load-file-name))
(setq package-user-dir (concat mage-package-dir "elpa/"))
(unless (file-directory-p package-user-dir)
  (make-directory package-user-dir))

(setq-default custom-file                  (concat mage-emacs-dir "custom.el")
	  abbrev-file-name             (concat mage-local-dir "abbrev.el")
	  auto-save-list-file-name     (concat mage-cache-dir "autosave")
	  backup-directory-alist       (list (cons "." (concat mage-cache-dir "backup/")))
	  mc/list-file                 (concat mage-etc-dir "mc-lists.el")
	  pcache-directory             (concat mage-cache-dir "pcache/")
	  request-storage-directory    (concat mage-cache-dir "request")
	  server-auth-dir              (concat mage-cache-dir "server/")
	  shared-game-score-directory  (concat mage-etc-dir "shared-game-score/")
	  tramp-auto-save-directory    (concat mage-cache-dir "tramp-auto-save/")
	  tramp-backup-directory-alist backup-directory-alist
	  tramp-persistency-file-name  (concat mage-cache-dir "tramp-persistency.el")
	  url-cache-directory          (concat mage-cache-dir "url/")
	  url-configuration-directory  (concat mage-etc-dir "url/"))

(defvar  mage--file-name-handler-alist file-name-handler-alist)
  (defun mage|pre-init()
  (setq gc-cons-threshold 402653184
	gc-cons-percentage 1.0
	file-name-handler-alist nil
  ))
(defun mage|post-init ()
	 (setq gc-cons-threshold 16777216
	   gc-cons-percentage 0.15
	   file-name-handler-alist mage--file-name-handler-alist))
(add-hook 'before-init-hook #'mage|pre-init)
(add-hook 'emacs-startup-hook #'mage|post-init)

(dolist (package-path (directory-files package-user-dir t))
  (if (directory-name-p package-path)
      (add-to-list 'load-path package-path)))

(defvar mage-init-time 'nil)
(defun mage-display-benchmark()
  (message "Mage loaded %s packages in %.03fs"
           (length package-activated-list)
           (or mage-init-time
               (setq mage-init-time
                     (float-time (time-subtract (current-time) before-init-time))))))
(add-hook 'emacs-startup-hook #'mage-display-benchmark)

(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq-default initial-major-mode 'fundamental-mode)

(load custom-file t t t)

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     
(prefer-coding-system        'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(setq-default debug-on-error nil)

(setq-default initial-scratch-message nil)

(setq-default inhibit-startup-screen t)

(setq-default auto-save-default nil
	  create-lockfiles nil
	  make-backup-files nil
	  history-length 500)

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default tab-width 4)

(setq package-enable-at-startup nil)
(package-initialize)

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
			             ("org"   . "http://elpa.emacs-china.org/org/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-verbose nil)
(setq use-package-always-ensure t)
(use-package use-package-ensure-system-package)

(setq load-prefer-newer t)

(use-package auto-compile
  :defer t
  :config (auto-compile-on-load-mode))

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t))

(defun +my/better-font()
  (interactive)
  ;; english font
  (if (display-graphic-p)
  (progn
	(set-face-attribute 'default nil :font (format   "%s:pixelsize=%d" "Source Code Pro" 17)) ;; 11 13 17 19 23
	;; chinese font
	(dolist (charset '(kana han symbol cjk-misc bopomofo))
	  (set-fontset-font (frame-parameter nil 'font)
			charset
			(font-spec :family "WenQuanYi Micro Hei Mono" :size 20)))) ;; 14 16 20 22 28
))

(defun +my|init-font(frame)
  (with-selected-frame frame
(if (display-graphic-p)
	(+my/better-font))))

(if (and (fboundp 'daemonp) (daemonp))
(add-hook 'after-make-frame-functions #'+my|init-font)
  (+my/better-font))

(defun mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
(setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
(setq reserve (- reserve 3)))
  (propertize " "
	  'display `((space :align-to
				(- (+ right right-fringe right-margin) ,reserve)))
	  'face face))

(setq projectile-mode-line
  (quote (:eval (when (and (fboundp 'projectile-project-p) (projectile-project-p))
		  (propertize (format " P[%s]" (projectile-project-name))
				  'face 'font-lock-variable-name-face)))))

(setq buffer-name-mode-line
  (quote (:eval (propertize "%b " 'face 'font-lock-string-face))))

(setq major-mode-mode-line
  (quote (:eval (propertize "%m " 'face 'font-lock-keyword-face))))

(setq file-status-mode-line 
  (quote (:eval (concat "["
			(when (buffer-modified-p)
			  (propertize "Mod "
					  'face 'font-lock-warning-face
					  'help-echo "Buffer has been modified"))
			(propertize (if overwrite-mode "Ovr" "Ins")
					'face 'font-lock-preprocessor-face
					'help-echo (concat "Buffer is in "
						   (if overwrite-mode
							   "overwrite"
							 "insert") " mode"))
			(when buffer-read-only
			  (propertize " RO"
					  'face 'font-lock-type-face
					  'help-echo "Buffer is read-only"))
			"]"))))

(setq flycheck-status-mode-line
  (quote (:eval (when (boundp 'flycheck-last-status-change)
		  (pcase flycheck-last-status-change
			(`finished
			 (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
				(errors (cdr (assq 'error error-counts)))
				(warnings (cdr (assq 'warning error-counts)))
				(face (cond (errors 'error)
					(warnings 'warning)
					(t 'success))))
		   (propertize (concat "["
					   (cond
						(errors (format "✗:%s" errors))
						(warnings (format "❗:%s" warnings))
						(t "✔"))
					   "]")
				   'face face))))))))



(setq line-column-mode-line
  (concat
   "("
   (propertize "%02l" 'face 'font-lock-type-face)
   ":"
   (propertize "%02c" 'face 'font-lock-type-face)
   ")"))

(setq encoding-mode-line
  (quote (:eval (propertize 
		 (concat (pcase (coding-system-eol-type buffer-file-coding-system)
			   (0 "LF ")
			   (1 "CRLF ")
			   (2 "CR "))
			 (let ((sys (coding-system-plist buffer-file-coding-system)))
			   (if (memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
			   "UTF-8"
				 (upcase (symbol-name (plist-get sys :name)))))
			 )))))

(setq time-mode-line
  (quote (:eval (propertize (format-time-string "%H:%M")))))

(setq-default mode-line-format
	  (list
	   " %1"
	   major-mode-mode-line
	   " %1"
	   buffer-name-mode-line
	   " %1"
	   file-status-mode-line
	   " %1"
	   projectile-mode-line
	   " %1"
	   line-column-mode-line
	   " "
	   flycheck-status-mode-line
	   (mode-line-fill 'mode-line 15)
	   encoding-mode-line
	   " "
	   time-mode-line
	   ))

(use-package rainbow-delimiters
  :defer 1
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package recentf
  :defer 1
  :commands recentf-open-files
  :config
  (setq recentf-save-file (concat mage-cache-dir "recentf")
	recentf-auto-cleanup 120
	recentf-max-menu-items 0
	recentf-max-saved-items 300
	recentf-filename-handlers '(file-truename)
	)
  ;; 关闭载入recentf文件的提示
  (cl-letf* (((symbol-function 'load-file) (lambda (file) (load file nil t) )))
(recentf-mode +1)))

(show-paren-mode +1)

(use-package popwin
  :defer 1
  :config
  (popwin-mode +1))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode +1)
  )

(use-package isolate
  :defer 1
  :load-path (lambda() (concat mage-ext-dir "isolate/"))
  :config
  (add-to-list 'isolate-pair-list
	   '(
		 (from . "os-\\(.*\\)4")
		 (to-left . (lambda(from)
			  (format "#+BEGIN_SRC %s\n" (match-string 1 from))))
		 (to-right . "\n#+END_SRC\n")
		 (condition . (lambda (_) (if (equal major-mode 'org-mode) t nil)))
		 )
	   ))

(use-package auto-save
  :defer 1
  :load-path (mage-ext-dir)
  :config
  (setq auto-save-slient t))

(defvar mage-auto-save-timer nil)

  (defun mage/toggle-auto-save()
(interactive)
(if mage-auto-save-timer
	(progn
	  (cancel-timer mage-auto-save-timer)
	  (setq mage-auto-save-timer nil)
	  (message "auto-save disabled"))
  (progn
	(setq mage-auto-save-timer (auto-save-enable))
	(message "auto-save enabled"))
  ))

(use-package undo-tree
   :defer 1
   :config
   (setq undo-tree-auto-save-history nil
	 ;; undo-in-region is known to cause undo history corruption, which can
	 ;; be very destructive! Disabling it deters the error, but does not fix
	 ;; it entirely!
	 undo-tree-enable-undo-in-region nil
	 undo-tree-history-directory-alist
	 `(("." . ,(concat mage-cache-dir "undo-tree-hist/"))))
   (global-undo-tree-mode +1)

;; compress undo history with xz
   (defun mage*undo-tree-make-history-save-file-name (file)
 (cond ((executable-find "zstd") (concat file ".zst"))
	   ((executable-find "gzip") (concat file ".gz"))
	   (file)))
   (advice-add #'undo-tree-make-history-save-file-name :filter-return
	   #'mage*undo-tree-make-history-save-file-name)

   (defun mage*strip-text-properties-from-undo-history (&rest args)
 (dolist (item buffer-undo-list)
   (and (consp item)
	(stringp (car item))
	(setcar item (substring-no-properties (car item))))))
   (advice-add 'undo-list-transfer-to-tree :before #'mage*strip-text-properties-from-undo-history)

   (defun mage*compress-undo-tree-history (orig-fn &rest args)
 (cl-letf* ((jka-compr-verbose nil)
		(old-write-region (symbol-function #'write-region))
		((symbol-function #'write-region)
		 (lambda (start end filename &optional append _visit &rest args)
		   (apply old-write-region start end filename append 0 args))))
   (apply orig-fn args)))
   (advice-add #'undo-tree-save-history :around #'mage*compress-undo-tree-history))

(use-package avy
  :defer 1
  :bind (("C-;" . avy-goto-char)))

(use-package expand-region
  :defer 1
  :bind (("C-=" . er/expand-region)))

(defun mage-reload-config()
  (interactive)
  (load user-init-file nil t))

(use-package projectile
  :defer 1
  :init
  (setq projectile-cache-file (concat mage-cache-dir "projectile.cache")
	projectile-enable-caching (not noninteractive)
	projectile-indexing-method 'alien
	projectile-known-projects-file (concat mage-cache-dir "projectile.projects")
	projectile-require-project-root t
	projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
	projectile-ignored-projects '("~/" "/tmp" "/usr/include"))
  :config
  (projectile-mode +1)
  )

(use-package company
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
	  company-backends '((:separate company-capf company-yasnippet)))
:config
(global-company-mode +1))

(use-package company-posframe
  :if (display-graphic-p)
  :after company
  :hook (company-mode . company-posframe-mode))

(use-package yasnippet
  :defer t
  :commands (yas-minor-mode-on yas-expand yas-expand-snippet yas-lookup-snippet
			   yas-insert-snippet yas-new-snippet yas-visit-snippet-file)
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode-on))
(use-package yasnippet-snippets
  :after yasnippet)

(use-package ivy
:config
(setq ivy-initial-inputs-alist nil
	  ivy-wrap t
	  ivy-height 15
	  ivy-fixed-height-minibuffer t
	  ivy-format-function #'ivy-format-function-line
	  )
(ivy-mode +1)
:bind ([remap switch-to-buffer] . #'ivy-switch-buffer)
)

(use-package ivy-posframe
:after (ivy)
:config
(setq ivy-display-function #'ivy-posframe-display-at-point
	  ivy-fixed-height-minibuffer nil
	  ivy-posframe-parameters
	  `((min-width . 90)
	(min-height .,ivy-height)
	(internal-border-width . 10))))

(use-package counsel
     :after (ivy)
  :bind (([remap execute-extended-command] . counsel-M-x)
	 ([remap find-file]                . counsel-find-file)
	 ([remap find-library]             . find-library)
	 ([remap imenu]                    . counsel-imenu)
	 ([remap recentf-open-files]       . counsel-recentf)
	 ([remap org-capture]              . counsel-org-capture)
	 ([remape swiper]                  . counsel-grep-or-swiper)
	 ([remap describe-face]            . counsel-describe-face)
	 ([remap describe-function]        . counsel-describe-function)
	 ([remap describe-variable]        . counsel-describe-variable))

  :config
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
	counsel-rg-base-command "rg -zS --no-heading --line-number --color never %s ."
	counsel-ag-base-command "ag -zS --nocolor --nogroup %s"
	counsel-pt-base-command "pt -zS --nocolor --nogroup -e %s")
  )

(use-package swiper
:defer 1
:bind ("C-s" . swiper))

(use-package counsel-projectile
  :after projectile
  :commands (counsel-projectile-find-file counsel-projectile-find-dir counsel-projectile-switch-to-buffer
                                          counsel-projectile-grep counsel-projectile-ag counsel-projectile-switch-project)
  :init
  :bind (([remap projectile-find-file]        . counsel-projectile-find-file)
         ([remap projectile-find-dir]         . counsel-projectile-find-dir)
         ([remap projectile-switch-to-buffer] . counsel-projectile-switch-to-buffer)
         ([remap projectile-grep]             . counsel-projectile-grep)
         ([remap projectile-ag]               . counsel-projectile-ag)
         ([remap projectile-switch-project]   . counsel-projectile-switch-project)))

(use-package flx
:defer t
:init
(setq ivy-re-builders-alist
	  '((counsel-ag . ivy--regex-plus)
	(counsel-grep . ivy--regex-plus)
	(swiper . ivy--regex-plus)
	(t . ivy--regex-fuzzy))
	  ivy-initial-inputs-alist nil)
)

(use-package company-english-helper
  :defer t
  :commands (toggle-company-english-helper)
  :load-path (lambda() (concat mage-ext-dir "/english-helper")))

(use-package helpful
  :defer 1
  :init
  (setq counsel-describe-function-function #'helpful-function
	counsel-describe-variable-function #'helpful-variable)
  :bind (([remap describe-key] . helpful-key))
  )

(use-package which-key
  :defer 1
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (which-key-mode +1)
  )

(use-package flycheck
  :defer t
  )

(use-package flycheck-posframe
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package lsp-mode
  :defer 1
  :config
  (setq lsp-project-blacklist '("^/usr/")
	lsp-highlight-symbol-at-point nil)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

(use-package lsp-ui
  :after (lsp-mode)
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable nil
	lsp-ui-sideline-show-symbol nil
	lsp-ui-sideline-show-symbol nil
	lsp-ui-sideline-ignore-duplicate t
	lsp-ui-doc-max-width 50
	)
  :bind (:map lsp-ui-peek-mode-map
	  ("h" . lsp-ui-peek--select-prev-file)
	  ("j" . lsp-ui-peek--select-next)
	  ("k" . lsp-ui-peek--select-prev)
	  ("l" . lsp-ui-peek--select-next-file)
	  :map lsp-ui-mode-map
	  ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	  ([remap xref-find-references]  . lsp-ui-peek-find-references)
	  ))

(use-package company-lsp
  :after (company lsp-mode)
  :init
  (setq company-lsp-cache-candidates nil)
  (add-hook 'lsp-mode-hook
	(lambda()
	  (add-to-list (make-local-variable 'company-backends)
		   'company-lsp)))
  )

(use-package cc-mode
  :init
  (setq-default c-basic-offset tab-width)
  :bind (:map c-mode-base-map
		 ("RET" . newline-and-indent))
  :config
  ;; smartparens do it
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1)
  (setq c-tab-always-indent nil
	c-electric-flag nil)
  (dolist (key '("#" "}" "/" "*" ";" "," ":" "(" ")" "\177"))
  (define-key c-mode-base-map key nil))
  (define-key c++-mode-map "<" nil)
  (define-key c++-mode-map ">" nil)
  (sp-with-modes '(c-mode c++-mode java-mode)
	(sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
	(sp-local-pair "/*" "*/" :post-handlers '(("||\n[i]" "RET")))
	)
  )

(use-package ccls
  :ensure-system-package ccls
  :defer 1
  :init
  (defun mage-lang-ccls-enable()
(condition-case nil
	(lsp-ccls-enable)
  (user-error nil)))
  (add-hook 'c-mode-hook #'mage-lang-ccls-enable)
  (add-hook 'c++-mode-hook #'mage-lang-ccls-enable)
  :config
  ;; overlay is slow
  ;; Use https://github.com/emacs-mirror/emacs/commits/feature/noverlay
  (setq ccls-sem-highlight-method 'font-lock)
  (ccls-use-default-rainbow-sem-highlight)
  (setq ccls-executable "/usr/bin/ccls")
  ;;(setq ccls-extra-args '("--log-file=/tmp/cq.log"))
  (setq ccls-extra-init-params '(
				 :completion (:detailedLabel t)
				 :diagnostics (:frequencyMs 5000)
				 :index (:reparseForDependency 1)))
  (with-eval-after-load 'projectile
(progn (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
   (setq projectile-project-root-files-top-down-recurring
		 (append '("compile_commands.json"
		   ".ccls_root")
			 projectile-project-root-files-top-down-recurring))))
  )

(use-package clang-format
  :after (ccls)
  :commands (clang-format-region clang-format-buffer))

(use-package google-c-style
  :defer 1
  :config
  (add-hook 'c-mode-hook #'google-set-c-style)
  (add-hook 'c++-mode-hook #'google-set-c-style))

(use-package cmake-mode
  :defer 1)

  (use-package cmake-project
:ensure-system-package cmake
:defer 1
:load-path (mage-ext-dir)
:bind (("<f7>" . cp-cmake-build-project)
   ("<f8>" . cp-cmake-run-project-with-args)))

(use-package modern-cpp-font-lock
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(use-package python
  :defer 1
  :init
  (setq python-environment-directory mage-cache-dir
	python-indent-guess-indent-offset-verbose nil
	)
  :config

  (define-key python-mode-map (kbd "DEL") nil) ; interferes with smartparens

  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt --no-color-info"
	python-shell-prompt-regexp "In \\[[0-9]+\\]: "
	python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
	python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
	python-shell-completion-setup-code
	"from IPython.core.completerlib import module_completion"
	python-shell-completion-string-code
	"';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

(use-package lsp-python
  :defer 1
  :after python
  :config
  (add-hook 'python-mode-hook #'lsp-python-enable))

(defvar mage-org-dir "~/Documents/Org/")
(use-package org
  :defer 1
  :ensure org-plus-contrib
  :config
  (setq org-ellipsis " ▼ "
	org-src-window-setup 'current-window
	org-image-actual-width '(400)
	org-export-backends '(ascii html latex md odt pandoc)
	org-ditaa-jar-path (concat mage-etc-dir "ditaa.jar")
	org-agenda-files (list (concat mage-org-dir "gtd.org"))
	)
  (setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{[:nonascii:]")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[[:nonascii:]")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-element-update-syntax)
  )
(use-package org-bullets
  :defer 1
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package ox-pandoc
  :defer 1)

(setq org-default-notes-file (expand-file-name "inbox.org" mage-org-dir))
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-capture-templates nil)
(with-eval-after-load 'org
  (require 'org-protocol))

(setq org-capture-templates `(
	("P" "Protocol" entry (file+headline ,(concat mage-org-dir "web.org") "Inbox")
	"* [[%:link]]\nSource: %u\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	("L" "Protocol Link" entry (file+headline ,(concat mage-org-dir "web.org") "Inbox")
	"* %? [[%:link][%:description]] \nCaptured On: %U")
))

(defvar mage-root-dir
  "~/Documents/Blog/"
  "Blog root directory.")

(defvar mage-img-dir
  (concat mage-root-dir "images/")
  "Blog's image directory.")

(defvar mage-res-url
  "http://source.xhcoding.cn/")

(use-package ctable
   :defer 1)
 (use-package epic
   :defer 1)                 
 (use-package orglue
   :defer 1)                 

 (use-package org-octopress
   :after (ctable epic orglue)
   :load-path (lambda() (concat mage-ext-dir "org-octopress/"))
   :config
   (require 'ox-jekyll)
   (setq
org-octopress-directory-top (expand-file-name "source" mage-root-dir)
org-octopress-directory-posts (expand-file-name "source/_posts" mage-root-dir)
org-octopress-directory-org-top mage-root-dir
org-octopress-directory-org-posts (expand-file-name "blog" mage-root-dir)
org-octopress-setup-file (expand-file-name "setupfile.org" mage-root-dir)
)      
   )

 (defun mage/open-org-octopress()
   (interactive)
   (let ((buffer (get-buffer "Octopress")))
 (if buffer
	 (switch-to-buffer buffer)
   (org-octopress))))

(defun mage*export-blog-image-url(filename)
  (if (equal
   (string-match-p
	(regexp-quote (expand-file-name mage-root-dir))
	(expand-file-name filename))
   0)
  (concat  mage-res-url (string-trim-left filename mage-img-dir))
nil))


(advice-add #'org-export-file-uri :before-until #'mage*export-blog-image-url)

(defun mage-kill-new-img-link(prefix imagename)
   (kill-new (format "[[file:%s%s]] " prefix imagename imagename)))


 ;;;###autoload
 (defun mage/capture-screenshot(basename)
   (interactive "sScreenshot name: ")
   (if (string-equal basename "")
   (setq basename
	 (file-name-base buffer-file-name)))
   (setq filename (concat basename (format-time-string "_%Y%H%M%S")))
   (sleep-for 3)
   (call-process-shell-command
(concat
 "deepin-screenshot -s " (concat (expand-file-name mage-img-dir) filename)))
   (mage-kill-new-img-link
mage-img-dir (concat filename ".png")))

(defun mage/org-save-and-export ()
  (interactive)
  (org-octopress-setup-publish-project)
  (org-publish-project "octopress" t))

(use-package hydra
  :commands (defhydra))
(use-package ivy-hydra
  :after (ivy hydra))

(global-set-key
  (kbd "C-n")
  (defhydra hydra-move (:pre-body (next-line))
"move"
("j" next-line)
("k" previous-line)
("l" forward-char)
("L" forward-word)
("H" backward-word)
("a" beginning-of-line)
("e" move-end-of-line)
("E" end-of-buffer)
("B" beginning-of-buffer)
("v" scroll-up-command)
;; Converting M-v to V here by analogy.
("V" scroll-down-command)
("c" recenter-top-bottom)
("g" goto-line "go")
("m" set-mark-command "mark" :bind nil)
("q" nil "quit")
))

(with-eval-after-load 'ivy
(define-key ivy-minibuffer-map "\C-o"
  (defhydra soo-ivy (:hint nil :color pink)
	"
   Move     ^^^^^^^^^^ | Call         ^^^^ | Cancel^^ | Options^^ | Action _w_/_s_/_a_: %s(ivy-action-name)
  ----------^^^^^^^^^^-+--------------^^^^-+-------^^-+--------^^-+---------------------------------
   _g_ ^ ^ _k_ ^ ^ _u_ | _f_orward _o_ccur | _i_nsert | _c_alling: %-7s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
   ^↨^ _h_ ^+^ _l_ ^↕^ | _RET_ done     ^^ | _q_uit   | _m_atcher: %-7s(ivy--matcher-desc) _t_runcate: %-11`truncate-lines
   _G_ ^ ^ _j_ ^ ^ _d_ | _TAB_ alt-done ^^ | ^ ^      | _<_/_>_: shrink/grow
  "
	;; arrows
	("j" ivy-next-line)
	("k" ivy-previous-line)
	("l" ivy-alt-done)
	("h" ivy-backward-delete-char)
	("g" ivy-beginning-of-buffer)
	("G" ivy-end-of-buffer)
	("d" ivy-scroll-up-command)
	("u" ivy-scroll-down-command)
	("e" ivy-scroll-down-command)
	;; actions
	("q" keyboard-escape-quit :exit t)
	("C-g" keyboard-escape-quit :exit t)
	("<escape>" keyboard-escape-quit :exit t)
	("C-o" nil)
	("i" nil)
	("TAB" ivy-alt-done :exit nil)
	("C-j" ivy-alt-done :exit nil)
	;; ("d" ivy-done :exit t)
	("RET" ivy-done :exit t)
	("C-m" ivy-done :exit t)
	("f" ivy-call)
	("c" ivy-toggle-calling)
	("m" ivy-toggle-fuzzy)
	(">" ivy-minibuffer-grow)
	("<" ivy-minibuffer-shrink)
	("w" ivy-prev-action)
	("s" ivy-next-action)
	("a" ivy-read-action)
	("t" (setq truncate-lines (not truncate-lines)))
	("C" ivy-toggle-case-fold)
	("o" ivy-occur :exit t))))

(with-eval-after-load 'projectile
(defhydra hydra-file (global-map "C-c f")
  "file"
  ("f" counsel-find-file)
  ("r" counsel-recentf)
  ("p" counsel-projectile-switch-project)
  ("q" nil)))

(defun my/name-of-buffers (n)
"Return the names of the first N buffers from `buffer-list'."
(let ((bns
	   (delq nil
		 (mapcar
		  (lambda (b)
		(unless (string-match "^ " (setq b (buffer-name b)))
		  b))
		  (buffer-list)))))
  (subseq bns 1 (min (1+ n) (length bns)))))

  ;; Given ("a", "b", "c"), return "1. a, 2. b, 3. c".
  (defun my/number-names (list)
"Enumerate and concatenate LIST."
(let ((i 0))
  (mapconcat
   (lambda (x)
	 (format "%d. %s" (cl-incf i) x))
   list
   ", ")))

  (defvar my/last-buffers nil)

  (defun my/switch-to-buffer (arg)
(interactive "p")
(switch-to-buffer
 (nth (1- arg) my/last-buffers)))

  (defun my/switch-to-buffer-other-window (arg)
(interactive "p")
(switch-to-buffer-other-window
 (nth (1- arg) my/last-buffers)))

  (global-set-key
   (kbd "C-c b")
   (defhydra my/switch-to-buffer (:exit t
				  :body-pre (setq my/last-buffers
						  (my/name-of-buffers 4)))
 "
  _o_ther buffers: %s(my/number-names my/last-buffers)

  "
 ("o" my/switch-to-buffer "this window")
 ("O" my/switch-to-buffer-other-window "other window")
 ("<escape>" nil)))

(defhydra hydra-toggle (global-map "C-c t")
"toggle"
("a" mage/toggle-auto-save)
("f" flycheck-mode)
)

(defhydra hydra-jump (global-map "C-c j")
"jump"
("c" avy-goto-char)
("e" avy-goto-word-0)
("w" avy-goto-word-1)
("(" avy-goto-open-paren)
(")" avy-goto-close-paren)
("j" flycheck-next-error)
("k" flycheck-previous-error)
)

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
				   :color pink
				   :hint nil
				   :post (deactivate-mark))
"
^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
  _h_   _l_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \-;;,_
^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
  ^^^^          _u_ndo        _g_ quit     ^ ^                     '---''(./..)-'(_\_)
  "
("k" rectangle-previous-line)
("j" rectangle-next-line)
("h" rectangle-backward-char)
("l" rectangle-forward-char)
("d" kill-rectangle)                    ;; C-x r k
("y" yank-rectangle)                    ;; C-x r y
("w" copy-rectangle-as-kill)            ;; C-x r M-w
("o" open-rectangle)                    ;; C-x r o
("t" string-rectangle)                  ;; C-x r t
("c" clear-rectangle)                   ;; C-x r c
("e" rectangle-exchange-point-and-mark) ;; C-x C-x
("N" rectangle-number-lines)            ;; C-x r N
("r" (if (region-active-p)
	 (deactivate-mark)
	   (rectangle-mark-mode 1)))
("u" undo nil)
("g" nil))      ;; ok
  (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)

(defhydra hydra-mark (:color blue :idle 1.5 :columns 4)
"Mark"
("m" set-mark-command "Set Mark" :bind nil)
	("d" er/mark-defun "Defun / Function")
	("f" er/mark-defun "Defun / Function")
	("w" er/mark-word "Word")
	("u" er/mark-url "Url")
	("e" mark-sexp "S-Expression")
	("E" er/mark-email "Email")
	("b" mark-whole-buffer "Buffer")
	("l" mage-mark-line "Line")
	("s" er/mark-sentence "Sentence")
	("p" er/mark-text-paragraph "Paragraph")
	("g" mark-page "Page")
	("S" er/mark-symbol "Symbol")
	("P" er/mark-symbol-with-prefix "Prefixed symbol")
	("q" er/mark-inside-quotes "Inside Quotes")
	("Q" er/mark-outside-quotes "Outside Quotes")
	("(" er/mark-inside-pairs "Inside Pairs")
	("[" er/mark-inside-pairs "Inside Pairs")
	("{" er/mark-inside-pairs "Inside Pairs")
	(")" er/mark-outside-pairs "Outside Pairs")
	("]" er/mark-outside-pairs "Outside Pairs")
	("}" er/mark-outside-pairs "Outside Pairs")
	("t" er/mark-inner-tag "Inner Tag")
	("T" er/mark-outer-tag "Outer Tag")
	("c" er/mark-comment "Comment")
	("a" er/mark-html-attribute "HTML Attribute")
	("." er/expand-region "Expand Region" :exit nil)
	("," er/contract-region "Contract Region" :exit nil))
  (global-set-key (kbd "C-SPC") #'hydra-mark/body)

(defhydra hydra-all (global-map "M-p" :hint nil)
"hydra-all"
("m" hydra-move/body)
("f" hydra-file/body)
("b" hydra-buffer/body)
("t" hydra-toggle/body)
("j" hydra-jump/body)
("q" nil))
