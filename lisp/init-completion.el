;; 需要的包
(require-package 'company)
(require-package 'yasnippet)

(require-package 'irony)
(require-package 'company-irony)
(require-package 'flycheck-irony)
(require-package 'irony-eldoc)
(require-package 'company-irony-c-headers)


;;company配置
(add-hook 'after-init-hook 'global-company-mode)

(after-load 'company
  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (delete 'company-semantic company-backends)
  (setq company-idle-delay 0.2
	company-minimum-prefix-length 2
	company-show-numbers t))

;; company-backends
;;设置默认的backends
(setq company-backends
      '((company-files
	 company-keywords
	 company-capf
	 company-yasnippet
	 )
	(company-abbrev company-dabbrev)
	))

;;c/c++ backends
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (add-to-list (make-local-variable 'company-backends)
			 '(company-irony company-clang company-irony-c-headers company-yasnippet))))





;;irony mode
(add-hook 'c-mode-common-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(after-load 'irony
  (setq irony-additional-clang-options '("-std=c++11"))
  (flycheck-irony-setup)
  )

;; yasnippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)


;;abbrev
(add-hook 'after-init-hook 'abbrev-mode)

;;hippie

(setq hippie-expand-try-functions-list '(
					 try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-all-abbrevs
					 try-expand-list
					 try-expand-line
					 try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol))

(global-set-key (kbd "C-c /") 'hippie-expand)

(provide 'init-completion)
