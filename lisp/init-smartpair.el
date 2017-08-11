;; 需要的包
(require-package 'smartparens)
(require-package 'rainbow-delimiters)

;; 开启模式
(smartparens-global-mode t)
(show-paren-mode t)

;;在emacs-lisp中不补全单引号
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

;;在c++-mode中不补全小括号
(sp-local-pair 'c++-mode "(" nil :actions nil)

;; 高亮括号的延迟
(setq show-paren-delay 0)

;;不同层次的括号显示不同颜色
(when (maybe-require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;显示配对括号，当光标在括号内
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens"
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))



;;把括号换成其他括号
(global-set-key (kbd "C-c p r") 'sp-rewrap-sexp)

;;去掉括号
(global-set-key (kbd "C-c p u") 'sp-splice-sexp)
(provide 'init-smartpair)
