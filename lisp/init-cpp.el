

(setq c-basic-offset 4)


(use-package lsp-mode)

(use-package cquery
  :load-path "site-lisp/"
  :config
  (progn
    (require 'cquery)
    (setq cquery-executable
	  "/home/xhcoding/Tools/cquery/build/release/bin/cquery")
    (setq cquery-extra-init-params '(:enableComments 2 :cacheFormat "msgpack"))
    (setq cquery-sem-highlight-method 'font-lock)
    (add-hook 'c-mode-common-hook 'lsp-cquery-enable)

    
    (use-package company-lsp
      :config
      (add-hook 'c-mode-common-hook
		(lambda ()
		  (add-to-list (make-local-variable 'company-backends)
			       '(company-lsp)))) )
    
    (use-package lsp-ui
      :config
      (progn
	(require 'lsp-ui)
	(add-hook 'lsp-mode-hook 'lsp-ui-mode)
	))))

(use-package ivy-xref
  :config
  :after 'ivy
  :config
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))

;; irony
(use-package irony :disabled t
  :diminish irony-mode "IR"
  :init
  (progn
    (setq irony-additional-clang-options '("-std=c++11"))
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'c++-mode-hook 'irony-mode)
    )
  :config
  (use-package company-irony-c-headers
    :disabled t)
  (use-package company-irony
    :disabled t
    :config
    (add-hook 'c-mode-common-hook
	      (lambda ()
		(add-to-list (make-local-variable 'company-backends)
			     '(company-irony company-irony-c-headers))))
    )
  (use-package flycheck-irony
    :defer t
    :disabled t
    :init
    (flycheck-irony-setup))
  )





(use-package cpputils-cmake :disabled t
  :config
  (progn
    (add-hook 'c-mode-common-hook
	      (lambda ()
		(if (derived-mode-p 'c-mode 'c++-mode)
		    (cppcm-reload-all)
		  )))
    ;; OPTIONAL, somebody reported that they can use this package with Fortran
    (add-hook 'c90-mode-hook (lambda () (cppcm-reload-all)))
    ;; OPTIONAL, avoid typing full path when starting gdb
    (global-set-key (kbd "C-c C-g")
		    '(lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))
    ;; OPTIONAL, some users need specify extra flags forwarded to compiler
    (setq cppcm-extra-preprocss-flags-from-user '("-I/usr/src/linux-headers-4.9.0-deepin11-amd64/include" "-DNDEBUG"))
    ))

(use-package counsel-gtags
  :disabled t
  :diminish counsel-gtags-mode "CG"
  :config
  (progn
    (setenv "GTAGSLIBPATH" (concat (expand-file-name "gtags-datas" user-emacs-directory)
				   "/usr/include"))
    (add-hook 'c-mode-hook 'counsel-gtags-mode)
    (add-hook 'c++-mode-hook 'counsel-gtags-mode)))


;; setting compile command by current buffer

(defun set-compile-command()
  "Setting compile command by current buffer"
  (interactive)
  (progn
    (setq file-path (buffer-file-name))
    (setq file-name-extension (file-name-extension file-path))
    (setq file-name (concat (file-name-base file-path) "." file-name-extension)))
  (cond
   ((file-exists-p "Makefile")
    (setq compile-command "make"))
   ((string-equal "c" file-name-extension)
    (setq compile-command (concat "gcc -g -Wall -std=c11 -o " (file-name-base file-path) " " file-name))
    )
   ((string-equal "cpp" file-name-extension)
    (setq compile-command (concat "g++ -g -Wall  -std=c++11 -o " (file-name-base file-path) " " file-name))
    )
   ))

(defun compile-current-buffer()
  "Compile current buffer"
  (interactive)
  (set-compile-command)
  (compile compile-command))

(use-package cc-mode
  :config
  (progn
    (define-key c-mode-map (kbd "<f7>") 'compile-current-buffer)
    (define-key c++-mode-map (kbd "<f7>") 'compile-current-buffer)
    ))


(defun ins-c++-curly ()
  "Insert {}.
Threat is as function body when from endline before )"
  (interactive)
  (if (looking-back "\\()\\|) \\|try\\|else[ ]\\|const\\|:\\)[ ]?$"
		    (line-beginning-position))
      (progn
        (insert " {\n\n}")
        (indent-according-to-mode)
        (forward-line -1)
        (indent-according-to-mode))
    (insert "{}")
    (backward-char)))

(defun my-c-common-hook ()
  (define-key c-mode-base-map "{" 'ins-c++-curly))


(add-hook 'c-mode-common-hook 'my-c-common-hook)


(provide 'init-cpp)
