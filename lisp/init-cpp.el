

(setq c-basic-offset 4)


;; irony
(use-package irony
  :diminish irony-mode "IR"
  :init
  (progn
    (setq irony-additional-clang-options '("-std=c++11"))
    (add-hook 'c-mode-common-hook 'irony-mode)
    ;;(add-hook 'objc-mode-hook 'irony-mode)
    ;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    )
  :config
    (use-package company-irony-c-headers)
    (use-package company-irony
      :config
      (add-hook 'c-mode-common-hook
		(lambda ()
		  (add-to-list (make-local-variable 'company-backends)
			       '(company-irony company-irony-c-headers))))
      )
    (use-package flycheck-irony
      :defer t
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
  :diminish counsel-gtags-mode "CG"
  :config
  (progn
    (setenv "GTAGSLIBPATH" (concat (expand-file-name "gtags-datas" user-emacs-directory)
				   "/usr/include"))
    (add-hook 'c-mode-hook 'counsel-gtags-mode)
    (add-hook 'c++-mode-hook 'counsel-gtags-mode)))

(provide 'init-cpp)
