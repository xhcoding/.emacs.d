

(setq c-basic-offset 4)

;;=============================cquery==========================================

(use-package lsp-mode
  :config
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-highlight-symbol-at-point nil)
  (defun cquery/base () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/base"))
  (defun cquery/callers () (interactive) (lsp-ui-peek-find-custom 'callers "$cquery/callers"))
  (defun cquery/derived () (interactive) (lsp-ui-peek-find-custom 'derived "$cquery/derived"))
  (defun cquery/vars () (interactive) (lsp-ui-peek-find-custom 'vars "$cquery/vars"))
  (defun cquery/random () (interactive) (lsp-ui-peek-find-custom 'random "$cquery/random"))
  )

(use-package cquery
  :config
  (progn
    (require 'cquery)
    (setq cquery-executable
	  "/usr/bin/cquery")
    ;;   (setq cquery-sem-highlight-method 'font-lock)
    ;; (cquery-use-default-rainbow-sem-highlight)
    (setq cquery-extra-init-params
          '(:cacheFormat "msgpack" :completion (:detailedLabel t) :xref (:container t)
                         :diagnostics (:frequencyMs 5000)))
    (require 'projectile)
    (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index")
    (add-to-list 'projectile-globally-ignored-directories "build")
    (add-hook 'c-mode-hook 'lsp-cquery-enable)
    (add-hook 'c++-mode-hook 'lsp-cquery-enable)

    
    (use-package company-lsp
      :config
      (progn
	(setq
	 company-transformers nil
	 company-lsp-async t
	 company-lsp-cache-candidates nil)
	(add-hook 'c-mode-common-hook
		  (lambda ()
		    (add-to-list (make-local-variable 'company-backends)
				 '(company-lsp))))
	))
    
    (use-package lsp-ui
      :config
      (progn
	(require 'lsp-ui)
	(add-hook 'lsp-mode-hook 'lsp-ui-mode)
	(define-key c-mode-base-map (kbd "M-.") 'lsp-ui-peek-find-definitions)
	(define-key c-mode-base-map (kbd "M-?") 'lsp-ui-peek-find-references)
	(define-key lsp-ui-peek-mode-map (kbd "h") 'lsp-ui-peek--select-prev-file)
	(define-key lsp-ui-peek-mode-map (kbd "l") 'lsp-ui-peek--select-next-file)
	(define-key lsp-ui-peek-mode-map (kbd "j") 'lsp-ui-peek--select-next)
	(define-key lsp-ui-peek-mode-map (kbd "k") 'lsp-ui-peek--select-prev)
	(setq lsp-ui-doc-include-signature nil)
	(setq lsp-ui-sideline-enable nil)
	(setq lsp-ui-sideline-show-symbol nil)  ;;don't show symbol on the right of info
	(setq lsp-ui-sideline-ignore-duplicate t)
	))))

(use-package ivy-xref
  :disabled t
  :config
  :after 'ivy
  :config
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))


;;=====================================================================

;; irony
;; (use-package irony
;;   :diminish irony-mode "IR"
;;   :init
;;   (progn
;;     (setq irony-additional-clang-options '("-std=c++11"))
;;     (add-hook 'c-mode-hook 'irony-mode)
;;     (add-hook 'c++-mode-hook 'irony-mode)
;;     )
;;   :config
;;   (use-package company-irony-c-headers)
;;   (use-package company-irony
;;     :config
;;     (add-hook 'c-mode-common-hook
;; 	      (lambda ()
;; 		(add-to-list (make-local-variable 'company-backends)
;; 			     '(company-irony company-irony-c-headers))))
;;     )
;;   (use-package flycheck-irony
;;     :defer t
;;     :init
;;     (flycheck-irony-setup))
;;   )



(use-package cmake-mode)

(use-package cpputils-cmake
  :config
  (progn
    (add-hook 'c-mode-common-hook
	      (lambda ()
		(if (derived-mode-p 'c-mode 'c++-mode)
		    (cppcm-reload-all)
		  )))
    ;; OPTIONAL, somebody reported that they can use this package with Fortran
    ;;(add-hook 'c90-mode-hook (lambda () (cppcm-reload-all)))
    ;; OPTIONAL, avoid typing full path when starting gdb
    (global-set-key (kbd "C-c C-g")
		    '(lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))
    (global-set-key (kbd "<f7>") 'compile)
    
    ;; OPTIONAL, some users need specify extra flags forwarded to compiler
    ;;(setq cppcm-extra-preprocss-flags-from-user '("-I/usr/src/linux-headers-4.9.0-deepin11-amd64/include" "-DNDEBUG"))
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
    (define-key c-mode-base-map (kbd "S-<f7>") 'compile-current-buffer)
    (define-key c-mode-base-map (kbd "M-f") 'ff-find-other-file)
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
