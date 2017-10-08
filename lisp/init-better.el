;; change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; turn off auto backup
(setq make-backup-files nil)

(global-auto-revert-mode t) ;; auto revert

;; sentence end with a single space
(setq sentence-end-double-space nil)

;; popwin
(use-package popwin
  :config
  (progn
    (require 'popwin)
    (popwin-mode t)))

(use-package window-numbering
  :config
  (window-numbering-mode t))

;; auto save
(use-package auto-save
  :ensure nil
  :config
  (progn
    (require 'auto-save)
    (setq auto-save-slient t)
    (auto-save-enable)
    ))

;; ivy can complete various things
(use-package ivy
  :diminish ivy-mode
  :config
  (progn
    (setq-default ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (ivy-mode t)
    (use-package swiper
      :bind ("C-s" . swiper))
    (use-package counsel
      :bind (("M-x" . counsel-M-x)
	     ("C-x C-f" . counsel-find-file))))
  :bind ("<f6>" . ivy-resume))


(use-package hungry-delete ;; hungry-delete
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode t))



;; disable clipboard manager support
(setq-default x-select-enable-clipboard-manager nil)

;; undo and redo window configuration
(use-package winner
  :defer t
  :config
  (winner-mode t))

;; miniedit
(use-package miniedit
  :commands minibuffer-edit
  :config
  (miniedit-install))

;; undo tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; guide key
(use-package which-key
  :defer t
  :diminish which-key-mode
  :config
  (progn
    (setq which-key-idle-delay 0.1)
    (which-key-mode)
    ))

;; utf-8
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; pop to mark
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;; recent file
(add-hook 'after-init-hook (lambda () (recentf-mode 1)))
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(global-set-key (kbd "C-c C-r") 'recentf-open-files)

;; smartparens
(use-package smartparens
  :defer t
  :init
  (progn
    (show-paren-mode t)
    (smartparens-global-mode t)
    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
    (setq show-paren-delay 0)

    (define-advice show-paren-function (:around (fn) fix-show-paren-function)
      "Highlight enclosing parens"
      (cond ((looking-at-p "\\s(") (funcall fn))
	    (t (save-excursion
		 (ignore-errors (backward-up-list))
		 (funcall fn)))))
    ))

(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; expand reggion
(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region)
  ("C-<prior>" . er/expand-region)
  ("C-<next>" . er/contract-region))

;; code folding
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :init
  (progn
    (add-hook 'prog-mode-hook 'hs-minor-mode)))

;; projectile
(use-package projectile
  :preface
  :defer t
  :init
  (projectile-mode)
  :config
  (progn
    (use-package counsel-projectile
      :init
      (progn
	(defvar my-simple-todo-regex "\\<\\(FIXME\\|TODO\\|BUG\\):")

	(defun my-simple-todo ()
	  "When in a project, create a `multi-occur' buffer matching the
  regex in `my-simple-todo-regex' across all buffers in the
  current project. Otherwise do `occur' in the current file."
	  (interactive)
	  (if (projectile-project-p)
	      (multi-occur (projectile-project-buffers) my-simple-todo-regex)
	    (occur my-simple-todo-regex)))
	(defun sr-open-file()
	  "Open project file"
	  (interactive)
	  (if (projectile-project-p)
	      (counsel-projectile-find-file)
	    (counsel-find-file)))))))

;; evil
(use-package evil
  :config
  (progn
    (evil-mode t)
    (use-package evil-escape
      :diminish evil-escape-mode
      :init
      (progn
	(setq-default evil-escape-key-sequence "fd"
		      evil-escape-delay 0.2)))
    :config
    (evil-escape-mode t))
  
  (use-package evil-leader
    :diminish evil-leader-mode
    :config
    (progn
      (global-evil-leader-mode t)
      (evil-leader/set-leader "<SPC>")
      ))
  (use-package evil-surround)
  (use-package evil-visualstar)
  (use-package evil-tutor))

;; flycheck
(use-package flycheck
  :defer t
  :init
  (add-hook 'after-init-hook  'global-flycheck-mode))

;; auto-completion

(use-package company
  :defer t
  :diminish company-mode " AC"
  :init
  (progn
    (setq company-idle-delay 0.2
	  company-minimum-prefix-length 2
	  company-require-match nil
	  company-dabbrev-ignore-case nil
	  company-dabbrev-downcase nil
	  company-backends
	  '((company-files
	     company-keywords
	     company-capf
	     company-yasnippet
	     )
	    (company-abbrev company-dabbrev)))
    :config
    (progn
      (with-eval-after-load 'company
	(define-key company-mode-map (kbd "M-/") 'company-complete)
	(define-key company-active-map (kbd "C-n") #'company-select-next)
	(define-key company-active-map (kbd "C-p") #'company-select-previous))
      (add-hook 'after-init-hook 'global-company-mode))))

;; yasnippet
(use-package yasnippet
  :diminish yas-minor-mode "YA"
  :config
  (progn
    (require 'yasnippet)
    (yas-reload-all)
    (add-hook 'prog-mode-hook #'yas-minor-mode)))

;; abbrev
(use-package abbrev
  :ensure nil
  :diminish abbrev-mode "AB"
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))


(use-package nasm-mode
  :mode "\\.nas\\'")

(provide 'init-better)
