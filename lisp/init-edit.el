;;; init-edit.el --- my emacs configuration -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

(use-package avy
  :bind (("C-'" . avy-goto-char-timer))
  :hook (after-init . avy-setup-default)
  :config (setq avy-all-windows nil
                avy-all-windows-alt nil
                avy-background t
                avy-style 'pre))

(use-package evil-nerd-commenter
  :bind ("C-/" . evilnc-comment-or-uncomment-lines))

(use-package auto-save
  :load-path (lambda() (expand-file-name "auto-save" talon-ext-dir))
  :init
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  :config
  (auto-save-enable)
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t))


(setq-default indent-tabs-mode nil)
(use-package whitespace
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
  )


(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode))


(use-package awesome-pair
  :load-path (lambda() (expand-file-name "awesome-pair" talon-ext-dir))
  :hook ((
          c-mode
          c++-mode
          emacs-lisp-mode
          python-mode
          js-mode
          ) . awesome-pair-mode)
  :bind (:map awesome-pair-mode-map
              ("(" . awesome-pair-open-round)
              (")" . awesome-pair-close-round)
              ("[" . awesome-pair-open-bracket)
              ("]" . awesome-pair-close-bracket)
              ("{" . awesome-pair-open-curly)
              ("}" . awesome-pair-close-curly)
              ("%" . awesome-pair-match-paren)
              ("\"" . awesome-pair-double-quote)
              ("M-o" . awesome-pair-backward-delete)
              ("C-k" . awesome-pair-kill)
              ("M-\"" . awesome-pair-wrap-double-quote)
              ("M-[" . awesome-pair-wrap-bracket)
              ("M-{" . awesome-pair-wrap-curly)
              ("M-(" . awesome-pair-wrap-round)
              ("M-]" . awesome-pair-unwrap)
              ("M-n" . awesome-pair-jump-right)
              ("M-p" . awesome-pair-jump-left)
              ))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package visual-regexp
  :commands (vr/query-replace vr/replace))


(use-package thing-edit
  :load-path (lambda() (expand-file-name "thing-edit" talon-ext-dir))
  :pretty-hydra
  ((:title "Thing Edit" :color teal :quit-key "q")
   ("Copy"
    (("w" thing-copy-word "copy word")
     ("p" thing-copy-parentheses "copy round"))
    "Cut"
    (("W" thing-cut-word "cut word")
     ("P" thing-cut-parentheses "cut round"))
   "Replace"
    (("r" thing-replace-parentheses "replace round"))))
  :bind ("C-." . thing-edit-hydra/body))


(provide 'init-edit)


;;; init-edit.el ends here
