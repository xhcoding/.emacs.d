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
                avy-style 'pre)
  :pretty-hydra
  ((:title (pretty-hydra-title "Go To" 'faicon "rocket") :quit-key "q")
   ("Base Move"
    (("n" next-line "next line")
     ("p" previous-line "previous line")
     ("f" forward-char "forward char")
     ("b" backward-char "backward char"))
    "Char Move"
    (("c" avy-goto-char-2)
     ("C" avy-goto-char-timer)
     ("l" avy-goto-char-in-line))
    "Other Move"
    (("L" avy-goto-line)
     ("e" flycheck-next-error)
     ("E" flycheck-previous-error))
    "Mark"
    (("m" set-mark-command))))
  :bind (("M-l" . avy-hydra/body)))

(use-package evil-nerd-commenter
  :bind ("C-/" . evilnc-comment-or-uncomment-lines))

(use-package auto-save
  :load-path (lambda() (expand-file-name "auto-save" talon-ext-dir))
  :init
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq auto-save-idle 3)
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
  (recentf-load-list)
  (remove-hook 'kill-emacs-hook 'recentf-save-list)
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
          rustic-mode
          groovy-mode
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
     ("p" thing-copy-parentheses "copy round")
     ("l" thing-copy-line "Copy line"))
    "Cut"
    (("W" thing-cut-word "cut word")
     ("P" thing-cut-parentheses "cut round")
     ("L" thing-cut-line "Cut line"))
    "Replace"
    (("rp" thing-replace-parentheses "replace round")
     ("rl" thing-replace-line))))
  :bind ("C-." . thing-edit-hydra/body))

(use-package undo-tree
  :hook (after-init . global-undo-tree-mode)
  :config
  (eval-after-load 'undo-tree
    '(progn
       (define-key undo-tree-map (kbd "C-/") nil)
       (define-key undo-tree-map (kbd "C-_") nil)
       (define-key undo-tree-map (kbd "C-?") nil)
       (define-key undo-tree-map (kbd "M-_") nil)
       (define-key undo-tree-map (kbd "C-z") 'undo-tree-undo)
       (define-key undo-tree-map (kbd "C-S-z") 'undo-tree-redo)))
  )


(use-package format-all
  :commands (format-all-buffer)
  :bind ("C-M-\\" . talon/format-buffer)
  :config
  (defun talon/format-buffer()
    (interactive)
    (if (memq major-mode '(c++-mode))
        (lsp-format-buffer)
      (format-all-buffer)))
  )

(use-package multiple-cursors
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("s-<mouse-1>"   . mc/add-cursor-on-click)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space)))

(use-package goto-chg
  :bind ("C-," . goto-last-change))

(use-package goto-last-point
  :diminish
  :bind ("C-M-," . goto-last-point)
  :hook (after-init . goto-last-point-mode))

(provide 'init-edit)


;;; init-edit.el ends here
