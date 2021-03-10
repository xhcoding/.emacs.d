;;; init-cc.el --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package cmake-mode
  :hook (cmake-mode . lsp))

(use-package qt-pro-mode
  :mode ("\\.pro\\'" "\\.pri\\'"))


(use-package cc-mode
  :ensure nil
  :mode ("\\.h\\'" . c++-mode)
  :hook (c++-mode . try-start-lsp)
  :config

  (defun try-start-lsp()
    (interactive)
    (when (file-exists-p (expand-file-name "build/compile_commands.json" (projectile-project-root)))
      (lsp)))

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
          ((talon*inner-comment-p) (c-indent-new-comment-line))
          (t (newline-and-indent))))

  (add-hook 'c++-mode-hook 'talon-set-c-style)
  :bind (:map c++-mode-map
              ("RET" . talon-c-new-line))
  )


(use-package tree-sitter
  :hook ((after-init . global-tree-sitter-mode)
         (tree-sitter-after-on . tree-sitter-hl-mode))
  )

(use-package tree-sitter-langs)

(provide 'init-cc)

;;; init-cc.el ends here
