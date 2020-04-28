;;; init-cc.el --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package cmake-mode
  :hook (cmake-mode . (lambda()
                        (set (make-local-variable 'company-backends)
                             '(company-cmake company-dabbrev-code)))))

(use-package qt-pro-mode
  :mode ("\\.pro\\'" "\\.pri\\'"))


(use-package cc-mode
  :ensure nil
  :mode ("\\.h\\'" . c++-mode)
  :config
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
          (t (newline-and-indent))))

  (add-hook 'c++-mode-hook 'talon-set-c-style)
  :bind (:map c++-mode-map
              ("RET" . talon-c-new-line))
  )


;; checkers
(flycheck-define-checker c/c++-cpplint
  "A C/C++ style checker using cpplint.
See URL
`https://github.com/cpplint/cpplint'"
  :command ("cpplint" source-original)
  :error-patterns
  ((warning line-start (file-name) ":" line ":  " (message) line-end))
  :modes (c-mode c++-mode))

(add-to-list 'flycheck-checkers 'c/c++-cpplint 'append)

(defun talon-append-checkers()
  "."
  (flycheck-add-next-checker 'lsp
                             '(warning . c/c++-cpplint)))

(add-hook 'lsp-after-initialize-hook 'talon-append-checkers)


(provide 'init-cc)

;;; init-cc.el ends here
