;;; init-cc.el --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package cmake-mode
  :hook (cmake-mode . (lambda()
                        (set (make-local-variable 'company-backends)
                             '((company-cmake company-dabbrev-code))))))

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



(provide 'init-cc)

;;; init-cc.el ends here
