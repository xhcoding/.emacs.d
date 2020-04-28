;;; init-completion.el --- emacs config  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:


(use-package company
  :hook (after-init . global-company-mode)
  :commands company-cancel
  :init
  (setq company-idle-delay 0.2
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-show-numbers t
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes
        '(not comint-mode erc-mode message-mode help-mode gud-mode)
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-backends '(company-capf))
  )

(use-package company-posframe
  :hook (company-mode . company-posframe-mode))



(provide 'init-completion)

;;; init-completion.el ends here
