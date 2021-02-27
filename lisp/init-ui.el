;;; init-ui.el --- my emacs configuration -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package all-the-icons
  :if (display-graphic-p))

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

(use-package mixed-pitch
  :diminish)


(if (member "等距更纱黑体 SC" (font-family-list))

    (if (> (x-display-pixel-height) 2000)
        (set-frame-font "-outline-等距更纱黑体 SC-normal-normal-normal-mono-34-*-*-*-c-*-iso8859-1" nil t)
      (set-frame-font "-outline-等距更纱黑体 SC-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1" nil t))
  (if (> (x-display-pixel-height) 2000)
      (set-frame-font "-outline-Sarasa Mono SC-normal-normal-normal-mono-34-*-*-*-c-*-iso8859-1" nil t)
    (set-frame-font "-outline-Sarasa Mono SC-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1" nil t)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package doom-themes
  :config
  (if talon-dumped-load-path
      (enable-theme 'doom-one-light)
    (load-theme 'doom-one-light t)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-unicode-fallback t)
  :init
  (unless after-init-time
    (setq doom-modeline--default-format mode-line-format)
    (setq-default mode-line-format nil)))

(provide 'init-ui)


;;; init-ui.el ends here
