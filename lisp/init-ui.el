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


(use-package awesome-tray
  :load-path (lambda() (expand-file-name "awesome-tray" talon-ext-dir))
  :config
  (defun talon-module-flycheck-info ()
    (when (boundp 'flycheck-last-status-change)
      (pcase flycheck-last-status-change
        (`finished
         (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                (errors (cdr (assq 'error error-counts)))
                (warnings (cdr (assq 'warning error-counts))))
           (concat "["
                   (cond
                    (errors (format "❄:%s" errors))
                    (warnings (format "☁:%s" warnings))
                    (t "☀"))
                   "]"))))))

  (defface talon-module-flycheck-face
    '((((background light))
       :foreground "#cc2444" :bold t)
      (t
       :foreground "#ff2d55" :bold t))
    "Flycheck state face."
    :group 'awesome-tray)

  (add-to-list 'awesome-tray-module-alist
               '("flycheck" . (talon-module-flycheck-info talon-module-flycheck-face)))

  (defun talon-module-rime-info ()
    (rime-lighter))

  (defface talon-module-rime-face
    '((((background light))
       :foreground "#9256B4" :bold t)
      (t
       :foreground "#9256B4" :bold t))
    "Rime module face."
    :group 'awesome-tray)

  (add-to-list 'awesome-tray-module-alist
               '("rime" . (talon-module-rime-info talon-module-rime-face)))

  (defun talon-module-encoding-info ()
    (with-current-buffer (buffer-name)
      (let ((eof (coding-system-eol-type buffer-file-coding-system))
            (sys (coding-system-plist buffer-file-coding-system))
            (info ""))
        (setq info (concat info
                           (pcase eof
                             (0 "LF ")
                             (1 "CRCF ")
                             (2 "CR "))
                           (cond ((memq (plist-get sys :category)
                                        '(coding-category-undecided coding-category-utf-8))
                                  "UTF-8")
                                 (t (upcase (symbol-name (plist-get sys :name)))))))
        info)))

  (defface talon-module-encoding-face
    '((((background light))
       :foreground "#1b2bdd" :bold t)
      (t
       :foreground "#1b2bdd" :bold t))
    "Encoding module face."
    :group 'awesome-tray)

  (add-to-list 'awesome-tray-module-alist
               '("encoding" . (talon-module-encoding-info talon-module-encoding-face)))



  (setq awesome-tray-active-modules '("flycheck" "rime" "location" "mode-name" "git" "encoding" "date"))

  (awesome-tray-mode +1))

(use-package lazycat-theme
  :load-path (lambda() (expand-file-name "lazycat-theme" talon-ext-dir))
  :commands (lazycat-theme-toggle)
  :config
  (load-theme 'lazycat-light t))


(set-frame-font "-outline-等距更纱黑体 SC-normal-normal-normal-mono-30-*-*-*-c-*-iso8859-1")

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package awesome-tab
  :load-path (lambda() (expand-file-name "awesome-tab" talon-ext-dir))
  :config
  (setq awesome-tab-height 100)
  (awesome-tab-mode +1)
  :bind ("C-t" . awesome-tab-ace-jump))


(provide 'init-ui)


;;; init-ui.el ends here
