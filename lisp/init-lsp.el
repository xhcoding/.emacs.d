;;; init-lsp.el --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package lsp-mode
  :diminish
  :hook (
         (lsp-mode . lsp-flycheck-enable)
         )
  :init
  (setq read-process-output-max (* 1024 1024))

  (setq lsp-keymap-prefix "C-c l"
        lsp-auto-guess-root t
        lsp-flycheck-live-reporting nil
        lsp-keep-workspace-alive nil
        lsp-prefer-capf t
        lsp-signature-auto-activate nil
        lsp-auto-configure nil

        lsp-enable-snippet nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        lsp-enable-symbol-highlighting nil)

  :config
  (require 'lsp-clients)
  (require 'lsp-pwsh)
  (require 'lsp-rust)
  (setq lsp-clients-clangd-args '("--compile-commands-dir=build" "--header-insertion=never" "--completion-style=detailed"))
  )

(use-package ccls
  :disabled t
  :hook ((c-mode c++-mode) . (lambda()
                               (require 'ccls)
                               (lsp)))
  :config
  (setq ccls-initialization-options
        `(:cache (:directory ,(expand-file-name "ccls_cache" talon-code-dir))
                 :compilationDatabaseDirectory "build"
                 :index (:threads 2))))

(use-package lsp-python-ms
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))

(use-package lsp-ui
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :pretty-hydra
  ((:title (pretty-hydra-title "LSP UI" 'faicon "rocket")
           :color amaranth :quit-key "q")
   ("Doc"
    (("d e" (lsp-ui-doc-enable (not lsp-ui-doc-mode))
      "enable" :toggle lsp-ui-doc-mode)
     ("d s" (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature))
      "signature" :toggle lsp-ui-doc-include-signature)
     ("d t" (setq lsp-ui-doc-position 'top)
      "top" :toggle (eq lsp-ui-doc-position 'top))
     ("d b" (setq lsp-ui-doc-position 'bottom)
      "bottom" :toggle (eq lsp-ui-doc-position 'bottom))
     ("d p" (setq lsp-ui-doc-position 'at-point)
      "at point" :toggle (eq lsp-ui-doc-position 'at-point))
     ("d f" (setq lsp-ui-doc-alignment 'frame)
      "align frame" :toggle (eq lsp-ui-doc-alignment 'frame))
     ("d w" (setq lsp-ui-doc-alignment 'window)
      "align window" :toggle (eq lsp-ui-doc-alignment 'window)))
    "Sideline"
    (("s e" (lsp-ui-sideline-enable (not lsp-ui-sideline-mode))
      "enable" :toggle lsp-ui-sideline-mode)
     ("s h" (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover))
      "hover" :toggle lsp-ui-sideline-show-hover)
     ("s d" (setq lsp-ui-sideline-show-diagnostics (not lsp-ui-sideline-show-diagnostics))
      "diagnostics" :toggle lsp-ui-sideline-show-diagnostics)
     ("s s" (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol))
      "symbol" :toggle lsp-ui-sideline-show-symbol)
     ("s c" (setq lsp-ui-sideline-show-code-actions (not lsp-ui-sideline-show-code-actions))
      "code actions" :toggle lsp-ui-sideline-show-code-actions)
     ("s i" (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate))
      "ignore duplicate" :toggle lsp-ui-sideline-ignore-duplicate))
    "Action"
    (("h" backward-char "←")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("l" forward-char "→")
     ("C-a" mwim-beginning-of-code-or-line nil)
     ("C-e" mwim-end-of-code-or-line nil)
     ("C-b" backward-char nil)
     ("C-n" next-line nil)
     ("C-p" previous-line nil)
     ("C-f" forward-char nil)
     ("M-b" backward-word nil)
     ("M-f" forward-word nil)
     ("c" lsp-ui-sideline-apply-code-actions "apply code actions"))))
  :bind (("C-c u" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ("M-<f6>" . lsp-ui-hydra/body)
         ("M-RET" . lsp-ui-sideline-apply-code-actions)
	     ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	     ([remap xref-find-references]  . lsp-ui-peek-find-references)
         )
  :hook (lsp-mode . lsp-ui-mode)
  :init (setq lsp-ui-doc-enable nil
              lsp-ui-doc-use-webkit nil
              lsp-ui-doc-delay 0.2
              lsp-ui-doc-include-signature t
              lsp-ui-doc-position 'at-point
              lsp-ui-doc-border (face-foreground 'default)
              lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer

              lsp-ui-sideline-enable t
              lsp-ui-sideline-show-hover nil
              lsp-ui-sideline-show-diagnostics nil
              lsp-ui-sideline-show-code-actions t
              lsp-ui-sideline-ignore-duplicate t

              lsp-ui-imenu-enable t
              lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                    ,(face-foreground 'font-lock-string-face)
                                    ,(face-foreground 'font-lock-constant-face)
                                    ,(face-foreground 'font-lock-variable-name-face)))
  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              (setq lsp-ui-doc-border (face-foreground 'default))
              (set-face-background 'lsp-ui-doc-background
                                   (face-background 'tooltip)))))

(provide 'init-lsp)

;;; init-lsp.el ends here
