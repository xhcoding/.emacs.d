;;; init-rust.el --- rust  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package rust-mode
  :init (setq rust-format-on-save t)
  :config
  (use-package cargo
    :diminish cargo-minor-mode
    :hook (rust-mode . cargo-minor-mode)
    :config
    ;; To render buttons correctly, keep it at the last
    (setq compilation-filter-hook
          (append compilation-filter-hook '(cargo-process--add-errno-buttons)))))

(use-package rust-playground)

(provide 'init-rust)

;;; init-rust.el ends here
