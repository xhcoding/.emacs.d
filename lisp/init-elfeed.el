;;; init-elfeed.el --- elfeed  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:


(use-package elfeed
  :bind (:map elfeed-search-mode-map
              ("RET" . eaf-elfeed-open-url))
  :config
  (setq eaf-elfeed-split-direction "right"))

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files `(,(expand-file-name "elfeed.org" talon-org-dir))))

(provide 'init-elfeed)

;;; init-elfeed.el ends here
