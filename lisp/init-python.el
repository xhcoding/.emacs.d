;;; init-python.el --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package python
  :ensure nil
  :init
  (setq python-indent-guess-indent-offset-verbose nil
        python-indent-guess-indent-offset nil
        python-indent-offset 4))


(provide 'init-python)

;;; init-python.el ends here
