;;; init-functions.el --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:


(defun talon-rename-this-file-and-buffer (new-name)
  "Rename both current buffer and file it's visiting to NEW_NAME"
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(provide 'init-functions)

;;; init-functions.el ends here
