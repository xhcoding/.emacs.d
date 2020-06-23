;;; init-functions.el --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:


(defun talon/rename-this-file-and-buffer (new-name)
  "Rename both current buffer and file it's visiting to NEW_NAME."
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

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))


(defun talon*inner-comment-p()
  "Check whether in comment."
  (nth 4 (syntax-ppss)))

;; (defun talon/format-buffer-or-region()
;;   (cond ((equal major-mode 'emacs-lisp-mod))))


(provide 'init-functions)

;;; init-functions.el ends here
