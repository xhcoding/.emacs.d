;; 定义一个函数打开init.el文件
(defun open-user-init-file()
  "Open user's init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))
;; 使用f2绑定open-user-init-file文件
(global-set-key (kbd "<f2>") 'open-user-init-file)

;; 在feature载入后执行body
(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))


;; 删除当前文件且关闭当前buffer
(defun delete-this-file()
  "Delete the current file, and kill the buffer"
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (y-or-n-p (format "Really delete '%s'?" (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))
;; 
(global-set-key (kbd "C-c f d") 'delete-this-file)

;;重新命名当前文件和buffer
(defun rename-this-file-and-buffer (new-name)
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
;;
(global-set-key (kbd "C-c f r") 'rename-this-file-and-buffer)

;;用默认浏览器打开当前文件
(defun browse-current-file()
  "Open the current file as a URL using `browse-rul'"
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
	     (tramp-tramp-file-p file-name))
	(error "Cannot open tramp")
      (browse-url (concat "file://" file-name)))))
;;
(global-set-key (kbd "C-c f b") 'browse-current-file)

;; 使用scrot截屏
(defun org-screenshot ()
  "Take a screenshot into a unique-named file in the current buffer file
  directory and insert a link to this file."
  (interactive)
  (setq filename
	(concat (make-temp-name
		 (concat (file-name-base buffer-file-name) "_" (format-time-string "%Y-%m-%d_" (current-time))))
		 ".png"))
  (setq dirname "/home/xhcoding/Documents/Blog/images/")
  (if (file-exists-p dirname)
      ()
    (make-directory dirname))
  (suspend-frame)
  (call-process-shell-command "scrot" nil nil nil nil " -s " (concat
							    "\"" dirname filename "\"" ))
  (insert (concat "[[" dirname filename "]]"))
  (org-display-inline-images))

(global-set-key (kbd "C-x p") 'org-screenshot)


(provide 'init-utils)
