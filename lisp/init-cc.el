;; 需要的包

;; .h作为cpp文件
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; 代码风格
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(setq c-default-style "google")
(setq c-basic-offset 4)


;; 编译简单的c++文件

(defun set-compile-command(command)
  (set (make-local-variable 'compile-command) command))

(defun my-compile-command()
  (cond
   ((file-exists-p "Makefile")
    (setq command "make"))
   ((file-exists-p "CMakeLists.txt")
    (setq command "cmake -G \"Unix Makefiles\" ./"))
   (t
    (setq command (let ((file (file-name-nondirectory buffer-file-name)))
		    (format "%s -o %s.out %s %s"
			    "g++"
			    (file-name-base file)
			    "-g"
			    file)))))
  (set-compile-command command))

(defun compile-not-ask()
  (interactive)
  (compile compile-command))

(add-hook 'c-mode-common-hook 'my-compile-command)
(require 'cc-mode)

;; 运行
(defun run-buffer-file()
  (interactive)
  (setq run-result-buffer "Result")
  (setq file-directory (file-name-directory buffer-file-name))
  (setq file-name (concat (file-name-base buffer-file-name) ".out"))
  (setq file-absolute-path (concat file-directory file-name))
  (when (file-exists-p file-absolute-path)
    (if (get-buffer run-result-buffer)
	(kill-buffer run-result-buffer))
    (call-process-shell-command file-absolute-path nil (get-buffer-create run-result-buffer) nil)
    (with-current-buffer run-result-buffer (help-mode))
    (display-buffer run-result-buffer)))

(defun compile-and-run()
  (interactive)
  (progn
    (compile compile-command)
    (run-buffer-file)))

(define-key c++-mode-map (kbd "<f7>") 'compile-not-ask)
(define-key c++-mode-map (kbd "<f8>") 'run-buffer-file)
(define-key c-mode-map (kbd "<f8>") 'run-buffer-file)
(define-key c++-mode-map (kbd "<f9>") 'compile-and-run)

(provide 'init-cc)
