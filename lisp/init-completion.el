;; 需要的包
(require-package 'company)
(require-package 'ycmd)
(require-package 'company-ycmd)
(require-package 'flycheck-ycmd)



;;company配置
(add-hook 'after-init-hook 'global-company-mode)

(after-load 'company
  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (delete 'company-semantic company-backends)
  (setq company-idle-delay 0.2
	company-show-numbers t))

;; company-backends
;;设置默认的backends
(setq company-backends
      '((company-files
	 company-keywords
	 company-capf
	 company-yasnippet
	 )
	(company-abbrev company-dabbrev)
	))

;;c/c++ backends
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (add-to-list (make-local-variable 'company-backends)
			 '(company-ycmd company-clang))))




;; ycmd
(require 'ycmd)

(add-hook 'c-mode-common-hook 'ycmd-mode)
(after-load 'ycmd
  (set-variable 'ycmd-server-command '("python" "/home/xhcoding/Tools/ycmd/ycmd/"))
  (set-variable 'ycmd-global-config "/home/xhcoding/Tools/ycmd/cpp/ycm/.ycm_extra_conf.py")
  ;;(company-ycmd-setup)
  (flycheck-ycmd-setup)
  )



;;abbrev
(add-hook 'after-init-hook 'abbrev-mode)

;;hippie

(setq hippie-expand-try-functions-list '(
					 try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-all-abbrevs
					 try-expand-list
					 try-expand-line
					 try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol))

(global-set-key (kbd "C-c /") 'hippie-expand)

;; cedet
;;(add-hook 'c-mode-common-hook 'semantic-mode)


;;头文件补全
(defun semantic-gcc-query (gcc-cmd &rest gcc-options)
  "Return program output or error code in case error happens.
GCC-CMD is the program to execute and GCC-OPTIONS are the options
to give to the program."
  ;; $ gcc -v
  ;;
  (let* ((buff (get-buffer-create " *gcc-query*"))
         (old-lc-messages (getenv "LC_ALL"))
         (options `(,nil ,(cons buff t) ,nil ,@gcc-options))
         (err 0))
    (with-current-buffer buff
      (erase-buffer)
      (setenv "LC_ALL" "C")
      (condition-case nil
          (setq err (apply 'call-process gcc-cmd options))
        (error ;; Some bogus directory for the first time perhaps?
         (let ((default-directory (expand-file-name "~/")))
           (condition-case nil
               (setq err (apply 'call-process gcc-cmd options))
             (error ;; gcc doesn't exist???
              nil)))))
      (setenv "LC_ALL" old-lc-messages)
      (prog1
          (if (zerop err)
              (buffer-string)
            err)
        (kill-buffer buff)))))

(defun semantic-gcc-get-include-paths (lang)
  "Return include paths as gcc uses them for language LANG."
  (let* ((gcc-cmd (cond
                   ((string= lang "c") "gcc")
                   ((string= lang "c++") "c++")
                   (t (if (stringp lang)
                          (error "Unknown lang: %s" lang)
                        (error "LANG=%S, should be a string" lang)))))
         (gcc-output (semantic-gcc-query gcc-cmd "-v" "-E" "-x" lang null-device))
         (lines (split-string gcc-output "\n"))
         (include-marks 0)
         (inc-mark "#include ")
         (inc-mark-len (length "#include "))
         inc-path)
    ;;(message "gcc-output=%s" gcc-output)
    (dolist (line lines)
      (when (> (length line) 1)
        (if (= 0 include-marks)
            (when (and (> (length line) inc-mark-len)
                       (string= inc-mark (substring line 0 inc-mark-len)))
              (setq include-marks (1+ include-marks)))
          (let ((chars (append line nil)))
            (when (= 32 (nth 0 chars))
              (let ((path (substring line 1)))
                (when (file-accessible-directory-p path)
                  (when (if (memq system-type '(windows-nt))
                            (/= ?/ (nth 1 chars))
                          (= ?/ (nth 1 chars)))
                    (add-to-list 'inc-path
                                 (expand-file-name (substring line 1))
                                 t)))))))))
    inc-path))


;; 输入 inc , 可以自动提示输入文件名称,可以自动补全.
;; Provided by yangyingchao@gmail.com
(mapc
 (lambda (mode)
   (define-abbrev-table mode '(
                               ("inc" "" skeleton-include 1)
                               )))
 '(c-mode-abbrev-table c++-mode-abbrev-table))

(defconst yc/inc-dir-list
  (append (semantic-gcc-get-include-paths "c++") '("./")) "nil")

(defvar inc-minibuffer-compl-list nil "nil")

(defun yc/update-minibuffer-complete-table ( )
  "Complete minibuffer."
  (interactive)
  (let ((prompt (minibuffer-prompt))
        (comp-part (minibuffer-contents-no-properties)))
    (when (and (string= "Include File:" prompt)
               (> (length comp-part) 0))
      (setq minibuffer-completion-table
            (append minibuffer-completion-table
                    (let ((inc-files nil)
                          (dirname nil)
                          (tmp-name nil))
                      (mapc
                       (lambda (d)
                         (setq dirname (format "%s/%s" d comp-part))
                         (when (file-exists-p dirname)
                           (mapc
                            (lambda (x)
                              (when (not (or (string= "." x)
                                             (string= ".." x)))
                                (setq tmp-name (format "%s/%s" comp-part x))
                                (add-to-list 'inc-files tmp-name)))
                            (directory-files dirname))))
                       yc/inc-dir-list)
                      inc-files)))))
  (insert "/"))

(let ((map minibuffer-local-completion-map))
  (define-key map "/" 'yc/update-minibuffer-complete-table))

(defun yc/update-inc-marks ( )
  "Description."
  (let ((statement (buffer-substring-no-properties
		    (point-at-bol) (point-at-eol)))
	(inc-file nil)
	(to-begin nil)
	(to-end nil)
	(yc/re-include
	 (rx "#include" (+ ascii) "|XXX|" (group (+ ascii)) "|XXX|")))
    (when (string-match yc/re-include statement)
      (setq inc-file (match-string 1 statement))
      (if (file-exists-p (format "./%s" inc-file))
	  (setq to-begin "\"" to-end "\"")
	(setq to-begin "<" to-end ">")
	)
      (move-beginning-of-line 1)
      (kill-line)
      (insert (format "#include %s%s%s" to-begin inc-file to-end))
      (move-end-of-line 1))))

(define-skeleton skeleton-include
  "generate include<>" ""
  > "#include |XXX|"
  (completing-read
   "Include File:"
   (mapcar
    (lambda (f) (list f ))
    (apply
     'append
     (mapcar
      (lambda (dir)
        (directory-files
         dir nil
         "\\(\\.h\\)?"))
      yc/inc-dir-list))))
  "|XXX|"
  (yc/update-inc-marks))


(provide 'init-completion)
