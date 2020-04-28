;;; init-accelerate.el --- emacs configuration  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

;; 增加启动速度的措施，毕竟大家都加了。
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda()
            "恢复默认值"
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 16777216)
            (setq gc-cons-percentage 0.1)))

(defvar talon-init-time 'nil)
(defun talon-display-benchmark()
  "计算启动时间."
  (message "Talon loaded %s packages in %.03fs"
           (length package-activated-list)
           (or talon-init-time
               (setq talon-init-time
                     (float-time (time-subtract (current-time) before-init-time))))))
(add-hook 'emacs-startup-hook #'talon-display-benchmark)


(provide 'init-accelerate)

;;; init-accelerate.el ends here
