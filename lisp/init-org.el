;;; Package --- init-org.el
;;; Commentary:
;;; org config


;;; Code:

(use-package markdown-mode)
(require 'org)
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)


(defun xhcoding/yank-link()
  (interactive)
  (insert "[[")
  (yank)
  (insert "]]"))

(setq org-src-fontify-natively t)
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "NEXT(n)" "|" "DONE(d!/!)" "CANCELLED")))


(setq org-log-done t)
(setq org-log-into-drawer "LOGBOOK")



;;=================== hexo blog====================

(defvar my-blog-top-dir
  "~/Blog/"
  "Blog root directory.")


(use-package orglue
  :defer t)

;; 临时修复BUG
(defun org-bug()
  (interactive)
  (load-file (expand-file-name
	      "elpa/org-octopress-20170820.2115/ox-jekyll.el"
	      user-emacs-directory)))


(use-package org-octopress
  :init
  (progn

    (require 'org-octopress)
    (setq
     org-octopress-directory-top (expand-file-name "source" my-blog-top-dir)
     org-octopress-directory-posts (expand-file-name "source/_posts" my-blog-top-dir)
     org-octopress-directory-org-top my-blog-top-dir
     org-octopress-directory-org-posts (expand-file-name "blog" my-blog-top-dir)
     org-octopress-setup-file (expand-file-name "setupfile.org" my-blog-top-dir)
     )))



;; sync images
(defun sync-blog-img-to-qiniu()
  (interactive)
  (call-process-shell-command "/home/xhcoding/Tools/qshell-v2.0.7/qshell qupload /home/xhcoding/Tools/qshell-v2.0.7/upload_config.json"))


(defun org-custom-img-link-follow (path)
  (org-open-file-with-emacs path))

(defun org-custom-img-link-export (path desc backend)
  (cond
   ((eq 'html backend)
    (setq image-name (file-name-nondirectory path))
    (format "<img src=\"http://ivme.xhcoding.cn/%s\" alt=\"%s\" />" image-name desc)
    )))

;; (defun org-custom-img-link-export-md (path desc backend)
;;   (cond
;;    ((eq 'md backend)
;;     (setq image-name (file-name-nondirectory path))
;;     (format "![%s](http://ivme.xhcoding.cn/%s)" desc image-name)
;;     )))



;; ;; [[img-hexo:image-pathG]]
(org-link-set-parameters
 "img-hexo"
 :follow 'org-custom-img-link-follow  ;;  click link
 :export 'org-custom-img-link-export  ;;  export html
 :help-echo "Click me to display image")




(defun xhcoding/insert-org-or-md-img-link (prefix imagename)
  (if (equal (file-name-extension (buffer-file-name)) "org")
      (insert (format "[[img-hexo:%s%s]]" prefix imagename))
    (insert (format "![%s](%s%s)" imagename prefix imagename))))



(defun xhcoding/capture-screenshot (basename)
  "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer/markdown-buffer and insert a link to this file."
  (interactive "sScreenshot name: ")
  (if (equal basename "")
      (setq basename (concat (file-name-base buffer-file-name) (format-time-string "_%H%M%S"))))
  (setq savepath (expand-file-name "images/" my-blog-top-dir))
  (setq fullpath (concat savepath basename))
  ;; miximize the window by call xdotool trigger a system global shortcut
  (call-process-shell-command "xdotool key super+n")
  ;; if emacs is gui mode 
  ;; (suspend-frame)
  (sleep-for 1)
  ;; use scrot
  ;; (call-process-shell-command "scrot" nil nil nil nil "-s" (concat fullpath ".png"))
  ;; use deepin-screenshot
  (call-process-shell-command
   (concat
    "flatpak run --branch=master --arch=x86_64 --command=deepin-screenshot com.deepin.Screenshot -i -s " (concat fullpath ".png")))
  (sync-blog-img-to-qiniu)
  (xhcoding/insert-org-or-md-img-link savepath (concat basename ".jpg"))
  (insert "\n"))


;;===============================================


(provide 'init-org)

