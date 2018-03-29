;;; Package --- init-org.el
;;; Commentary:
;;; org config


;;; Code:

(use-package markdown-mode)
(require 'org)
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)

;; plantuml
;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))

(setq org-plantuml-jar-path
      (expand-file-name (concat user-emacs-directory "plantuml/plantuml.jar")))

(defun xhcoding/yank-link()
  (interactive)
  (insert "[[")
  (yank)
  (insert "]]"))

(setq org-src-fontify-natively t)
(setq org-directory "~/Documents/Life")
(setq org-default-notes-file "~/Documents/Org/inbox.org")
(setq-default org-agenda-files '("~/Documents/Org/notes.org"))
(setq org-todo-keywords
      '((sequence "TODO(t)" "ROUTINE(r)" "STARTED(s)" "NEXT(n)" "|" "DONE(d!/!)" "CANCELLED")))


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
    (require 'ox-jekyll)
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

;; test
(setq org-publish-project-alist
      '(("test-publish"
	 :base-directory "~/Documents/Blog/blog"
	 :publishing-directory "~/Documents/Blog/source/_posts"
	 :base-extension "org"
	 :recursive t
	 :htmlized-source t
	 :publishing-function org-html-publish-to-html
	 :with-toc nil
	 )))

(use-package blog-admin
:init
(progn
  ;; your config
  (setq blog-admin-backend-type 'hexo)
  (setq blog-admin-backend-path "~/Documents/Blog")
  (setq blog-admin-backend-new-post-in-drafts t)
  (setq blog-admin-backend-new-post-with-same-name-dir t)
  ))

(provide 'init-org)

