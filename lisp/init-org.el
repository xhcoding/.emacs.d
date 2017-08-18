
;; basic config
(setq org-src-fontify-natively t)


;; blog config

(require-package 'org-octopress)
(require-package 'ox-latex-chinese)
(require-package 'uimage)

(require 'org-octopress)
(setq org-octopress-directory-top "~/Documents/Blog/hexo/source")
(setq org-octopress-directory-posts "~/Documents/Blog/hexo/source/_posts")
(setq org-octopress-directory-org-top "~/Documents/Blog/hexo/")
(setq org-octopress-directory-org-posts "~/Documents/Blog/hexo/blog")




(defun org-custom-img-link-follow (path)
  (org-open-file-with-emacs path))

(defun org-custom-img-link-export (path desc backend)
  (cond
   ((eq 'html backend)
    (setq image-name (file-name-nondirectory path))
    (format "<img src=\"http://ivme.xhcoding.cn/%s\" alt=\"%s\" />" image-name desc)
    )))

;; [[img-hexo:image-path]]
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
  (setq savepath "/home/xhcoding/Documents/Blog/hexo/images/")
  (setq fullpath (concat savepath basename))
  ;; miximize the window by call xdotool trigger a system global shortcut
  (call-process-shell-command "xdotool key super+n")
  ;; if emacs is gui mode 
  ;; (suspend-frame)
  (sleep-for 1)
  ;; use scrot
  ;; (call-process-shell-command "scrot" nil nil nil nil "-s" (concat fullpath ".png"))
  ;; use deepin-screenshot
  (call-process-shell-command (concat "deepin-screenshot -i -s" (concat fullpath ".jpg")))
  ;;(call-process-shell-command "/home/xhcoding/Tools/qshell-v2.0.7/qshell qupload /home/xhcoding/Tools/qshell-v2.0.7/upload_config.json")
  (xhcoding/insert-org-or-md-img-link savepath (concat basename ".jpg"))
  (insert "\n"))




(provide 'init-org)
