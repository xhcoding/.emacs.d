
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



(defun xhcoding/insert-org-or-md-img-link (prefix imagename)
  (if (equal (file-name-extension (buffer-file-name)) "org")
      (insert (format "[[%s%s]]" prefix imagename))
    (insert (format "![%s](%s%s)" imagename prefix imagename))))



(defun xhcoding/capture-screenshot (basename)
  "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer/markdown-buffer and insert a link to this file."
  (interactive "sScreenshot name: ")
  (if (equal basename "")
      (setq basename (concat (file-name-base buffer-file-name) (format-time-string "_%H%M%S"))))
  (setq fullpath (concat "/home/xhcoding/Documents/Blog/hexo/images/" basename))
  (suspend-frame)
  (call-process-shell-command "scrot" nil nil nil nil "-s" (concat fullpath ".png"))
  (call-process-shell-command "/home/xhcoding/Tools/qshell-v2.0.7/qshell qupload /home/xhcoding/Tools/qshell-v2.0.7/upload_config.json")
  (xhcoding/insert-org-or-md-img-link "http://ivme.xhcoding.cn/" (concat basename ".png"))
  (insert "\n"))


(provide 'init-org)
