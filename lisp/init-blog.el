;;; init-blog.el --- emacs  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:


(defconst +my-blog-root-dir
  "~/Blog/"
  "Blog root directory.")

(defconst +my-blog-img-dir
  (expand-file-name "images/" +my-blog-root-dir)
  "Blog's image directory.")

(defconst +my-blog-res-url
  "http://source.xhcoding.cn/"
  "Blog's source address.")

(use-package ox-hugo)

(defun +my-blog*easy-hugo--org-headers (file)
  "Return a draft org mode header string for a new article as FILE."
  (let ((date
         (format-time-string "%Y-%m-%d")))
    (concat
     "#+HUGO_BASE_DIR: ../"
     "\n#+HUGO_SECTION: post"
     "\n#+TITLE: " file
     "\n#+DATE: " date
     "\n#+AUTHOR:"
     "\n#+HUGO_CUSTOM_FRONT_MATTER: :author \"xhcoding\""
     "\n#+HUGO_TAGS: "
     "\n#+HUGO_CATEGORIES: "
     "\n#+HUGO_DRAFT: false"
     "\n\n")))

(use-package easy-hugo
  :commands (easy-hugo)
  :config
  (setq easy-hugo-basedir (expand-file-name +my-blog-root-dir)
        easy-hugo-postdir "blog"
        easy-hugo-org-header t)
  (advice-add #'easy-hugo--org-headers :override #'+my-blog*easy-hugo--org-headers)
  )


(use-package org-download
  :config
  (when IS-WINDOWS
    (setq org-download-screenshot-method "irfanview /capture=4 /convert=\"%s\""
          org-download-display-inline-images 'posframe
          org-download-abbreviate-filename-function 'expand-file-name))
  (defun talon/org-download-clipboard()
    (interactive)
    (let ((org-download-screenshot-method "irfanview /clippaste /convert=\"%s\""))
      (org-download-screenshot)))

  )

(defun +my-blog/publish()
  "Publish my blog."
  (interactive)
  (let ((default-directory +my-blog-root-dir))
    (call-process-shell-command "hugo")
    (call-process-shell-command
     (concat "D:/Applications/Scoop/apps/cwrsync/current/bin/rsync.exe"
             " -e /cygdrive/d/Applications/Scoop/apps/cwrsync/current/bin/ssh.exe"
             " -avz --delete"
             " /cygdrive/c/Users/xhcoding/Blog/public/"
             " xhcoding@xhcoding.cn:~/public/"))
    (message "publish finished")))

(defun +my-blog/export-all()
  "Export all org to md."
  (interactive)
  (let ((default-directory (expand-file-name "blog" +my-blog-root-dir))
        (files (directory-files (expand-file-name
                                 "blog" +my-blog-root-dir))))
    (seq-each
     (lambda(file)
       (when (and
              (not (string-equal "." file))
              (not (string-equal ".." file))
              (not (string-equal "config.toml" file))
              (not (string-equal ".dir-locals.el" file)))
         (with-temp-buffer
           (find-file file)
           (org-hugo-export-to-md))))
     files)))

;; ~/Documents/Blog/images/1.png ==> http://source.xhcoding.cn/1.png
(defun +my-blog*export-blog-image-url(args)
  (let* ((link (nth 0 args))
         (desc (nth 1 args))
         (info (nth 2 args))
         (old-raw-path (org-element-property :path link))
         (file-path (expand-file-name old-raw-path))
         (type (org-element-property :type link)))
    (if (and (string= type "file")
             (equal
              (string-match-p
               (regexp-quote (expand-file-name +my-blog-root-dir))
               file-path)
              0))
        (progn
          (let* ((image-url (concat  +my-blog-res-url (string-trim-left file-path (expand-file-name +my-blog-root-dir))))
                 (new-link (org-element-put-property link :path image-url )))
            `(,new-link ,desc ,info)))
      `(,link ,desc ,info))))

(advice-add #'org-hugo-link :filter-args #'+my-blog*export-blog-image-url)

(provide 'init-blog)

;;; init-blog.el ends here
