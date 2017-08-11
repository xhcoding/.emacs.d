(require-package 'helm)
(helm-mode t)


(after-load 'helm
  (helm-autoresize-mode t)
  (setq-default helm-autoresize-max-height 30);;调整高度
  )



;; 全局键绑定
(global-set-key (kbd "M-x")                          'undefined)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "M-y")                          'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
(global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
(global-set-key (kbd "C-h r")                        'helm-info-emacs)
(global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")                          'helm-calcul-expression)
(global-set-key (kbd "C-h d")                        'helm-info-at-point)
(global-set-key (kbd "C-h i")                        'helm-info)
(global-set-key (kbd "C-x C-d")                      'helm-browse-project)
(global-set-key (kbd "<f1>")                         'helm-resume)
(global-set-key (kbd "C-h C-f")                      'helm-apropos)
(global-set-key (kbd "C-h a")                        'helm-apropos)
(global-set-key (kbd "<f5> s")                       'helm-find)
(global-set-key (kbd "S-<f2>")                       'helm-execute-kmacro)
(global-set-key (kbd "C-c i")                        'helm-imenu-in-all-buffers)
(global-set-key (kbd "C-s")                          'helm-occur)
(provide 'init-helm)
