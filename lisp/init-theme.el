;; 需要的主题包
(require-package 'zenburn-theme)
(maybe-require-package 'monokai-theme)
(maybe-require-package 'color-theme-sanityinc-solarized)
(maybe-require-package 'color-theme-sanityinc-tomorrow)

(setq-default custom-enabled-themes '(sanityinc-solarized-dark))
	      
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote, custom-enabled-themes))))


(add-hook 'after-init-hook 'reapply-themes)


(provide 'init-theme)
