;;; init-hydra.el --- my emacs configuration -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(use-package pretty-hydra
  :bind ("<f6>" . toggle-hydra/body)
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (and (display-graphic-p) (require 'all-the-icons nil t)) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face))))

  (with-no-warnings
    (pretty-hydra-define toggle-hydra (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on")
                                              :color amaranth :quit-key "q")
      ("Basic"
       (("n" display-line-numbers-mode "line number" :toggle t)))
      )))


(use-package which-key
  :config
  (which-key-mode +1))


(provide 'init-hydra)
;;; init-hydra.el ends here
