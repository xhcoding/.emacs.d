

;;; Code:

(use-package nasm-mode
  :mode "\\.nas\\'")

;; asm86
(autoload 'asm86-mode "asm86-mode.el")
(setq auto-mode-alist
      (append '(("\\.asm\\'" . asm86-mode) ("\\.inc\\'" . asm86-mode))
	      auto-mode-alist))



(defun compile-masm-with-dosbox()
  "Compile masm code with dosbox."
  (interactive)
  (setq filepath (buffer-file-name))
  (setq filename (file-name-nondirectory filepath))
  (setq objfilename (concat (file-name-base filepath) ".obj"))
  (setq dosbox-config-file "~/.dosbox/dosbox-0.74.conf")
  (replace-line-regexp dosbox-config-file "^\\(mount c \\)\\(.*\\)" (concat "\\1" (file-name-directory filepath)))
  (replace-line-regexp dosbox-config-file "^\\(masm.exe \\)\\(.*\\)" (concat "\\1" filename ";"))
  (replace-line-regexp dosbox-config-file "^\\(link.exe \\)\\(.*\\)" (concat "\\1" objfilename ";"))
  (if (process-status "dosbox")
      (kill-process "dosbox"))
  (async-start-process "dosbox" "dosbox" nil)
  )

 
(provide 'init-asm)
;;; init-asm.el ends here
