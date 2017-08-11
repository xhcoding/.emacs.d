;; 使用spc键作为leader键

;; 全局操作
(evil-leader/set-key
  "c r" 'recentf-open-files
  "x s" 'save-buffer
  "x c" 'save-buffers-kill-terminal
  "x h" 'mark-whole-buffer
  "x d" 'dired
  "x k" 'kill-buffer
  "x 3" 'split-window-right
  "x 2" 'split-window-below
  "s" 'swiper
  "0" 'delete-window
  "1" 'select-window-1
  "2" 'select-window-2
  "3" 'select-window-3
  "4" 'select-window-4
  "5" 'select-window-5
  )

;; 帮助相关
(evil-leader/set-key
  "h t" 'help-with-tutorial
  "h f" 'describe-function
  "h v" 'describe-variable
  "h k" 'describe-key
  "h m" 'describe-mode)

;; 文件相关
(evil-leader/set-key
  "f o" 'counsel-find-file
  "f r" 'recentf-open-files
  "f n" 'rename-this-file-and-buffer
  "f d" 'delete-this-file
  "f s" 'save-buffer
  )

;; ggtags
(evil-leader/set-key
  "g d" 'ggtags-find-definition
  "g r" 'ggtags-find-reference
  "g f" 'ggtags-find-file
  "g t" 'ggtags-find-tag-dwim
  "g c" 'ggtags-create-tags
  "g u" 'ggtags-update-tags
  "g h" 'ggtags-view-search-history)

;; ycmd
(evil-leader/set-key-for-mode 'c++-mode "j c" 'ycmd-goto-declaration)
(evil-leader/set-key-for-mode 'c++-mode "j f" 'ycmd-goto-definition)
(evil-leader/set-key-for-mode 'c++-mode "j r" 'ycmd-goto-references)
(evil-leader/set-key-for-mode 'c++-mode "j i" 'ycmd-goto-include)
(evil-leader/set-key-for-mode 'c-mode "j c" 'ycmd-goto-declaration)
(evil-leader/set-key-for-mode 'c-mode "j f" 'ycmd-goto-definition)
(evil-leader/set-key-for-mode 'c-mode "j r" 'ycmd-goto-references)
(evil-leader/set-key-for-mode 'c-mode "j i" 'ycmd-goto-include)


(provide 'init-spc-keybindings)
