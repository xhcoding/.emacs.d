

(global-set-key (kbd "C-c i") 'open-user-init-file)
(global-set-key (kbd "M-s o") 'occur-dwim)
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

;; use <SPC> as evil leader

;; global
(evil-leader/set-key
  "c r" 'recentf-open-files
  "c c" 'hs-toggle-hiding
  "x s" 'save-buffer
  "x c" 'save-buffers-kill-terminal
  "x h" 'mark-whole-buffer
  "x d" 'dired
  "x k" 'kill-buffer
  "x 3" 'split-window-right
  "x 2" 'split-window-below
  "l" 'goto-line
  "s" 'swiper
  "q" 'quit-window
  "0" 'delete-window
  "1" 'select-window-1
  "2" 'select-window-2
  "3" 'select-window-3
  "4" 'select-window-4
  "5" 'select-window-5
  ;; org
  "o l" 'org-insert-link-global
  "o o" 'org-open-at-point-global
  "o c" 'org-capture
  "o y" 'xhcoding/yank-link
  "o a" 'org-agenda
  )


;; about help
(evil-leader/set-key
  "h t" 'help-with-tutorial
  "h f" 'describe-function
  "h v" 'describe-variable
  "h k" 'describe-key
  "h m" 'describe-mode)

;; about file
(evil-leader/set-key
  "f o" 'counsel-find-file
  "f r" 'recentf-open-files
  "f n" 'rename-this-file-and-buffer
  "f d" 'delete-this-file
  "f s" 'save-buffer
  "f b" 'browse-current-file
  )

;; about gtags
(evil-leader/set-key-for-mode 'c-mode
  "g c" 'counsel-gtags-create-tags
  "g u" 'counsel-gtags-update-tags
  "g w" 'counsel-gtags-dwim
  "g d" 'counsel-gtags-find-definition
  "g r" 'counsel-gtags-find-reference
  "g s" 'counsel-gtags-find-symbol
  "g f" 'counsel-gtags-find-file
  )

(evil-leader/set-key-for-mode 'c++-mode
  "g c" 'counsel-gtags-create-tags
  "g u" 'counsel-gtags-update-tags
  "g w" 'counsel-gtags-dwim
  "g d" 'counsel-gtags-find-definition
  "g r" 'counsel-gtags-find-reference
  "g s" 'counsel-gtags-find-symbol
  "g f" 'counsel-gtags-find-file
  )


;; about projectile
(evil-leader/set-key
  "p o" 'sr-open-file
  "p t" 'my-simple-todo)

;; about blog
(evil-leader/set-key
  "b s" 'org-octopress
  "b n" 'org-octopress-new-post
  "b d" 'org-octopress-delete-post
  "b r" 'org-octopress-refresh)


;; change default

(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
(define-key evil-insert-state-map (kbd "C-v") 'scroll-up-command)

;; magit
(evil-leader/set-key
  "m s" 'magit-status
  "m h" 'magit-dispatch-popup)

;; org
(evil-leader/set-key-for-mode 'org-mode
  "o t" 'org-todo
  "o #" 'org-update-statistics-cookies
  "o ." 'org-time-stamp
  "o >" 'org-goto-calendar)


(provide 'init-keys)
