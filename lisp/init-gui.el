
;; turn off tool bar
(tool-bar-mode -1)

;; turn off menu bar
(menu-bar-mode -1)

;; turn off scroll bar
(scroll-bar-mode -1)

;; enable column number
(column-number-mode t)

;; turn off splash screen
(setq inhibit-splash-screen t)

;; fullscreen at startup
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; change cursor-type
(setq-default cursor-type 'bar)

;; highline
(global-hl-line-mode)


;;==============theme===================

(use-package zenburn-theme
  :unless window-system
  :init
  (progn
    (setq-default custom-enabled-themes '(zenburn))))


;; (use-package color-theme-sanityinc-solarized
;;   :disabled
;;   :if window-system
;;   :init
;;   (progn
;;     (setq-default custom-enabled-themes '(sanityinc-solarized-dark))
;;     ))

(use-package dracula-theme
  :if window-system
  :init
  (progn
    (setq-default custom-enabled-themes '(dracula))))

;; toggle between light and night
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-solarized-light))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-solarized-dark))
  (reapply-themes))


;;==============theme===================


;;==============mode-line===============

(use-package cyphejor :disabled t
  :init
  (progn
    (setq
     cyphejor-rules
     '(:upcase
       ("bookmark"    "→")
       ("buffer"      "β")
       ("diff"        "Δ")
       ("dired"       "δ")
       ("emacs"       "ε")
       ("inferior"    "i" :prefix)
       ("interaction" "i" :prefix)
       ("interactive" "i" :prefix)
       ("lisp"        "λ" :postfix)
       ("menu"        "▤" :postfix)
       ("mode"        "")
       ("package"     "↓")
       ("python"      "π")
       ("shell"       "sh" :postfix)
       ("text"        "ξ")
       ("wdired"      "↯δ")))
    (cyphejor-mode t)))


(use-package anzu)

(defun zilongshanren/update-persp-name ()
  (when (bound-and-true-p persp-mode)
    ;; There are multiple implementations of
    ;; persp-mode with different APIs
    (progn
      (or (not (string= persp-nil-name (safe-persp-name (get-frame-persp))))
	  "Default")
      (let ((name (safe-persp-name (get-frame-persp))))
	(propertize (concat "[" name "] ")
		    'face 'font-lock-preprocessor-face
		    'help-echo "Current Layout name.")))))


(defun spaceline--unicode-number (str)
  "Return a nice unicode representation of a single-digit number STR."
  (cond
   ((string= "1" str) "➊")
   ((string= "2" str) "➋")
   ((string= "3" str) "➌")
   ((string= "4" str) "➍")
   ((string= "5" str) "➎")
   ((string= "6" str) "➏")
   ((string= "7" str) "➐")
   ((string= "8" str) "➑")
   ((string= "9" str) "➒")
   ((string= "0" str) "➓")))

(defun window-number-mode-line ()
  "The current window number. Requires `window-numbering-mode' to be enabled."
  (when (bound-and-true-p window-numbering-mode)
    (let* ((num (window-numbering-get-number))
	   (str (when num (int-to-string num))))
      (spaceline--unicode-number str))))

(defun mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
	      'display `((space :align-to
				(- (+ right right-fringe right-margin) ,reserve)))
	      'face face))

(defun buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
	(match-string 1 buf-coding)
      buf-coding)))

(setq my-flycheck-mode-line
      '(:eval
	(pcase flycheck-last-status-change
	  (`not-checked nil)
	  (`no-checker (propertize " -" 'face 'warning))
	  (`running (propertize " ✷" 'face 'success))
	  (`errored (propertize " !" 'face 'error))
	  (`finished
	   (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
		  (no-errors (cdr (assq 'error error-counts)))
		  (no-warnings (cdr (assq 'warning error-counts)))
		  (face (cond (no-errors 'error)
			      (no-warnings 'warning)
			      (t 'success))))
	     (propertize (format "[%s/%s]" (or no-errors 0) (or no-warnings 0))
			 'face face)))
	  (`interrupted " -")
	  (`suspicious '(propertize " ?" 'face 'warning)))))

(setq projectile-mode-line
      (quote
       (:eval
	(format " P[%s]" (projectile-project-name)))))

(setq-default mode-line-format
	      (list
	       " %1"
	       '(:eval (propertize
			(window-number-mode-line)
			'face
			'font-lock-type-face))
	       " "
	       '(:eval (zilongshanren/update-persp-name))

	       "%1 "
	       ;; the buffer name; the file name as a tool tip
	       '(:eval (propertize "%b " 'face 'font-lock-keyword-face
				   'help-echo (buffer-file-name)))


	       " [" ;; insert vs overwrite mode, input-method in a tooltip
	       '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
				   'face 'font-lock-preprocessor-face
				   'help-echo (concat "Buffer is in "
						      (if overwrite-mode
							  "overwrite"
							"insert") " mode")))

	       ;; was this buffer modified since the last save?
	       '(:eval (when (buffer-modified-p)
			 (concat ","  (propertize "Mod"
						  'face 'font-lock-warning-face
						  'help-echo "Buffer has been modified"))))

	       ;; is this buffer read-only?
	       '(:eval (when buffer-read-only
			 (concat ","  (propertize "RO"
						  'face 'font-lock-type-face
						  'help-echo "Buffer is read-only"))))
	       "] "

	       ;; anzu
	       anzu--mode-line-format

	       ;; relative position, size of file
	       "["
	       (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
	       "/"
	       (propertize "%I" 'face 'font-lock-constant-face) ;; size
	       "] "

	       ;; the current major mode for the buffer.
	       '(:eval (propertize "%m" 'face 'font-lock-string-face
				   'help-echo buffer-file-coding-system))

	       "%1 "
	       my-flycheck-mode-line
	       "%1 "
	       
	       ;; evil state
	       '(:eval evil-mode-line-tag)

	       ;; minor modes
	       minor-mode-alist
	       " "
	       ;; git info
	       `(vc-mode vc-mode)

	       " "

	       '(:eval (list nyan-create))
	       
	       " "
	       ;; global-mode-string goes in mode-line-misc-info
	       mode-line-misc-info

	       (mode-line-fill 'mode-line 22)

	       ;; line and column
	       "(" ;; '%02' to set to 2 chars at least; prevents flickering
	       (propertize "%02l" 'face 'font-lock-type-face) ","
	       (propertize "%02c" 'face 'font-lock-type-face)
	       ") "

	       '(:eval (buffer-encoding-abbrev))
	       
	       ;;mode-line-end-spaces
	       " "
	       ;; add the time, with the date and the emacs uptime in the tooltip
	       '(:eval (propertize (format-time-string "%H:%M")
	                           'help-echo
	                           (concat (format-time-string "%c; ")
	                                   (emacs-uptime "Uptime:%hh"))))
	       " "
	       ))


(setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
      evil-emacs-state-tag    (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
      evil-insert-state-tag   (propertize "[I]" 'face '((:background "chartreuse3") :foreground "white"))
      evil-motion-state-tag   (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
      evil-visual-state-tag   (propertize "[V]" 'face '((:background "gray" :foreground "black")))
      evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))


;;======================================

;;==========font============

(defun my-default-font()
  (interactive)
  
  ;; english font
  (set-face-attribute 'default nil :font (format   "%s:pixelsize=%d" "Source Code Pro" 17))
  ;; chinese font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
		      charset
		      (font-spec :family "WenQuanYi Micro Hei Mono" :size 20))))


(add-to-list 'after-make-frame-functions
	     (lambda (new-frame)
	       (select-frame new-frame)
	       (if window-system
		   (my-default-font)
		 )))

(if window-system
    (my-default-font)
  )

;;============================

(provide 'init-gui)
