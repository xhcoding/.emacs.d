;;; init-vcs.el --- emacs configure  -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:

;;; Code:


(use-package magit
  :config
  (when IS-WINDOWS
    (setenv "GIT_ASKPASS" "git-gui--askpass")))

(use-package git-messenger
  :bind (:map vc-prefix-map
              ("p" . git-messenger:popup-message)
              :map git-messenger-map
              ("m" . git-messenger:copy-message))
  :init (setq git-messenger:show-detail t
              git-messenger:use-magit-popup t)
  :config
  (with-no-warnings
    (with-eval-after-load 'hydra
      (defhydra git-messenger-hydra (:color blue)
        ("s" git-messenger:popup-show "show")
        ("c" git-messenger:copy-commit-id "copy hash")
        ("m" git-messenger:copy-message "copy message")
        ("," (catch 'git-messenger-loop (git-messenger:show-parent)) "go parent")
        ("q" git-messenger:popup-close "quit"))))

  (defun my-git-messenger:format-detail (vcs commit-id author message)
    (if (eq vcs 'git)
        (let ((date (git-messenger:commit-date commit-id))
              (colon (propertize ":" 'face 'font-lock-comment-face)))
          (concat
           (format "%s%s %s \n%s%s %s\n%s  %s %s \n"
                   (propertize "Commit" 'face 'font-lock-keyword-face) colon
                   (propertize (substring commit-id 0 8) 'face 'font-lock-comment-face)
                   (propertize "Author" 'face 'font-lock-keyword-face) colon
                   (propertize author 'face 'font-lock-string-face)
                   (propertize "Date" 'face 'font-lock-keyword-face) colon
                   (propertize date 'face 'font-lock-string-face))
           (propertize (make-string 38 ?─) 'face 'font-lock-comment-face)
           message
           (propertize "\nPress q to quit" 'face '(:inherit (font-lock-comment-face italic)))))
      (git-messenger:format-detail vcs commit-id author message)))

  (defun my-git-messenger:popup-message ()
    "Popup message with `posframe', `pos-tip', `lv' or `message', and dispatch actions with `hydra'."
    (interactive)
    (let* ((vcs (git-messenger:find-vcs))
           (file (buffer-file-name (buffer-base-buffer)))
           (line (line-number-at-pos))
           (commit-info (git-messenger:commit-info-at-line vcs file line))
           (commit-id (car commit-info))
           (author (cdr commit-info))
           (msg (git-messenger:commit-message vcs commit-id))
           (popuped-message (if (git-messenger:show-detail-p commit-id)
                                (my-git-messenger:format-detail vcs commit-id author msg)
                              (cl-case vcs
                                (git msg)
                                (svn (if (string= commit-id "-")
                                         msg
                                       (git-messenger:svn-message msg)))
                                (hg msg)))))
      (setq git-messenger:vcs vcs
            git-messenger:last-message msg
            git-messenger:last-commit-id commit-id)
      (run-hook-with-args 'git-messenger:before-popup-hook popuped-message)
      (git-messenger-hydra/body)
      (cond ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
             (let ((buffer-name "*git-messenger*"))
               (posframe-show buffer-name
                              :string popuped-message
                              :left-fringe 8
                              :right-fringe 8
                              :internal-border-color (face-foreground 'default)
                              :internal-border-width 1)
               (unwind-protect
                   (push (read-event) unread-command-events)
                 (posframe-delete buffer-name))))
            ((and (fboundp 'pos-tip-show) (display-graphic-p))
             (pos-tip-show popuped-message))
            ((fboundp 'lv-message)
             (lv-message popuped-message)
             (unwind-protect
                 (push (read-event) unread-command-events)
               (lv-delete-window)))
            (t (message "%s" popuped-message)))
      (run-hook-with-args 'git-messenger:after-popup-hook popuped-message)))
  (advice-add #'git-messenger:popup-close :override #'ignore)
  (advice-add #'git-messenger:popup-message :override #'my-git-messenger:popup-message))

(use-package magit-todos)

;; Resolve diff3 conflicts
(use-package smerge-mode
  :ensure nil
  :diminish
  :pretty-hydra
  ((:title (pretty-hydra-title "Smerge" 'octicon "diff")
    :color pink :quit-key "q")
   ("Move"
    (("n" smerge-next "next")
     ("p" smerge-prev "previous"))
    "Keep"
    (("b" smerge-keep-base "base")
     ("u" smerge-keep-upper "upper")
     ("l" smerge-keep-lower "lower")
     ("a" smerge-keep-all "all")
     ("RET" smerge-keep-current "current")
     ("C-m" smerge-keep-current "current"))
    "Diff"
    (("<" smerge-diff-base-upper "upper/base")
     ("=" smerge-diff-upper-lower "upper/lower")
     (">" smerge-diff-base-lower "upper/lower")
     ("R" smerge-refine "refine")
     ("E" smerge-ediff "ediff"))
    "Other"
    (("C" smerge-combine-with-next "combine")
     ("r" smerge-resolve "resolve")
     ("k" smerge-kill-current "kill")
     ("ZZ" (lambda ()
             (interactive)
             (save-buffer)
             (bury-buffer))
      "Save and bury buffer" :exit t))))
  :bind (:map smerge-mode-map
         ("C-c m" . smerge-mode-hydra/body))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (hydra-smerge/body))))))

(provide 'init-vcs)

;;; init-vcs.el ends here
