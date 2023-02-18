;;; init-git.el --- Git SCM 的支持 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'magit)

(require-package 'fullframe)
(with-eval-after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(require-package 'diff-hl)
(add-hook 'after-init-hook 'global-diff-hl-mode)

(defun aborn/simple-git-commit-push ()
  "Simple commit current git project and push to its upstream."
  ;; (interactive "sCommit Message: ")
  (interactive)
  (when (and buffer-file-name
             (buffer-modified-p))
    (save-buffer))                   ;; save it first if modified.
  (magit-diff-unstaged)
  (when (yes-or-no-p "Do you really want to commit everything?")
    (magit-stage-modified)
    ;; (magit-mode-bury-buffer)
    (magit-diff-staged)
    (setq msg (read-string "Commit Message: "))
    (when (= 0 (length msg))
      (setq msg (format-time-string "commit by magit in emacs@%Y-%m-%d %H:%M:%S"
                                    (current-time))))
    (message "commit message is %s" msg)
    ;;(magit-commit (list "-m" msg))
    (magit-call-git "commit" "-m" msg)
    (magit-push-current-to-upstream nil)
    (message "now do async push to %s" (magit-get "remote" "origin" "url")))
  (magit-mode-bury-buffer))

(global-set-key (kbd "C-c c") 'aborn/simple-git-commit-push)


(provide 'init-git)
;;; init-git.el ends here
