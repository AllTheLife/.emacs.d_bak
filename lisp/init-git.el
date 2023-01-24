;;; init-git.el --- Git SCM 的支持 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'magit)

(require-package 'fullframe)
(with-eval-after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(require-package 'diff-hl)
(add-hook 'after-init-hook 'global-diff-hl-mode)


(provide 'init-git)
;;; init-git.el ends here
