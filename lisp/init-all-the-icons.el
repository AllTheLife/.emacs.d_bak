;;; init-all-the-icons.el --- 为Emacs提供图标支持 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (display-graphic-p)
  (require-package 'all-the-icons))

(with-eval-after-load 'marginalia
  (require-package 'all-the-icons-completion)
  (add-hook 'marginalia-mode-hook 'all-the-icons-completion-marginalia-setup)
  (add-hook 'after-init-hook 'all-the-icons-completion-mode))

(provide 'init-all-the-icons)
;;; init-all-the-icons.el ends here
