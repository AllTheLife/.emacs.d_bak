;;; init-yas.el --- yasnippet 集成 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'yasnippet)
  (add-hook 'after-init-hook 'yas-global-mode)

(require-package 'yasnippet-snippets)

(when (fboundp 'diminish)
  (add-hook 'prog-mode-hook (lambda () (diminish 'yas-minor-mode))))


(provide 'init-yas)
;;; init-yas.el ends here
