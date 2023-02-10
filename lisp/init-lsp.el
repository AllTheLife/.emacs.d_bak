;;; init-lsp.el --- language-server-protocol 集成 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'markdown-mode)

(install-vc-package "https://github.com/manateelazycat/lsp-bridge" "lsp-bridge") ;; 从Github下载lsp-bridge
(add-hook 'after-init-hook (lambda () (require 'lsp-bridge)))
(with-eval-after-load 'lsp-bridge
  (add-hook 'emacs-lisp-mode-hook 'lsp-bridge-mode)
  (add-hook 'org-mode-hook 'lsp-bridge-mode))
;; (setq lsp-bridge-enable-auto-format-code t)
;; (setq lsp-bridge-enable-hover-diagnostic t)
;; (setq lsp-bridge-c-lsp-server "ccls")
(setq acm-enable-tabnine nil)
(setq acm-enable-yas nil)
;; (setq acm-enable-citre t)
;; (setq acm-enable-quick-access t)

(when (fboundp 'diminish)
  (add-hook 'lsp-bridge-mode-hook (lambda () (diminish 'lsp-bridge-mode))))



(require-package 'eglot)
(when (and (fboundp 'eglot-ensure))
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'sh-mode-hook 'eglot-ensure)
  (add-hook 'c++-ts-mode-hook 'eglot-ensure)
  (add-hook 'c-ts-mode-hook 'eglot-ensure)
  (add-hook 'python-ts-mode-hook 'eglot-ensure)
  (add-hook 'sh-ts-mode-hook 'eglot-ensure))

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs '((python-ts-mode python-mode) . ("pyright-langserver"))))


(provide 'init-lsp)
;;; init-lsp.el ends here
