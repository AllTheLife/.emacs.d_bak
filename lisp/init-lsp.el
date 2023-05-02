;;; init-lsp.el --- language-server-protocol 集成 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 使用lsp-bridge来进行补全

(require-package 'markdown-mode)

(install-vc-package "https://github.com/manateelazycat/lsp-bridge" "lsp-bridge") ;; 从Github下载lsp-bridge
(run-with-idle-timer 2 nil (lambda () (require 'lsp-bridge)))
(with-eval-after-load 'lsp-bridge
  (add-hook 'emacs-lisp-mode-hook 'lsp-bridge-mode)
  (add-hook 'org-mode-hook 'lsp-bridge-mode)

  ;; (setq lsp-bridge-enable-auto-format-code t)
  ;; (setq lsp-bridge-enable-hover-diagnostic t)
  ;; (setq lsp-bridge-c-lsp-server "ccls")
  (setq acm-enable-tabnine nil)
  (setq acm-enable-yas nil)
  (setq lsp-bridge-enable-org-babe t)
  ;; (setq acm-enable-citre t)
  ;; (setq acm-enable-quick-access t)
  )

(when (fboundp 'diminish)
  (add-hook 'lsp-bridge-mode-hook (lambda () (diminish 'lsp-bridge-mode))))


;; 使用Eglot来进行补全

(require-package 'eglot)
(when (and (fboundp 'eglot-ensure))
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'sh-mode-hook 'eglot-ensure)
  (add-hook 'c++-ts-mode-hook 'eglot-ensure)
  (add-hook 'c-ts-mode-hook 'eglot-ensure)
  (add-hook 'python-ts-mode-hook 'eglot-ensure)
  (add-hook 'sh-ts-mode-hook 'eglot-ensure)
  (add-hook 'mhtml-mode-hook 'eglot-ensure))

;; (with-eval-after-load 'auto-save
;;   (advice-add 'auto-save-buffers :before 'eglot-format-buffer))

;; 显示相关信息
(require-package 'sideline)
(require-package 'sideline-flymake)
(require-package 'sideline-blame)

(setq sideline-backends-skip-current-line t  ; don't display on current line
        sideline-order-left 'down            ; or 'up
        sideline-order-right 'up             ; or 'down
        sideline-format-left "%s   "         ; format for left aligment
        sideline-format-right "   %s"        ; format for right aligment
        sideline-priority 100                ; overlays' priority
        sideline-display-backend-name t
        sideline-backends-left '(sideline-flymake)
        sideline-backends-right '(sideline-blame))

(with-eval-after-load 'sideline-autoloads
  (global-sideline-mode))


(provide 'init-lsp)
;;; init-lsp.el ends here
