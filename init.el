;;; init.el --- 加载整个配置文件 -*- lexical-binding: t -*-

;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; 当Emacs发生错误时在`*Bactrace*'中显示回溯，这对调试启动时的问题很有用
;; (setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)) ;; 将`./lisp/'下的所有文件加入加载列表
(require 'init-benchmarking) ;; 加载`./lisp/init-benchmarking.el'以测量启动时间

(defconst *spell-check-support-enabled* nil) ;; 如果你想开启拼写检查就把其值设为 t


;; 在启动期间调整垃圾收集阈值以加快启动速度，之后恢复为正常大小

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;; 基础配置

(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-utils)
(require 'init-site-lisp) ;; 必须在 init-elpa 之前被加载，因为它可能会加载 package.el
;; 运行 (package-initialize)
(require 'init-elpa)      ;; 自动安装包
(require 'init-exec-path) ;; 设置 $PATH
(require 'init-git)
(require 'init-github)


;; 加载额外功能和模式的配置
(require-package 'diminish)

(require 'init-frame-hooks)
(require 'init-themes)
(require 'init-all-the-icons)
(require 'init-gui-frames)
(require 'init-dired)
(add-hook 'after-init-hook (lambda () (require 'init-eaf)))

(require 'init-company)
(require 'init-yas)
(require 'init-lsp)
(require 'init-ctags)
(require 'init-debug)
(require 'init-minibuffer)
(require 'init-windows)
(require 'init-sessions)
(require 'init-editing-utils)
(require 'init-whitespace)

(require 'init-misc)
(require 'init-prog)

;; 允许来自 emacsclient 的访问
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; 加载 custom.el
(when (file-exists-p custom-file)
  (load custom-file))


(provide 'init)
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
