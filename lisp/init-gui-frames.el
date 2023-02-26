;;; init-gui-frames.el --- GUI框架的相关配置 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; 关闭部分GUI的功能

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)



;; 窗口大小和功能

(setq frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))



;; 设置字体
(add-hook 'after-make-window-system-frame-hooks
          (lambda () (set-fontset-font t 'unicode (font-spec :family "SimHei" :size 15))))



;; 浮动窗口

(require-package 'posframe)

(with-eval-after-load 'posframe
  (global-set-key (kbd "C-c p h") 'posframe-hide-all))


;; 支持像素滚动
(add-hook 'after-make-window-system-frame-hooks
          'pixel-scroll-precision-mode)


(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
