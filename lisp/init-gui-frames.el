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
(set-fontset-font t 'unicode (font-spec :family "SimHei" :size 15))


;; 浮动窗口

(when (maybe-require-package 'posframe)
 (add-hook 'after-init-hook 'posframe-delete-all)
 (global-set-key (kbd "C-c p h") 'posframe-hide-all))



;; 标签栏增强
(install-vc-package "https://github.com/manateelazycat/sort-tab" "sort-tab")
(require 'sort-tab)
(sort-tab-mode 1)
(global-set-key (kbd "C-1") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-2") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-3") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-4") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-5") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-6") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-7") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-8") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-9") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-0") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-S-q") 'sort-tab-close-all-tabs)
(global-set-key (kbd "C-s-q") 'sort-tab-close-mode-tabs)
(global-set-key (kbd "C-s-n") 'sort-tab-select-next-tab)
(global-set-key (kbd "C-s-p") 'sort-tab-select-prev-tab)
(global-set-key (kbd "C-;") 'sort-tab-close-current-tab)

(defun restart-sort-tab ()
  (interactive)
  (sort-tab-turn-off)
  (sort-tab-turn-on))
(advice-add 'delete-other-windows :after 'restart-sort-tab) ;; 修复 (delete-other-windows) 后sort-tab消失的问题



;; 更漂亮的 modeline
(when (maybe-require-package 'doom-modeline)
 (add-hook 'after-init-hook #'doom-modeline-mode)
 (setq doom-modeline-support-imenu t
       doom-modeline-buffer-file-name-style 'auto
       doom-modeline-icon t
       doom-modeline-major-mode-icon t
       doom-modeline-major-mode-color-icon t
       doom-modeline-github t
       doom-modeline-time t
       doom-modeline-height 40))
(if (facep 'mode-line-active)
  (set-face-attribute 'mode-line nil :family "Source Code Pro" :height 100))
(set-face-attribute 'mode-line-inactive nil :family "Source Code Pro" :height 100)



;; Org-mode 美化
(when (maybe-require-package 'org-modern)
 (add-hook 'org-mode-hook 'org-modern-mode))

(with-eval-after-load 'org
  (set-face-attribute 'org-level-1 nil :height 1.6 :bold t)
  (set-face-attribute 'org-level-2 nil :height 1.4 :bold t)
  (set-face-attribute 'org-level-3 nil :height 1.2 :bold t))

(when (maybe-require-package 'valign)
  (add-hook 'org-mode-hook #'valign-mode))


(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
