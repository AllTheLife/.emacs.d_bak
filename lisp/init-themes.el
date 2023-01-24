;;; init-themes.el --- 主题的默认值 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'ef-themes)
(require-package 'doom-themes)

;; 不提示确认主题安全。这避免了在Emacs > 26.3 上首次启动时出现的问题。
(setq custom-safe-themes t)

;; 设置默认主题
(setq-default custom-enabled-themes '(doom-palenight))

;; 确保主题启用，即使其未被定制
(defun reapply-themes ()
  "强制加载 `custom-enable-themes' 中列出的主题."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(reapply-themes)


(provide 'init-themes)
;;; init-themes.el ends here
