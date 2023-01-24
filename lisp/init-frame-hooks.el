;;; init-frame-hooks.el --- 为 GUI/TTY框架的创建提供特定的钩子 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar after-make-console-frame-hooks '()
  "创建新的TTY框架后运行的钩子")
(defvar after-make-window-system-frame-hooks '()
  "创建新的GUI框架后运行的钩子")

(defun run-after-make-frame-hooks (frame)
  "运行配置以响应新创建的 `FRAME' 的钩子.
有选择地运行 `after-make-console-frame-hooks' 或
`after-make-window-system-frame-hooks'."
  (with-selected-frame frame
    (run-hooks (if window-system
                   'after-make-window-system-frame-hooks
                 'after-make-console-frame-hooks))))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(defconst was/initial-frame (selected-frame)
  "Emacs初始化期间处于活动状态的框架(如果有).")

(add-hook 'after-init-hook
          (lambda () (when was/initial-frame
                  (run-after-make-frame-hooks was/initial-frame))))


(provide 'init-frame-hooks)
;;; init-frame-hooks.el ends here
