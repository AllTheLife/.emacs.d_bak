;;; init-windows.el --- 处理框架内的窗口 -*- lexical-binding: t -*-
;;; Commentary:

;; 这与 `Windows' 操作系统无关，而是Emacs的 `窗口' 概念。

;;; Code:

;; 当窗口数目大于2时，按下 "C-x o" 来选择窗口
(require-package 'switch-window)
(setq-default switch-window-shortcut-style 'alphabet)
(setq-default switch-window-timeout nil)
(global-set-key (kbd "C-x o") 'switch-window)



;; 当分割窗口时，在新窗口中显示另一个buffer

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "除非提供 `ARG', 否则拆分此窗口并切换到新窗口."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))



(require-package 'winum)
(setq winum-auto-setup-mode-line nil)
(add-hook 'after-init-hook 'winum-mode)


(provide 'init-windows)
;;; init-windows.el ends here
