;;; init-windows.el --- 处理框架内的窗口 -*- lexical-binding: t -*-
;;; Commentary:

;; 这与 `Windows' 操作系统无关，而是Emacs的 `窗口' 概念。

;;; Code:

(add-hook 'after-init-hook 'winner-mode)

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



(defun toggle-delete-other-windows ()
  "删除掉frame中的其他窗口(如果有), 不然恢复以前的窗口配置."
  (interactive)
  (if (and winner-mode
           (equal (length (window-list)) 2)) ;; Because of `*sort-tab*'
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'toggle-delete-other-windows)




(unless (memq window-system '(nt w32))
  (require-package 'windswap)
  (add-hook 'after-init-hook (apply-partially 'windmove-default-keybindings 'control))
  (add-hook 'after-init-hook (apply-partially 'windswap-default-keybindings 'shift 'control)))


(provide 'init-windows)
;;; init-windows.el ends here
