;;; init-whitespace.el --- 对空格的特殊处理 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default show-trailing-whitespace nil)


;;; 空格

(defun was/show-trailing-whitespace ()
  "在此缓冲区中高亮显示行为空格."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'was/show-trailing-whitespace))

(global-set-key [remap just-one-space] 'cycle-spacing)


(provide 'init-whitespace)
;;; init-whitespace.el ends here
