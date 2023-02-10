;;; init-misc.el --- 杂项配置 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(fset 'yes-or-no-p 'y-or-n-p)

;; 使Emacs在退出时不询问是否要结束进程
;; See `https://emacs.stackexchange.com/questions/26565/exiting-emacs-without-confirmation-to-kill-running-processes'
(setq confirm-kill-processes nil)


(provide 'init-misc)
;;; init-misc.el ends here
