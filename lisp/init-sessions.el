;;; init-sessions.el --- 在重新启动后恢复Emacs的会话 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 在 ~/.emacs.d/.emacs.desktop 记录关闭前所打开的文件
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600)
(desktop-save-mode 1)


;; 重启后回复历史和寄存器

(require-package 'session)

(setq session-save-file (locate-user-emacs-file ".session"))
(setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
(setq session-save-file-coding-system 'utf-8)

(add-hook 'after-init-hook 'session-initialize)


;; 设置 desktop文件中要保存的内容，并指定其最大长度
(setq desktop-globals-to-save
      '((comint-input-ring        . 50)
        (compile-history          . 30)
        desktop-missing-file-warning
        (dired-regexp-history     . 20)
        (extended-command-history . 30)
        (face-name-history        . 20)
        (file-name-history        . 100)
        (grep-find-history        . 30)
        (grep-history             . 30)
        (consult-history          . 100)
        (magit-revision-history   . 50)
        (minibuffer-history       . 50)
        (org-clock-history        . 50)
        (org-refile-history       . 50)
        (org-tags-history         . 50)
        (query-replace-history    . 60)
        (read-expression-history  . 60)
        (regexp-history           . 60)
        (regexp-search-ring       . 20)
        register-alist
        (search-ring              . 20)
        (shell-command-history    . 50)
        tags-file-name
        tags-table-list))


(provide 'init-sessions)
;;; init-sessions.el ends here
