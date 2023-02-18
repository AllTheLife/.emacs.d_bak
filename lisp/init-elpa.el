;;; init-elpa.el --- Package.el 的设置和助手 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)
(require 'cl-lib)


;;; 把packages为每个Emacs版本安装到独立的文件夹中，以防止不兼容问题
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))



;;; 设置package源
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))



;;; 按需安装 packages

(defun require-package (package &optional min-version no-refresh)
  "安装给定的 `PACKAGE', 可以选择安装一定版本.
如果 `NO-REFRESH' 不是nil, 则此 `PACKAGE' 将不会重新下载以找到安装包."
  (when (stringp min-version)
    (setq min-version (version-to-list min-version)))
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
             (best (car (sort known (lambda (a b)
                                      (version-list-<= (package-desc-version b)
                                                       (package-desc-version a)))))))
        (if (and best (version-list-<= min-version (package-desc-version best)))
            (package-install best)
          (if no-refresh
              (error "No version of %s >= %S is available" package min-version)
            (package-refresh-contents)
            (require-package package min-version t)))
        (package-installed-p package min-version))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "尝试安装软件包, 如果成功就返回t.
如果发生故障, 就返回nil并打印警告信息.
可以选择安装一定版本. 如果 `NO-REFRESH' 不是nil, 则此 `PACKAGE' 将不会重新下载以找到安装包."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))


;;; 启动 package.el

(setq package-enable-at-startup nil)
(package-initialize)



;; package.el 仅在加载完配置文件后才能正确地更新package-selected-packages中的软件包，这是一个bug。
;; 在启动完成后，我们将所加载的软件包添加到package-selected-packages 中来解决此问题。

(defvar was/required-packages nil)

(defun was/note-selected-package (oldfun package &rest args)
    "如果 `OLDFUN' 报告 `PACKAGE' 被成功安装, 请注意, 软件包的名字会被添加到 `was/required-packages'.
这个函数是 `require-package' 的一个 advice, 同时传递了 `ARGS'."
  (let ((available (apply oldfun package args)))
    (prog1
        available
      (when available
        (add-to-list 'was/required-packages package)))))

(advice-add 'require-package :around 'was/note-selected-package)

(when (fboundp 'package--save-selected-packages)
  (require-package 'seq)
  (add-hook 'after-init-hook
            (lambda ()
              (package--save-selected-packages
               (seq-uniq (append was/required-packages package-selected-packages))))))


(require-package 'fullframe)
(fullframe list-packages quit-window)


(let ((package-check-signature nil))
  (require-package 'gnu-elpa-keyring-update))

(require-package 'auto-package-update)


(defun was/set-tabulated-list-column-width (col-name width)
  "把所有名为 `COL-NAME' 的列设置为给定的宽度 `WIDTH'."
  (when (> width (length col-name))
    (cl-loop for column across tabulated-list-format
             when (string= col-name (car column))
             do (setf (elt column 1) width))))

(defun was/maybe-widen-package-menu-columns ()
  "扩大软件包菜单中的一些列以避免显示被截断."
  (when (boundp 'tabulated-list-format)
    (was/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (was/set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook 'was/maybe-widen-package-menu-columns)


;; 安装第三方软件包
(defun install-vc-package (url name)
  (unless (file-exists-p (concat "~/.emacs.d/site-lisp/" name))
    (shell-command (concat "git clone " url " ~/.emacs.d/site-lisp/" name)))
  (add-to-list 'load-path (concat "~/.emacs.d/site-lisp/" name)))


(provide 'init-elpa)
;;; init-elpa.el ends here
