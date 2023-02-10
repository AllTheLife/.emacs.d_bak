;;; init-site-lisp.el --- 加载 site-lisp文件夹下的所有elisp文件 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; 设置加载路径

(require 'cl-lib)

;; (defun was/add-subdirs-to-load-path (parent-dir)
;;   "把所有site-lisp/ 下的未隐藏的子目录都加入 `load-path'."
;;   (let ((default-directory parent-dir))
;;     (setq load-path
;;           (append
;;            (cl-remove-if-not
;;             #'file-directory-p
;;             (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
;;            load-path))))

(defun was/add-subdirs-to-load-path (search-dir)
  (interactive)
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             ;; 过滤出不必要的目录，提升 Emacs 启动速度
             (cl-remove-if
              #'(lambda (subdir)
                  (or
                   ;; 不是文件的都移除
                   (not (file-directory-p (concat dir subdir)))
                   ;; 目录匹配下面规则的都移除
                   (member subdir '("." ".." ;Linux 当前目录和父目录
                                    "dist" "node_modules" "__pycache__" ;语言相关的模块目录
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github")))) ;版本控制目录
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        ;; 目录下有 .el .so .dll 文件的路径才添加到 load-path 中，提升 Emacs 启动速度
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll 文件指非 Elisp 语言编写的 Emacs 动态库
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))

          ;; 注意：add-to-list 函数的第三个参数必须为 t ，表示加到列表末尾
          ;; 这样 Emacs 会从父目录到子目录的顺序搜索 Elisp 插件，顺序反过来会导致 Emacs 无法正常启动
          (add-to-list 'load-path subdir-path t))

        ;; 继续递归搜索子目录
        (was/add-subdirs-to-load-path subdir-path)))))

;; 把site-lisp/ 及其下所有未隐藏的子目录都加入 `load-path'
(let ((site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory)))
  (push site-lisp-dir load-path)
  (was/add-subdirs-to-load-path site-lisp-dir))


;;; 从上游获取文件的工具

(defun site-lisp-dir-for (name)
  (expand-file-name (format "site-lisp/%s" name) user-emacs-directory))

(defun site-lisp-library-el-path (name)
  (expand-file-name (format "%s.el" name) (site-lisp-dir-for name)))

(defun download-site-lisp-module (name url)
  (let ((dir (site-lisp-dir-for name)))
    (message "Downloading %s from %s" name url)
    (unless (file-directory-p dir)
      (make-directory dir t))
    (add-to-list 'load-path dir)
    (let ((el-file (site-lisp-library-el-path name)))
      (url-copy-file url el-file t nil)
      el-file)))

(defun ensure-lib-from-url (name url)
  (unless (site-lisp-library-loadable-p name)
    (byte-compile-file (download-site-lisp-module name url))))

(defun site-lisp-library-loadable-p (name)
  "返回 ~/.emacs.d/site-lisp/name/ 下的库 `name' 能否作为源文件被加载."
  (let ((f (locate-library (symbol-name name))))
    (and f (string-prefix-p (file-name-as-directory (site-lisp-dir-for name)) f))))


(provide 'init-site-lisp)
;;; init-site-lisp.el ends here
