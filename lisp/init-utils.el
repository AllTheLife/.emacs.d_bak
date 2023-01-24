;;; init-utils.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(define-obsolete-function-alias 'after-load 'with-eval-after-load "")


;; 把模式添加到auto-mode-alist 中使其打开对应文件时自动开启
(defun add-auto-mode (mode &rest patterns)
  "把条目添加到 `auto-mode-alist' 中，使 `MODE' 遇到对应的 `PATTERNS' 自动开启."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;; 类似于 diminish, 但用于 major-mode
(defun was/set-major-mode-name (name)
  "不在缓冲区中显示 major-mode的名字."
  (setq-local mode-name name))

(defun was/major-mode-lighter (mode name)
  (add-hook (derived-mode-hook-name mode)
            (apply-partially 'was/set-major-mode-name name)))


;; Emacs 核心中缺少的字符操作

(defun was/string-all-matches (regex str &optional group)
  "在`STR'查找`REGEX'的所有匹配项, 并返回所有匹配的字符串(组)`GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))



;; 删除当前文件

(defun delete-this-file ()
  "删除此文件，并抹掉对应的缓冲区"
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))



;; 重命名当前文件

(defun rename-this-file-and-buffer (new-name)
  "重命名当前文件和缓冲区，并访问新名称"
  (interactive "New name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))


;; 预览HTML文件

(defun browse-current-file ()
  "用 `browse-url' 把文件作为网页预览."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))


(provide 'init-utils)
;;; init-utils.el ends here
