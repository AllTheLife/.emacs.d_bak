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



(defun remember-init ()
  "记住当前位置."
  (interactive)
  (point-to-register 8)
  (message "Have remember one position"))

(defun remember-jump ()
  "跳转到最后一次的位置."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

(global-set-key (kbd "C-c i i") 'remember-init)
(global-set-key (kbd "C-c i e") 'remember-jump)



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



; 炫酷的特效，可以提示你的光标在哪里

(when (maybe-require-package 'beacon)
  (setq-default beacon-lighter "")
  (setq-default beacon-size 20)
  (beacon-mode)
  (add-hook 'after-init-hook 'beacon-mode))



;; 快速跳转

(require-package 'avy)
(global-set-key (kbd "C-c a c") 'avy-goto-char)
(global-set-key (kbd "C-c w") 'avy-goto-char-2)
(global-set-key (kbd "C-c a b") 'avy-goto-word-0)

 ;; 为Colemak用户提供特殊按键支持
(setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o))



;; 快速编辑文本

(install-vc-package "https://github.com/manateelazycat/thing-edit" "thing-edit")
(run-with-idle-timer 2 nil (lambda () (require 'thing-edit)))

(install-vc-package "https://github.com/lyjdwh/avy-thing-edit" "avy-thing-edit")
(with-eval-after-load 'thing-edit
  (require 'avy-thing-edit)
  (global-set-key (kbd "C-c k u") 'avy-thing-cut-url)
  (global-set-key (kbd "C-c k l") 'avy-thing-cut-line)
  (global-set-key (kbd "C-c k w") 'avy-thing-cut-word)
  (global-set-key (kbd "C-c k s") 'avy-thing-cut-sexp)
  (global-set-key (kbd "C-c k p") 'avy-thing-cut-page)
  (global-set-key (kbd "C-c k f") 'avy-thing-cut-defun)
  (global-set-key (kbd "C-c k c") 'avy-thing-cut-comment)
  (global-set-key (kbd "C-c k ,") 'avy-thing-cut-sentence)
  (global-set-key (kbd "C-c k .") 'avy-thing-cut-paragraph)
  (global-set-key (kbd "C-c k '") 'avy-thing-cut-parentheses)
  (global-set-key (kbd "C-c C-k u") 'avy-thing-copy-url)
  (global-set-key (kbd "C-c C-k l") 'avy-thing-copy-line)
  (global-set-key (kbd "C-c C-k w") 'avy-thing-copy-word)
  (global-set-key (kbd "C-c C-k s") 'avy-thing-copy-sexp)
  (global-set-key (kbd "C-c C-k p") 'avy-thing-copy-page)
  (global-set-key (kbd "C-c C-k f") 'avy-thing-copy-defun)
  (global-set-key (kbd "C-c C-k c") 'avy-thing-copy-comment)
  (global-set-key (kbd "C-c C-k ,") 'avy-thing-copy-sentence)
  (global-set-key (kbd "C-c C-k .") 'avy-thing-copy-paragraph)
  (global-set-key (kbd "C-c C-k '") 'avy-thing-copy-parentheses))



;; 模糊搜索
(install-vc-package "https://github.com/manateelazycat/blink-search" "blink-search")

(defun require-blink-search ()
  (interactive)
  (if (fboundp 'blink-search)
      (blink-search)
    (progn (require 'blink-search)
           (blink-search))))
(global-set-key (kbd "C-c s") 'require-blink-search)

(with-eval-after-load 'meow
  (advice-add 'blink-search :after 'meow-insert-mode))



;; 必要时以root用户编辑文件
(require-package 'sudo-edit)



;; 翻译单词
(require-package 'go-translate)
(with-eval-after-load 'go-translate
  (setq gts-translate-list '(("en" "zh")))
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-google-engine) (gts-stardict-engine))
         :render (gts-posframe-pin-render))))
(global-set-key (kbd "C-c t") 'gts-do-translate)



;; Eshell强化
(install-vc-package "https://github.com/manateelazycat/aweshell" "aweshell")
(run-with-idle-timer 3 nil (lambda () (require 'aweshell)))

(with-eval-after-load 'aweshell
  (setq aweshell-auto-suggestion-p t))

(global-set-key (kbd "C-c r") 'aweshell-dedicated-toggle)
(global-set-key (kbd "C-c C-r") 'aweshell-new)



;; 轻松粘贴代码到笔记里

(install-vc-package "https://github.com/AmaiKinono/clue" "clue")
(require 'clue)
(add-hook 'find-file-hook #'clue-auto-enable-clue-mode)


;; 在Emacs中使用Bing Chat

(require-package 'async-await)

(install-vc-package "https://github.com/xhcoding/emacs-aichat" "emacs-aichat")
(run-with-idle-timer 2 nil
                     (lambda ()
                       (require 'aichat-bingai)
                       (add-hook 'kill-emacs-hook (lambda () (delete-file "~/.emacs.d/aichat.md")))

                       (aichat-bingai-prompt-create "translator"
                                                    :input-prompt "请翻译: "
                                                    :text-format "请识别我说的语言，如果我说的语言是中文，请把它翻译成英文；如果我说的语言不是中文，请把它翻译成中文。你的返回内容必须是翻译后的文本，不能包括其它内容，不要输出我输入的是什么语言和句号以及资料出处:\n%s"
                                                    :chat t
                                                    :assistant t
                                                    :replace-or-insert t)

                       (aichat-bingai-prompt-create "reconstructor"
                                                    :input-prompt "请输入要重构的代码: "
                                                    :text-format "请对下面这些代码生成重构建议，你的回答应只包含重构后的代码和你对代码的建议，而不包括形如你更改了什么、句号、资料来源等其他信息:\n%s"
                                                    :chat t
                                                    :assistant t
                                                    :replace-or-insert t)

                       (aichat-bingai-prompt-create "explainer"
                                                    :input-prompt "请输入要讲解的代码: "
                                                    :text-format "请讲解下面这些代码，你的回答应只包含讲解的内容，而不包括形如你资料来源等其他信息:\n%s"
                                                    :chat t
                                                    :assistant t
                                                    :replace-or-insert t))
                     )



;; 提升英文阅读体验

;; (install-vc-package "https://github.com/ginqi7/dictionary-overlay" "dictionary-overlay")
;; (with-eval-after-load 'websocket-bridge
;;   (require 'dictionary-overlay))



;; 截屏代码

(install-vc-package "https://github.com/tecosaur/screenshot" "screenshot")
(require 'screenshot)


(provide 'init-utils)
;;; init-utils.el ends here
