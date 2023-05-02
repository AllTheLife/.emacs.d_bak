;;; init-editing-utils.el --- 日常编辑助手 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; 一些基础选项

(global-hl-line-mode)
(setq-default
 column-number-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 scroll-preserve-screen-position 'always
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

(add-hook 'after-init-hook 'delete-selection-mode)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

(add-hook 'after-init-hook 'transient-mark-mode)



;;; 结构化编辑

(install-vc-package "https://github.com/manateelazycat/fingertip" "fingertip")
(require 'fingertip)
(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'php-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'qml-mode-hook
               'jade-mode-hook
               'css-mode-hook
               'ruby-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'minibuffer-inactive-mode-hook
               'typescript-mode-hook

               'c-ts-mode-hook
               'c++-ts-mode-hook
               'cmake-ts-mode-hook
               'toml-ts-mode-hook
               'css-ts-mode-hook
               'js-ts-mode-hook
               'json-ts-mode-hook
               'python-ts-mode-hook
               'bash-ts-mode-hook
               'typescript-ts-mode-hook
               ))
  (add-hook hook '(lambda () (fingertip-mode 1))))

(with-eval-after-load 'fingertip
  (define-key fingertip-mode-map (kbd "(") 'fingertip-open-round)
  (define-key fingertip-mode-map (kbd "[") 'fingertip-open-bracket)
  (define-key fingertip-mode-map (kbd "{") 'fingertip-open-curly)
  (define-key fingertip-mode-map (kbd ")") 'fingertip-close-round)
  (define-key fingertip-mode-map (kbd "]") 'fingertip-close-bracket)
  (define-key fingertip-mode-map (kbd "}") 'fingertip-close-curly)
  (define-key fingertip-mode-map (kbd "=") 'fingertip-equal)

  (define-key fingertip-mode-map (kbd "%") 'fingertip-match-paren)
  (define-key fingertip-mode-map (kbd "\"") 'fingertip-double-quote)
  (define-key fingertip-mode-map (kbd "'") 'fingertip-single-quote)

  (define-key fingertip-mode-map (kbd "SPC") 'fingertip-space)
  (define-key fingertip-mode-map (kbd "RET") 'fingertip-newline)

  (define-key fingertip-mode-map (kbd "M-o") 'fingertip-backward-delete)
  (define-key fingertip-mode-map (kbd "C-d") 'fingertip-forward-delete)
  (define-key fingertip-mode-map (kbd "C-k") 'fingertip-kill)

  (define-key fingertip-mode-map (kbd "M-\"") 'fingertip-wrap-double-quote)
  (define-key fingertip-mode-map (kbd "M-'") 'fingertip-wrap-single-quote)
  (define-key fingertip-mode-map (kbd "M-[") 'fingertip-wrap-bracket)
  (define-key fingertip-mode-map (kbd "M-{") 'fingertip-wrap-curly)
  (define-key fingertip-mode-map (kbd "M-(") 'fingertip-wrap-round)
  (define-key fingertip-mode-map (kbd "M-)") 'fingertip-unwrap)

  (define-key fingertip-mode-map (kbd "M-p") 'fingertip-jump-left)
  (define-key fingertip-mode-map (kbd "M-n") 'fingertip-jump-right)
  (define-key fingertip-mode-map (kbd "M-:") 'fingertip-jump-up)

  (define-key fingertip-mode-map (kbd "C-j") 'fingertip-jump-out-pair-and-newline))



;;; 识别驼峰式单词

(require 'subword)
(add-hook 'prog-mode-hook 'subword-mode)

(with-eval-after-load 'diminish
  (diminish 'subword-mode))


;;; 将换行符显示为水平直线

(when (maybe-require-package 'page-break-lines)
  (add-hook 'after-init-hook 'global-page-break-lines-mode)
  (setq page-break-lines-char ?=) ;; 将换行符显示为一行的 =
  (with-eval-after-load 'page-break-lines
    (diminish 'page-break-lines-mode)))


;;; 用Meow来管理Emacs的移动操作

(add-hook 'after-make-window-system-frame-hooks
          (lambda () (progn (require-package 'meow)
                            (require 'meow))))
(with-eval-after-load 'meow
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)
    (meow-motion-overwrite-define-key
     ;; use e to move up, n to move down.
     ;; since special modes usually use n to move down, we only overwrite e here.
     '("e" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     '("?" . meow-cheatsheet)
     ;; to execute the originally e in motion state, use spc e.
     '("e" . "h-e")
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("1" . meow-expand-1)
     '("2" . meow-expand-2)
     '("3" . meow-expand-3)
     '("4" . meow-expand-4)
     '("5" . meow-expand-5)
     '("6" . meow-expand-6)
     '("7" . meow-expand-7)
     '("8" . meow-expand-8)
     '("9" . meow-expand-9)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("/" . meow-visit)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("e" . meow-prev)
     '("E" . meow-prev-expand)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-right)
     '("I" . meow-right-expand)
     '("j" . meow-join)
     '("k" . meow-kill)
     '("l" . meow-line)
     '("L" . meow-goto-line)
     '("m" . meow-mark-word)
     '("M" . meow-mark-symbol)
     '("n" . meow-next)
     '("N" . meow-next-expand)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("r" . meow-replace)
     '("s" . meow-insert)
     '("S" . meow-open-above)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-search)
     '("w" . meow-next-word)
     '("W" . meow-next-symbol)
     '("x" . delete-char)
     '("X" . backward-delete-char)
     '("y" . meow-save)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (meow-setup)

  (setq meow-replace-state-name-list
        '((normal . "NOR")
	  (beacon . "BEA")
	  (insert . "INS")
	  (motion . "MOT")
	  (keypad . "KEY")))

  (meow-global-mode 1))



;;; 按下 "C-c /" 来注释

(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)



;;; Undo Redo 集成

(when (maybe-require-package 'undo-fu)
  (require-package 'undo-fu-session)
  (require-package 'vundo)

  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (add-hook 'after-init-hook 'undo-fu-session-global-mode)
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))



;;; 自动保存

(install-vc-package "https://github.com/manateelazycat/auto-save" "auto-save")
(run-with-idle-timer 2 nil (lambda () (require 'auto-save)))
(with-eval-after-load 'auto-save
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t)
  (auto-save-enable))



;;; 展开区域

(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)



;;; 使用 color-rg 来搜索和重构

(install-vc-package "https://github.com/manateelazycat/color-rg" "color-rg")
(defun require-color-rg ()
  (interactive)
  (if (fboundp 'color-rg-mode)
      (color-rg-search-input-in-project)
    (progn (require 'color-rg)
           (color-rg-search-input-in-project))))
(global-set-key (kbd "C-r") 'require-color-rg)


;;; 像兔子一样在标记之间跳跃

(install-vc-package "https://github.com/liuyinz/binky-mode" "binky-mode")
(require 'binky-mode)

(add-to-list 'after-init-hook 'binky-mode)
(add-to-list 'after-init-hook 'binky-margin-mode)
(global-set-key (kbd "C-c b a") 'binky-add)
(global-set-key (kbd "C-c b j") 'binky-jump)



;;; 查找孤立的函数

(install-vc-package "https://github.com/manateelazycat/find-orphan" "find-orphan")
(run-with-idle-timer 5 nil (lambda () (require 'find-orphan)))



;;; 插入翻译后的名字

(install-vc-package "https://github.com/manateelazycat/insert-translated-name" "insert-translated-name")
(with-eval-after-load 'deno-bridge
  (require 'insert-translated-name)
  (global-set-key (kbd "C-c i n") 'insert-translated-name-insert))



;;; 在英文单词与中文单词之间自动插入空格

(require-package 'pangu-spacing)
(global-pangu-spacing-mode)



;;; 到底是哪个按键呢？

(require-package 'which-key)
(which-key-mode 1)

(require-package 'which-key-posframe)

(add-hook 'after-make-window-system-frame-hooks
          (lambda () (which-key-posframe-mode 1)))



;;; 为Emacs提供中文支持

(require-package 'rime)
(setq default-input-method "rime"
      rime-show-candidate 'posframe
      rime-posframe-properties (list :font "SimHei"
                                     :internal-border-width 10))
(global-set-key (kbd "C-<return>") 'toggle-input-method)

(require-package 'jsonrpc)
(install-vc-package "https://github.com/cireu/jieba.el" "jieba")
(run-with-idle-timer 3 nil (lambda () (require 'jieba)))
(global-set-key (kbd "M-m") 'jieba-mark-word)
(with-eval-after-load 'jieba
  (jieba-mode 1))

(install-vc-package "https://github.com/Eason0210/im-cursor-chg" "im-cursor-chg")
(with-eval-after-load 'rime
  (require 'im-cursor-chg)
  (cursor-chg-mode 1))



;;; 运行Org-mode中的代码片段

(org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (C . t)
        (java . t)
        (js . t)
        (ruby . t)
        (ditaa . t)
        (python . t)
        (shell . t)
        (latex . t)
        (plantuml . t)
        (R . t)))



;;; 操作多个symbol

(require-package 'symbol-overlay)

(with-eval-after-load 'symbol-overlay-autoloads
  (add-hook 'prog-mode-hook 'symbol-overlay-mode)
  (add-hook 'org-mode-hook 'symbol-overlay-mode)
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (global-set-key (kbd "C-M-i") 'symbol-overlay-remove-all))



;;; 使用喜欢的模式在单独的缓冲区中编辑注释/字符串/文档字符串/代码块

(require 'separedit)

(define-key prog-mode-map        (kbd "C-c '") #'separedit)
(define-key minibuffer-local-map (kbd "C-c '") #'separedit)
(define-key help-mode-map        (kbd "C-c '") #'separedit)

(setq separedit-default-mode 'org-mode)
(setq separedit-write-file-when-execute-save t)
(setq separedit-remove-trailing-spaces-in-comment t)


(provide 'init-editing-utils)
;;; init-editing-utils.el ends here.
