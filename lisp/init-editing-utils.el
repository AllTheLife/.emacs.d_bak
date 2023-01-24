;;; init-editing-utils.el --- 日常编辑助手 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (fboundp 'electric-pair-mode)
  (add-hook 'org-mode-hook 'electric-pair-mode)
  (add-hook 'emacs-lisp-mode-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)

(install-vc-package "https://github.com/manateelazycat/grammatical-edit" "grammatical-edit")
(add-hook 'after-init-hook (lambda () (require 'grammatical-edit)))
(with-eval-after-load 'grammatical-edit
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
                 ))
    (add-hook hook '(lambda () (grammatical-edit-mode 1))))
  (define-key grammatical-edit-mode-map (kbd "(") 'grammatical-edit-open-round)
  (define-key grammatical-edit-mode-map (kbd "[") 'grammatical-edit-open-bracket)
  (define-key grammatical-edit-mode-map (kbd "{") 'grammatical-edit-open-curly)
  (define-key grammatical-edit-mode-map (kbd ")") 'grammatical-edit-close-round)
  (define-key grammatical-edit-mode-map (kbd "]") 'grammatical-edit-close-bracket)
  (define-key grammatical-edit-mode-map (kbd "}") 'grammatical-edit-close-curly)
  (define-key grammatical-edit-mode-map (kbd "=") 'grammatical-edit-equal)

  (define-key grammatical-edit-mode-map (kbd "%") 'grammatical-edit-match-paren)
  (define-key grammatical-edit-mode-map (kbd "\"") 'grammatical-edit-double-quote)
  (define-key grammatical-edit-mode-map (kbd "'") 'grammatical-edit-single-quote)

  (define-key grammatical-edit-mode-map (kbd "SPC") 'grammatical-edit-space)
  ;; (define-key grammatical-edit-mode-map (kbd "RET") 'grammatical-edit-newline)  ;; 这会使 `lsp-bridge' 工作不良

  (define-key grammatical-edit-mode-map (kbd "M-o") 'grammatical-edit-backward-delete)
  (define-key grammatical-edit-mode-map (kbd "C-d") 'grammatical-edit-forward-delete)
  (define-key grammatical-edit-mode-map (kbd "C-k") 'grammatical-edit-kill)

  (define-key grammatical-edit-mode-map (kbd "M-\"") 'grammatical-edit-wrap-double-quote)
  (define-key grammatical-edit-mode-map (kbd "M-'") 'grammatical-edit-wrap-single-quote)
  (define-key grammatical-edit-mode-map (kbd "M-[") 'grammatical-edit-wrap-bracket)
  (define-key grammatical-edit-mode-map (kbd "M-{") 'grammatical-edit-wrap-curly)
  (define-key grammatical-edit-mode-map (kbd "M-(") 'grammatical-edit-wrap-round)
  (define-key grammatical-edit-mode-map (kbd "M-)") 'grammatical-edit-unwrap)

  (define-key grammatical-edit-mode-map (kbd "M-n") 'grammatical-edit-jump-right)
  (define-key grammatical-edit-mode-map (kbd "M-p") 'grammatical-edit-jump-left)
  (define-key grammatical-edit-mode-map (kbd "M-:") 'grammatical-edit-jump-out-pair-and-newline)

  (define-key grammatical-edit-mode-map (kbd "C-j") 'grammatical-edit-jump-up))


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


;; 炫酷的特效，可以提示你的光标在哪里
(when (maybe-require-package 'beacon)
  (setq-default beacon-lighter "")
  (setq-default beacon-size 20)
  (beacon-mode)
  (add-hook 'after-init-hook 'beacon-mode))


;; 识别驼峰式单词

(require 'subword)
(add-hook 'prog-mode-hook 'subword-mode)

(with-eval-after-load 'diminish
  (diminish 'subword-mode))


;; 绘制行号
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))


;; 彩虹括号

(require-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; 将换行符显示为水平直线

(when (maybe-require-package 'page-break-lines)
  (add-hook 'after-init-hook 'global-page-break-lines-mode)
  (setq page-break-lines-char ?=) ;; 将换行符显示为一行的 =
  (with-eval-after-load 'page-break-lines
    (diminish 'page-break-lines-mode)))


;; 用Meow来管理Emacs的移动操作

(when (maybe-require-package 'meow)
  (require 'meow)
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


;; 按下 "C-c /" 来注释
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)


;; 快速跳转
(require-package 'avy)
(global-set-key (kbd "C-c a c") 'avy-goto-char)
(global-set-key (kbd "C-c a w") 'avy-goto-char-2)
(global-set-key (kbd "C-c a b") 'avy-goto-word-0)
(setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o))


;; Undo Redo 集成
(when (maybe-require-package 'undo-fu)
  (require-package 'undo-fu-session)
  (require-package 'vundo)

  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (add-hook 'after-init-hook 'global-undo-fu-session-mode)
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))


;; 自动保存
(install-vc-package "https://github.com/manateelazycat/auto-save" "auto-save")
(require 'auto-save)
(setq auto-save-silent t)
(setq auto-save-delete-trailing-whitespace t)
(add-hook 'after-init-hook 'auto-save-enable)


;; 展开区域
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;; 使用 color-rg 来搜索和重构
(install-vc-package "https://github.com/manateelazycat/color-rg" "color-rg")
(require 'color-rg)
(global-set-key (kbd "C-r") 'color-rg-search-input-in-project)


;; 快速搜索
(install-vc-package "https://github.com/manateelazycat/blink-search" "blink-search")
(require 'blink-search)
(global-set-key (kbd "C-c s") 'blink-search)

(when (display-graphic-p)
 (setq blink-search-enable-posframe t))

(when (fboundp 'meow-insert-mode)
  (advice-add 'blink-search :after 'meow-insert-mode))


;; 必要时以root用户编辑文件
(require-package 'sudo-edit)


;; 想兔子一样在标记之间跳跃
(install-vc-package "https://github.com/liuyinz/binky-mode" "binky-mode")
(require 'binky-mode)
(add-to-list 'after-init-hook 'binky-mode)
(add-to-list 'after-init-hook 'binky-margin-mode)
(global-set-key (kbd "C-c b a") 'binky-add)
(global-set-key (kbd "C-c b j") 'binky-jump)


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


;; 查找孤立的函数
(install-vc-package "https://github.com/manateelazycat/find-orphan" "find-orphan")
(add-hook 'after-init-hook (lambda () (require 'find-orphan)))


;; 快速编辑
(install-vc-package "https://github.com/manateelazycat/thing-edit" "thing-edit")
(add-hook 'after-init-hook (lambda () (require 'thing-edit)))
(global-set-key (kbd "C-c e") 'thing-cut-sexp)


;; 建立Deno运行环境
(require-package 'websocket)
(install-vc-package "https://github.com/manateelazycat/deno-bridge" "deno-bridge")
(require 'deno-bridge)


;; 插入翻译后的内容
(defun insert-translated-name (content to)
  (interactive "sContent: \nsTo: ")
  (setq input (concat "trans -t " to " -b " content))
  (insert (downcase (shell-command-to-string input)))
  (puni-backward-delete-char))


;; 在英文单词与中文单词之间自动插入空格
(require-package 'pangu-spacing)
(global-pangu-spacing-mode)


;; 到底是哪个按键呢？
(require-package 'which-key)
(which-key-mode 1)

(when (display-graphic-p)
 (require-package 'which-key-posframe)
 (which-key-posframe-mode 1))


;; 为Emacs提供中文支持

(require-package 'rime)
(setq default-input-method "rime"
      rime-show-candidate 'posframe
      rime-posframe-properties (list :font "SimHei"
                                     :internal-border-width 10))
(global-set-key (kbd "C-<return>") 'toggle-input-method)

(require-package 'jsonrpc)
(install-vc-package "https://github.com/cireu/jieba.el" "jieba")
(require 'jieba)
(add-hook 'after-init-hook 'jieba-mode)
(global-set-key (kbd "M-m") 'jieba-mark-word)


(provide 'init-editing-utils)
;;; init-editing-utils.el ends here.
