;;; init-prog.el --- Initialize programming configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 在行号旁显示文件指示符
(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))


;; Tree-sitter 配置

;; (require-package 'tree-sitter)
;; (require-package 'tree-sitter-langs)
;; (add-hook 'after-init-hook 'global-tree-sitter-mode)
;; (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode)
;; (with-eval-after-load 'tree-sitter
;;   (tree-sitter-load 'elisp "elisp")
;;   (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp)))

(require 'treesit)

(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
        (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go"))
        (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
        (make . ("https://github.com/alemuller/tree-sitter-make"))
        (markdown . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
        (org . ("https://github.com/milisims/tree-sitter-org"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (php . ("https://github.com/tree-sitter/tree-sitter-php"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))

(add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))



;; 一键运行

(install-vc-package "https://github.com/honmaple/emacs-maple-run" "maple-run")
(require 'maple-run)
(add-to-list 'maple-run:alist '((c++-ts-mode c++-mode) :command "g++ %F -o /tmp/%b && /tmp/%b"))
(add-to-list 'maple-run:alist '((python-mode python-ts-mode) :command "python %F"))
(add-to-list 'maple-run:alist '((html-mode web-mode html-ts-mode) :command browse-url-of-file))
(advice-add 'maple-run:finish :after (lambda () (kill-buffer "*maple-run-process*")))
(setq maple-run:timeout 20000)
(global-set-key (kbd "C-c X") 'maple-run)
;; (require-package 'quickrun)
;; (global-set-key (kbd "C-c q q") 'quickrun)
;; (global-set-key (kbd "C-c q b") 'quickrun-region)
;; (global-set-key (kbd "C-c q c") 'quickrun-compile-only)
;; (global-set-key (kbd "C-c q i") 'quickrun-replace-region)


;; 在标签栏下方显示一个 Sticky Header
(install-vc-package "https://github.com/alphapapa/topsy.el" "topsy")
(require 'topsy)
(add-hook 'prog-mode-hook #'topsy-mode)


;; 快速折叠
(require-package 'origami)
(add-hook 'prog-mode-hook 'origami-mode)
(global-set-key (kbd "C-<tab>") 'origami-toggle-node)


;; 在modeline中显示在哪个函数中
(add-hook 'prog-mode-hook 'which-function-mode)

(provide 'init-prog)
;;; init-prog.el ends here
