;;; init-prog.el --- Initialize programming configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 在行号旁显示文件指示符
(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))


;; Treesit 配置

(require-package 'tree-sitter)
(require-package 'tree-sitter-langs)
(add-hook 'after-init-hook 'global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode)
(with-eval-after-load 'tree-sitter
  (tree-sitter-load 'elisp "elisp")
  (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp)))

;; (setq treesit-language-source-alist
;;       '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash.git"))
;;         (c          . ("https://github.com/tree-sitter/tree-sitter-c.git"))
;;         (cmake      . ("https://github.com/uyha/tree-sitter-cmake.git"))
;;         (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp.git"))
;;         (css        . ("https://github.com/tree-sitter/tree-sitter-css.git"))
;;         (html       . ("https://github.com/tree-sitter/tree-sitter-html.git"))
;;         (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript.git"))
;;         (json       . ("https://github.com/tree-sitter/tree-sitter-json.git"))
;;         (python     . ("https://github.com/tree-sitter/tree-sitter-python.git"))
;;         (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "typescript/src"))
;;         (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml.git"))
;;         (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))))
;; (setq treesit-auto--language-source-alist treesit-language-source-alist)

;; (install-vc-package "https://github.com/renzmann/treesit-auto" "treesit-auto")
;; (require 'treesit-auto)
;; (with-eval-after-load 'treesit-auto
;;   (treesit-auto-apply-remap))



;; 一键运行

(install-vc-package "https://github.com/honmaple/emacs-maple-run" "maple-run")
(require 'maple-run)
(add-to-list 'maple-run:alist '(c++-mode :command "g++ %F -o /tmp/%b && /tmp/%b"))
(add-to-list 'maple-run:alist '((html-mode web-mode) :command browse-url-of-file))
(global-set-key (kbd "C-c X") 'maple-run)


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
