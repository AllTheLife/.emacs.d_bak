;;; init-ctags.el --- Tags 集成 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'citre)
(setq citre-default-create-tags-file-location 'global-cache)
(setq citre-use-project-root-when-creating-tags t)
(setq citre-prompt-language-for-ctags-command t)
(setq citre-enable-capf-integration t)
(require 'citre-config)

(add-hook 'c++-mode-hook 'citre-mode)

(require-package 'ggtags)


(provide 'init-ctags)
;;; init-ctags.el ends here
