;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'vertico)
  (add-hook 'after-init-hook 'vertico-mode))

(require-package 'consult)
(global-set-key [remap switch-to-buffer] 'consult-buffer)
(global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
(global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
(global-set-key [remap goto-line] 'consult-goto-line)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "M-y") 'consult-yank-pop)

(when (and (fboundp 'vertico-mode) (fboundp 'consult-completion-in-region))
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

(require-package 'marginalia)
(add-hook 'after-init-hook 'marginalia-mode)

(require-package 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(when (display-graphic-p)
  (require-package 'vertico-posframe)
  (add-hook 'after-init-hook 'vertico-posframe-mode))

(require-package 'embark)
(global-set-key (kbd "C-c C-e") 'embark-act)

(when (and (fboundp 'vertico-mode) (fboundp 'embark-act))
  (require-package 'embark-consult))


(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
