;;; init-dired.el --- Dired 配置 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(when (maybe-require-package 'dirvish)
 (add-hook 'after-init-hook 'dirvish-override-dired-mode)
 (setq dirvish-attributes '(subtree-state all-the-icons git-msg file-size)))

(with-eval-after-load 'dirvish
  (define-key dirvish-mode-map (kbd "a") 'dirvish-quick-access)
  (define-key dirvish-mode-map (kbd "f") 'dirvish-file-info-menu)
  (define-key dirvish-mode-map (kbd "y") 'dirvish-yank-menu)
  (define-key dirvish-mode-map (kbd "N") 'dirvish-narrow)
  (define-key dirvish-mode-map (kbd "^") 'dirvish-history-last)
  (define-key dirvish-mode-map (kbd "h") 'dirvish-history-jump)
  (define-key dirvish-mode-map (kbd "s") 'dirvish-quicksort)
  (define-key dirvish-mode-map (kbd "v") 'dirvish-vc-menu)
  (define-key dirvish-mode-map (kbd "TAB") 'dirvish-subtree-toggle)
  (define-key dirvish-mode-map (kbd "M-f") 'dirvish-history-go-forward)
  (define-key dirvish-mode-map (kbd "M-b") 'dirvish-history-go-backward)
  (define-key dirvish-mode-map (kbd "M-l") 'dirvish-ls-switches-menu)
  (define-key dirvish-mode-map (kbd "M-m") 'dirvish-mark-menu)
  (define-key dirvish-mode-map (kbd "M-t") 'dirvish-layout-toggle)
  (define-key dirvish-mode-map (kbd "M-s") 'dirvish-setup-menu)
  (define-key dirvish-mode-map (kbd "M-e") 'dirvish-emerge-menu)
  (define-key dirvish-mode-map (kbd "M-j") 'dirvish-fd-jump)

  (global-set-key (kbd "C-c d") 'dirvish-side))

(provide 'init-dired)
;;; init-dired.el ends here
