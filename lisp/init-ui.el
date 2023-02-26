;;; init-ui.el --- 为 Emacs 提供ui方面的美化
;;; Commentary:
;;; Code:

;; 标签栏增强
(install-vc-package "https://github.com/manateelazycat/sort-tab" "sort-tab")
(defun require-sort-tab ()
  (interactive)
  (if (not (fboundp 'sort-tab-mode))
      (require 'sort-tab)))

(add-hook 'after-make-window-system-frame-hooks
          (lambda ()
            (run-with-idle-timer 0.1 nil
                                 'require-sort-tab)))

(with-eval-after-load 'sort-tab
  (sort-tab-mode 1)

  (setq sort-tab-show-index-number t)
  (global-set-key (kbd "C-1") 'sort-tab-select-visible-tab)
  (global-set-key (kbd "C-2") 'sort-tab-select-visible-tab)
  (global-set-key (kbd "C-3") 'sort-tab-select-visible-tab)
  (global-set-key (kbd "C-4") 'sort-tab-select-visible-tab)
  (global-set-key (kbd "C-5") 'sort-tab-select-visible-tab)
  (global-set-key (kbd "C-6") 'sort-tab-select-visible-tab)
  (global-set-key (kbd "C-7") 'sort-tab-select-visible-tab)
  (global-set-key (kbd "C-8") 'sort-tab-select-visible-tab)
  (global-set-key (kbd "C-9") 'sort-tab-select-visible-tab)
  (global-set-key (kbd "C-0") 'sort-tab-select-visible-tab)
  (global-set-key (kbd "C-S-q") 'sort-tab-close-all-tabs)
  (global-set-key (kbd "C-s-q") 'sort-tab-close-mode-tabs)
  (global-set-key (kbd "C-s-n") 'sort-tab-select-next-tab)
  (global-set-key (kbd "C-s-p") 'sort-tab-select-prev-tab)
  (global-set-key (kbd "C-;") 'sort-tab-close-current-tab))




;; 更漂亮的 modeline
(when (maybe-require-package 'doom-modeline)
 (add-hook 'after-init-hook #'doom-modeline-mode)
 (setq doom-modeline-support-imenu t
       doom-modeline-buffer-file-name-style 'auto
       doom-modeline-icon t
       doom-modeline-major-mode-icon t
       doom-modeline-major-mode-color-icon t
       doom-modeline-github t
       doom-modeline-time t
       doom-modeline-height 40))
(if (facep 'mode-line-active)
  (set-face-attribute 'mode-line nil :family "Source Code Pro" :height 100))
(set-face-attribute 'mode-line-inactive nil :family "Source Code Pro" :height 100)



;; Org-mode 美化
(when (maybe-require-package 'org-modern)
 (add-hook 'org-mode-hook 'org-modern-mode))

(with-eval-after-load 'org
  (set-face-attribute 'org-level-1 nil :height 1.6 :bold t)
  (set-face-attribute 'org-level-2 nil :height 1.4 :bold t)
  (set-face-attribute 'org-level-3 nil :height 1.2 :bold t))

(when (maybe-require-package 'valign)
  (add-hook 'after-make-window-system-frame-hooks
            (lambda () (add-hook 'org-mode-hook #'valign-mode))))



;; 区分活动与非活动窗口

(require-package 'solaire-mode)

;; 修复 `solaire-mode' 在emacsclient中无法启用的问题
;; See here: `https://github.com/hlissner/emacs-solaire-mode/issues/46'

(defun load-solaire-mode ()
  (interactive)
  (if (not solaire-global-mode)
      (with-no-warnings
        (dolist (theme custom-enabled-themes)
          (load-theme theme))
        (solaire-global-mode +1))))
(add-hook 'after-make-window-system-frame-hooks
          (lambda ()
            (run-with-idle-timer 0.01 nil
                                 'load-solaire-mode)))


(provide 'init-ui)
;;; init-ui.el ends here
