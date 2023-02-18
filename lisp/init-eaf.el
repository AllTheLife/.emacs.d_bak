;;; init-eaf.el --- 生活在Emacs里 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(install-vc-package "https://github.com/emacs-eaf/emacs-application-framework" "emacs-application-framework")
(run-with-idle-timer 3 t (lambda () (require 'eaf)))

(with-eval-after-load 'eaf
  (require 'eaf-browser)
  (require 'eaf-image-viewer)
  (require 'eaf-org-previewer)
  (require 'eaf-pdf-viewer)
  (require 'eaf-system-monitor)
  (require 'eaf-file-manager)
  (require 'eaf-all-the-icons)

  (setq eaf-browser-continue-where-left-off t
        eaf-browser-enable-adblocker t
        browse-url-browser-function 'eaf-open-browser
        eaf-pdf-dark-mode nil)
  (defalias 'browse-web #'eaf-open-browser)

  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)

  ;; 添加EAF对dwm的支持。
  ;; See here: `https://github.com/emacs-eaf/emacs-application-framework/issues/237'
  (add-to-list 'eaf-wm-focus-fix-wms "dwm"))




(provide 'init-eaf)
;;; init-eaf.el ends here.
