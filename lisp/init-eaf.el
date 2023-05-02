;;; init-eaf.el --- 生活在Emacs里 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(install-vc-package "https://github.com/emacs-eaf/emacs-application-framework" "emacs-application-framework")
(add-hook 'after-make-window-system-frame-hooks
          (lambda () (run-with-idle-timer 5 nil
                                          (lambda () (require 'eaf)))))

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

  ;; 为Colemak键盘布局提供特殊的键位
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_up "n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "e" eaf-pdf-viewer-keybinding)

  (eaf-bind-key js_select_next_file "n" eaf-file-manager-keybinding)
  (eaf-bind-key js_select_prev_file "e" eaf-file-manager-keybinding)
  (eaf-bind-key batch_rename "k" eaf-file-manager-keybinding)
  (eaf-bind-key new_file "j" eaf-file-manager-keybinding)

  (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  (eaf-bind-key insert_or_scroll_up "n" eaf-browser-keybinding)
  (eaf-bind-key insert_or_scroll_down "e" eaf-browser-keybinding)
  (eaf-bind-key insert_or_select_left_tab "N" eaf-browser-keybinding)
  (eaf-bind-key insert_or_select_right_tab "E" eaf-browser-keybinding)
  (eaf-bind-key insert_or_scroll_right "i" eaf-browser-keybinding)
  (eaf-bind-key insert_or_history_forward "I" eaf-browser-keybinding)
  (eaf-bind-key insert_or_focus_input "l" eaf-browser-keybinding)
  (eaf-bind-key insert_or_open_downloads_setting "L" eaf-browser-keybinding)
  (eaf-bind-key insert_or_export_text "j" eaf-browser-keybinding)
  (eaf-bind-key insert_or_render_by_eww "J" eaf-browser-keybinding)
  (eaf-bind-key insert_or_edit_url "k" eaf-browser-keybinding)

  ;; 添加EAF对dwm的支持。
  ;; See here: `https://github.com/emacs-eaf/emacs-application-framework/issues/237'
  (add-to-list 'eaf-wm-focus-fix-wms "dwm")
  )


(provide 'init-eaf)
;;; init-eaf.el ends here.
