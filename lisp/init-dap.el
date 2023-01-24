;;; init-dap.el --- Initialize debugging configurations
;;; Commentary:
;;; Code:

(require-package 'dap-mode)

(setq dap-auto-configure-mode t
      dap-auto-configure-features
      '(sessions locals breakpoints expressions tooltip))

(require 'dap-lldb)
