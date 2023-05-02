;;; init-bridge.el --- 建立与外部环境通信的渠道 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; 建立Deno运行环境
(require-package 'websocket)

(install-vc-package "https://github.com/manateelazycat/deno-bridge" "deno-bridge")
(run-with-idle-timer 3 nil (lambda () (require 'deno-bridge)))



;; (install-vc-package "https://github.com/ginqi7/websocket-bridge" "websocket-bridge")
;; (run-with-idle-timer 1 nil (lambda () (require 'websocket-bridge)))


(provide 'init-bridge)
