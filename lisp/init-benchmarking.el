;;; init-benchmarking.el --- 测量启动和加载时间 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun was/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defvar was/require-times nil
  "一个 (FEATURE LOAD-START-TIME LOAD-DURATION) 形式的列表.
LOAD-DURATION 是 FEATURE加载时花费的时间(以毫秒为单位).")

(defun was/require-times-wrapper (orig feature &rest args)
  "记录 `require-times' 中每个 FEATURE 加载耗费的时间."
  (let* ((already-loaded (memq feature features))
	 (require-start-time (and (not already-loaded) (current-time))))
    (prog1
	(apply orig feature args)
      (when (and (not already-loaded) (memq feature features))
	(let ((time (was/time-subtract-millis (current-time) require-start-time)))
	  (add-to-list 'was/require-times
		       (list feature require-start-time time)
		       t))))))

(advice-add 'require :around 'was/require-times-wrapper)




(define-derived-mode was/require-times-mode tabulated-list-mode "Require-Times"
  "展示 `require' 包所使用的时间."
  (setq tabulated-list-format
        [("Start time (ms)" 20 was/require-times-sort-by-start-time-pred)
         ("Feature" 30 t)
         ("Time (ms)" 12 was/require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  (setq tabulated-list-entries #'was/require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

(defun was/require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun was/require-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun was/require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in was/require-times
           with order = 0
           do (cl-incf order)
           collect (list order
                         (vector
                          (format "%.3f" (was/time-subtract-millis start-time before-init-time))
                          (symbol-name feature)
                          (format "%.3f" millis)))))

(defun was/require-times ()
  "以表格的形式显示列表 `was/require-times'."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (was/require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))




(defun was/show-init-time ()
  (message "init completed in %.2fms"
           (was/time-subtract-millis after-init-time before-init-time)))

(add-hook 'after-init-hook 'was/show-init-time)


(provide 'init-benchmarking)
;;; init-benchmarking.el ends here
