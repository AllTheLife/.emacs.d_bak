;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)
(setq package-check-signature nil)

;; (setq inhibit-automatic-native-compilation t)
;; (setq package-native-compile t)
;; (native-compile-async "~/.emacs.d/site-lisp" 'recursively)

(provide 'early-init)

;;; early-init.el ends here.
