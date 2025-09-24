;;; my-rust.el --- A simple hello world package

;;; Commentary:
;; A basic example package

;;; Code:

(use-package rust-mode
  :ensure t
  :hook ((rust-mode) . eglot-ensure))

(provide 'my-rust)
;;; my-rust.el ends here

