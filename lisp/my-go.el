;;; my-go.el --- A simple hello world package

;;; Commentary:
;; A basic example package

;;; Code:
(use-package go-mode
  :ensure t
  :hook ((go-mode) . eglot-ensure))

(provide 'my-go)
;;; my-go.el ends here

