;;; my-ts.el --- A simple hello world package

;;; Commentary:
;; A basic example package

;;; Code:
(use-package typescript-mode
  :ensure t
  :hook ((typescript-mode) . eglot-ensure)
  :config
  (setq typescript-indent-level 2)
  :mode "\\.ts\\'")

(use-package js-mode
  :hook ((js-mode) . eglot-ensure)
  :config
  (setq js-basic-offset 2)
  (setq js-indent-level 2)
  :mode "\\.js\\'")

(use-package json-mode
  :mode "\\.json\\'")

(provide 'my-ts)
;;; my-ts.el ends here

