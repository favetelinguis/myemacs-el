;;; my-zig.el --- A simple hello world package

;;; Commentary:
;; A basic example package

;;; Code:
(use-package zig-mode
  :hook ((zig-mode) . eglot-ensure)
  :config
  (setq zig-format-on-save nil) ; rely on :editor format instead
  :vc (:url "https://github.com/ziglang/zig-mode" :branch "master")
  :mode "\\.zig\\'")

(provide 'my-zig)
;;; my-zig.el ends here

