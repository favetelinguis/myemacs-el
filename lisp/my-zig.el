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

(defun my/zig-test-file-local ()
  "Insert file-local variable to set compile command for zig test on current file."
  (interactive)
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (comment-start (or comment-start "//"))
         (comment-end (or comment-end "")))
    (save-excursion
      (goto-char (point-min))
      (insert (format "%s -*- compile-command: \"zig test src/%s\" -*- %s\n"
                      comment-start filename comment-end)))))

(provide 'my-zig)
;;; my-zig.el ends here

