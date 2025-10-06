;;; my-zig.el --- A simple hello world package

;;; Commentary:
;; A basic example package

;;; Code:
(use-package zig-mode
  :hook ((zig-mode) . eglot-ensure)
  :bind (:map zig-mode-map
	      ("C-c C-c t" . my/zig-build-test)
	      ("C-c C-c l" . my/zig-test-file-local)
	      ("C-c C-c r" . my/zig-build-test-filter-at-point)
	      ("C-c C-c f" . my/zig-test-filter-at-point))
  :config
  (setq zig-format-on-save nil) ; rely on :editor format instead
  :vc (:url "https://github.com/ziglang/zig-mode" :branch "master")
  :mode "\\.zig\\'")

(defun my/zig-test-file-local ()
  "Set compile command for zig test on current file and trigger compilation."
  (interactive)
  (let* ((project-root (or (project-root (project-current))
                           default-directory))
         (relative-path (file-relative-name (buffer-file-name) project-root)))
    (setq compile-command (format "zig test %s" relative-path))
    (let ((default-directory project-root))
      (compile compile-command))))

(defun my/zig-test-filter-at-point ()
  "Run zig test with filter using quoted string at point."
  (interactive)
  (let* ((project-root (or (project-root (project-current))
                           default-directory))
         (relative-path (file-relative-name (buffer-file-name) project-root))
         (quoted-string (thing-at-point 'string t)))
    (if quoted-string
	(progn
          (setq compile-command 
                (format "zig test %s --test-filter %s" relative-path quoted-string))
	  (let  ((default-directory project-root))
	    (compile compile-command)))
      (message "No quoted string found at point"))))

;; I can pass in multiple test-filter might want to extend this to that?
;; zig build test -Dtest-filter="parse ls output" -Dtest-filter="next test"
(defun my/zig-build-test-filter-at-point ()
  "Run zig build test with filter using quoted string at point."
  (interactive)
  (let* ((project-root (or (project-root (project-current))
                           default-directory))
         (relative-path (file-relative-name (buffer-file-name) project-root))
         (quoted-string (thing-at-point 'string t)))
    (if quoted-string
	(progn
          (setq compile-command 
                (format "zig build test --summary new -Dtest-filter=%s" quoted-string))
	  (let  ((default-directory project-root))
	    (compile compile-command)))
      (message "No quoted string found at point"))))

(defun my/zig-build-test ()
  "Set compile command to 'zig build test' and run compilation."
  (interactive)
  (let ((project-root (or (project-root (project-current))
			  default-directory)))
    (setq compile-command "zig build test")
    (let  ((default-directory project-root))
      (compile compile-command))))

(provide 'my-zig)
;;; my-zig.el ends here

