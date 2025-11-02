;;; my-odin.el --- A simple hello world package
;;; Commentary:
;; A basic example package

(defun odin-test-at-point ()
  "Run odin test command with current buffer's folder. If word under cursor exists, add it as test name."
  (interactive)
  (let ((word (thing-at-point 'word t))
        (folder (file-name-directory (buffer-file-name))))
    (if folder
        (let ((command (if word
                           (format "odin test %s -define:ODIN_TEST_NAMES=%s" folder word)
                         (format "odin test %s" folder))))
          (compile command))
      (message "Buffer is not associated with a file"))))

(use-package odin-mode
  :vc (:url "https://github.com/mattt-b/odin-mode" :rev :newest)
  :after eglot
  :bind (("C-c t" . odin-test-at-point))
  :hook ((odin-mode) . eglot-ensure)
  ((odin-mode) . (lambda ()
		   (setq tab-width 4
			 indent-tabs-mode t)
		   (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  :config
  (add-to-list 'compilation-error-regexp-alist-alist
               '(odin-test
		 "^\\[ERROR\\].*\\[\\([^:]+\\):\\([0-9]+\\):"
		 1 2 nil 2 1))
  (add-to-list 'compilation-error-regexp-alist 'odin-test)
  :mode ("\\.odin\\'" . odin-mode))

(provide 'my-odin)
;;; my-odin.el ends here

