;;; my-odin.el --- A simple hello world package
;;; Commentary:
;; A basic example package
(use-package odin-mode
  :vc (:url "https://github.com/mattt-b/odin-mode" :rev :newest)
  :after eglot
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

