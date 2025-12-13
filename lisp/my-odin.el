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

(defun odin-check-module ()
  "Run odin check command with current buffer's folder."
  (interactive)
  (let ((folder (file-name-directory (buffer-file-name))))
    (if folder
        (let ((command (format "odin check %s" folder)))
          (compile command))
      (message "Buffer is not associated with a file"))))

(defun odin-run-module ()
  "Run odin run command with current buffer's folder."
  (interactive)
  (let ((folder (file-name-directory (buffer-file-name))))
    (if folder
        (let ((command (format "odin run %s" folder)))
          (compile command))
      (message "Buffer is not associated with a file"))))

(use-package odin-ts-mode
  :vc (:url "https://github.com/Sampie159/odin-ts-mode" :rev :newest)
  :after apheleia
  :bind (:map odin-ts-mode-map
	      ("C-c C-c t" . odin-test-at-point)
	      ("C-c C-c r" . odin-run-module)
	      ("C-c C-c c" . odin-check-module))
  :hook ((odin-ts-mode) . eglot-ensure)
  ((odin-ts-mode) . (lambda ()
		      (setq tab-width 4
			    indent-tabs-mode t)))
  :mode ("\\.odin\\'" . odin-ts-mode)
  :config
  (add-to-list 'treesit-language-source-alist
               '(odin "https://github.com/tree-sitter-grammars/tree-sitter-odin"))
  (add-to-list 'apheleia-formatters
	       '(odinfmt . ("odinfmt" "-stdin")))
  (add-to-list 'apheleia-mode-alist
	       '(odin-ts-mode . odinfmt))
  ;; (add-to-list 'compilation-error-regexp-alist-alist
  ;;              '(odin-test
  ;; 		 "^\\[ERROR\\].*\\[\\([^:]+\\):\\([0-9]+\\):"
  ;; 		 1 2 nil 2 1))
  ;; (add-to-list 'compilation-error-regexp-alist 'odin-test)
  
  )

(provide 'my-odin)
;;; my-odin.el ends here

