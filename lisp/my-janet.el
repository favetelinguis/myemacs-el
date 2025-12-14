;;; my-janet.el --- A simple hello world package

;;; Commentary:
;; A basic example package

;;; Code:
;; (defun ajrepl-eval-defun ()
;;   "Evaluate the current top-level form."
;;   (interactive)
;;   (save-excursion
;;     (let (start end)
;;       (treesit-beginning-of-defun)
;;       (setq start (point))
;;       (treesit-end-of-defun)
;;       (setq end (point))
;;       (ajrepl-send-region start end))))

(defun my-janet-setup-completion ()
  "Setup buffer-based completion for janet-ts-mode."
  (setq-local completion-at-point-functions
              (list #'cape-dabbrev)))

(defun run-judge-at-point ()
  "Run judge with current buffer's file path and cursor position."
  (interactive)
  (let* ((file (buffer-file-name))
         (line (line-number-at-pos))
         (col (current-column))
         (target (format "%s:%d:%d" file line col)))
    (if file
        (progn
          (when (buffer-modified-p)
            (save-buffer))
          (compile (format "judge -a %s" target)))
      (error "Buffer is not visiting a file"))))

(use-package ajrepl
  :vc (:url "https://github.com/sogaiu/ajrepl"
	    :rev :newest)
  :config
  (add-hook 'janet-ts-mode-hook
	    #'my-janet-setup-completion)
  (add-hook 'janet-ts-mode-hook
            #'ajrepl-interaction-mode))

(use-package janet-ts-mode
  :vc (:url "https://github.com/sogaiu/janet-ts-mode"
	    :rev :newest)
  :bind (:map janet-ts-mode-map
	      ("C-c C-c" . run-judge-at-point))
  :hook
  (janet-ts-mode . (lambda () (electric-pair-local-mode -1)))
  :config
  (add-to-list 'apheleia-formatters
               '(janet-fmt . ("janet" "-e" "(import spork/fmt) (fmt/format-print (file/read stdin :all))")))
  (add-to-list 'apheleia-mode-alist
               '(janet-ts-mode . janet-fmt))
  (setq treesit-language-source-alist
	(if (eq 'windows-nt system-type)
            '((janet-simple
               . ("https://github.com/sogaiu/tree-sitter-janet-simple"
                  nil nil "gcc.exe")))
          '((janet-simple
             . ("https://github.com/sogaiu/tree-sitter-janet-simple")))))

  (when (not (treesit-language-available-p 'janet-simple))
    (treesit-install-language-grammar 'janet-simple)))

(provide 'my-janet)
;;; my-janet.el ends here

