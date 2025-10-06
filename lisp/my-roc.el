;;; my-roc.el --- A simple hello world package

;;; Commentary:
;; A basic example package

;;; Code:

;; M-x treesit-install-language-grammar
;; there is a roc-ts-install-treesitter-grammar

(use-package roc-ts-mode
  :ensure t
  :mode ("\\.roc\\'" . roc-ts-mode)
  :hook ((roc-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(roc-ts-mode . ("roc_language_server"))))

(provide 'my-roc)
;;; my-roc.el ends here

