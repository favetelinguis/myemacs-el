(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  ;;TODO not sure this is working prob need to fix
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("dired" (mode . dired-mode))
           ("org" (mode . org-mode))
           ("programming" (or
                          (mode . python-mode)
                          (mode . emacs-lisp-mode)))
           ("emacs" (or
                    (name . "^\\*scratch\\*$")
                    (name . "^\\*Messages\\*$"))))))
  (add-hook 'ibuffer-mode-hook
            (lambda () (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package which-key
  :custom
  (which-key-mode 1))

;; do i really need this when i have sessions with tabstops package
;; (use-package desktop
;;   :ensure nil  ; built-in package
;;   :config
;;   (desktop-save-mode 1)
;;   :custom
;;   (desktop-dirname user-emacs-directory)
;;   (desktop-base-file-name "desktop")
;;   (desktop-base-lock-name "desktop.lock")
;;   (desktop-path (list user-emacs-directory))
;;   (desktop-save t)
;;   (desktop-auto-save-timeout 30)
;;   (desktop-load-locked-desktop t))

(use-package ediff
  :ensure nil
  :config
  ;; dont open external frame with ediff
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package emacs
  :ensure nil
  :bind
  (:map global-map
	("M-o" . other-window)
("C-x k" . kill-current-buffer))
  :config
  ;; dissable creating lock files, i can now edit the same file from multiple emacs instances which can be bad
  (setq create-lockfiles nil)
  ;; I dont really know what this does i got it from
  ;;https://blog.chmouel.com/posts/emacs-isearch/
  ;; I use it to get occur selected from isearch
  ;; i have replaced this with popper or?
;;   (defun my-select-window (window &rest _)
;;     "Select WINDOW for display-buffer-alist"
;;     (select-window window))
;;   (setq display-buffer-alist
;; '(((or . ((derived-mode . occur-mode)))
;;            (display-buffer-reuse-mode-window display-buffer-at-bottom)
;;            (body-function . my-select-window)
;;            (dedicated . t)
;;            (preserve-size . (t . t)))))

  (setq ring-bell-function 'ignore)
  ;; allow all disabled commands without prompting
  (setq disabled-command-function nil)
  ;;This tells Emacs to write all customizations (from `M-x customize` interface) to `~/.emacs.d/custom.el` instead of appending them to your `init.el`.
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file 'noerror)
  ;; Use ripgrep for project search ripgrep
  (setq xref-search-program 'ripgrep)
  (setq-default line-spacing 0.2)
  (set-face-attribute 'default nil
                      :font "JetBrains Mono-13"
                      :weight 'normal
                      :width 'normal)
  :custom
 
  ;; Curfu
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;; (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Vertico
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :config
  (tool-bar-mode -1)      ;; Disable toolbar
  (menu-bar-mode -1)      ;; Disable menu bar
  (scroll-bar-mode -1)    ;; Disable scroll bar
  (tooltip-mode -1)      ;; Disable tooltips
  ;; (desktop-save-mode 1)
  ;; Disable initial scratch message
  (setq initial-scratch-message nil)
  ;; No blinking cursor
  (blink-cursor-mode -1)
  (setq inhibit-startup-message t)
  ;; Put auto-save files in a dedicated directory
  (setq auto-save-file-name-transforms
`((".*" ,(concat user-emacs-directory "auto-save/") t)))

  ;; Create the directory if it doesn't exist
  (make-directory (concat user-emacs-directory "auto-save/") t)

  ;; Put backup files in a dedicated directory
  (setq backup-directory-alist
`((".*" . ,(concat user-emacs-directory "backup/"))))

  (make-directory (concat user-emacs-directory "backup/") t)
  )

(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))

(use-package corfu
  :ensure t
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
  )

;; Enable Vertico.

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package markdown-mode
  :ensure t)

(use-package git-timemachine
  :ensure t
  :bind ("C-x v t" . git-timemachine)
  :config
  ;; Show abbreviated commit hash in header line
  (setq git-timemachine-show-minibuffer-details t)
  ;; Automatically kill timemachine buffer when quitting
  (setq git-timemachine-quit-to-invoking-buffer t))

(use-package gptel
  :ensure t
;;   :hook ((gptel-post-stream . gptel-auto-scroll)
;; (gptel-post-response-functions . gptel-end-of-response))
  :bind
  ( :map global-map
    ("C-c q r" . gptel-rewrite)
    ("C-c q m" . gptel-menu)
    ("C-c q a" . gptel-add)
    ("C-c q c" . gptel-context-remove-all)
    ("C-c q f" . gptel-add-file)
    ("C-c q q" . gptel-send-with-options)
    ("C-c q Q" . gptel-send))
  :config
  (defun gptel-send-with-options (&optional arg)
  "Send query.  With prefix ARG open gptel's menu instead. in gptel menu select options and save with c-x c-s"
  (interactive "P")
  (if arg
      (call-interactively 'gptel-menu)
    (gptel--suffix-send (transient-args 'gptel-menu))))
   
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model 'claude-sonnet-4-20250514
	gptel-backend (gptel-make-anthropic "AICHAT"
			:stream t
			:models '(claude-sonnet-4-20250514)
			:key (getenv "ANTHROPIC_API_KEY"))))

(use-package x509-mode
  :ensure t)

(use-package jwt
  :ensure t
  :commands (jwt-decode jwt-encode))

;; LSP support
(use-package eglot
  :ensure nil
  :hook ((typescript-mode js-mode) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil))

;; Used to get direnv working when launching in sway
(use-package exec-path-from-shell
  :ensure t
  :config
  ;; set all variables that should be copied
  (dolist (var '("ANTHROPIC_API_KEY"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;;---COMPLETION---start
(use-package vertico
  :ensure t
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))
;;---COMPLETION---end

;;---NOTE TAKING---start
(use-package org
  :ensure t
  :hook (org-mode . visual-line-mode)
  :config
  ;; Set default directory for org files
  (setq org-directory "~/org-notes")
  (setq org-default-todo-file (expand-file-name "tasks.org" org-directory))
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory))

  ;; Agenda files location
  (setq org-agenda-files (list org-directory))
  
  ;; Create directory if it doesn't exist
  (unless (file-exists-p org-directory)
    (make-directory org-directory t))
  
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline org-default-todo-file "Tasks")
           "* TODO %?\n  %i\n")
	  ("n" "Note" entry (file+headline org-default-notes-file "Notes")
           "* %?\nEntered on %U  %i\n")))
  ;; Basic keybindings
  :bind
  ("C-c n a" . org-agenda)
  ("C-c n t" . (lambda () (interactive) (org-capture nil "t"))) ; will trigger the capture for t
  ("C-c n n" . (lambda () (interactive) (org-capture nil "n"))) ; will trigger the capture for t
  ("C-c n T" . org-todo-list)
  ("C-c n c" . org-capture))
;;---NOTE TAKING---end

(use-package cider
  :ensure t
  :config
  (setq cider-repl-pop-to-buffer-on-connect nil))
;;---PROGRAMMING---end

(use-package meow
  :ensure t
  :init
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    ;; when i use 1.6 i should use this
    ;;(meow-motion-define-key
    (meow-motion-overwrite-define-key
     ;; '("j" . meow-next)
     ;; '("k" . meow-prev)
     '("<escape>" . ignore)
     )
    (meow-leader-define-key
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("j" . tabspaces-open-or-create-project-and-workspace)
     '("b" . tabspaces-switch-buffer-and-tab)
     `("p" . ,project-prefix-map)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet)
     )
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '(":" . meow-goto-line)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit) ; i want to only use isearch
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("SPC" . meow-keypad) ; meow-keypad original setting or ignore to turn off
     '("<escape>" . ignore)))
  :custom
  (meow-use-clipboard t)
  :config
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))
  (meow-global-mode 1)
  (meow-setup))

;; Extend isearch with commands
(use-package isearch
  :ensure nil
  :defer t
  :config
  (defun my-occur-from-isearch ()
    (interactive)
    (let ((query (if isearch-regexp
    isearch-string
  (regexp-quote isearch-string))))
      (isearch-update-ring isearch-string isearch-regexp)
      (let (search-nonincremental-instead)
        (ignore-errors (isearch-done t t)))
      (occur query)))
  (defun my-project-search-from-isearch ()
    (interactive)
    (let ((query (if isearch-regexp
    isearch-string
  (regexp-quote isearch-string))))
      (isearch-update-ring isearch-string isearch-regexp)
      (let (search-nonincremental-instead)
        (ignore-errors (isearch-done t t)))
      (project-find-regexp query)))
 
      :bind
  (:map isearch-mode-map
   ("C-o" . my-occur-from-isearch)
   ("C-g" . my-project-search-from-isearch)
   ("C-d" . isearch-forward-symbol-at-point)))

(use-package tabspaces
  :ensure t
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup. 
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-todo-file-name "project-todo.org")
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  (tab-bar-new-tab-choice "*scratch*"))

(use-package justl
  :ensure t
  ;; :config
  ;; (setq justl-per-recipe-buffer t)
  ;; bind to r as in run
  :bind (("C-c r m" . justl)
	 ("C-c r d" . justl-exec-default-recipe)
	 ("C-c r r" . justl-exec-recipe-in-dir)))

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-group-function #'popper-group-by-project) 
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
	  "*vc-git.*\\*$"
	  "*just.*\\*$"
          "\\*AICHAT\\*"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints
