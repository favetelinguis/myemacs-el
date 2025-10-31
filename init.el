(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;; for the compilation buffer to support colors
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

;; for compilation will remove the osc stuff making odin test better
(use-package ansi-osc
  :ensure t
  :hook (compilation-filter . ansi-osc-compilation-filter))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config (setq ibuffer-expert t))

(use-package which-key
  :custom
  (which-key-mode 1))

(use-package ediff
  :ensure nil
  :config
  ;; dont open external frame with ediff
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :config
  (setq 
   aw-ignore-current t
   aw-ignore-on t
   aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package emacs
  :ensure nil
  :hook
  (prog-mode . (lambda () (setq truncate-lines t))); prevent long line warpping in prog modes
  ;; Disable electric-pair in Lisp modes
  (emacs-lisp-mode . (lambda () (electric-pair-local-mode -1)))
  (clojure-mode . (lambda () (electric-pair-local-mode -1)))
  (lisp-mode . (lambda () (electric-pair-local-mode -1)))
  :bind
  (:map global-map
	("C-c c" . project-recompile)
	;; ("M-o" . other-window)
	;; ("C-c o" . find-file-at-point) ;; redundant use embark
	("C-c p" . my/switch-to-bb-playground)
	("C-x k" . kill-current-buffer))
  :config
  (winner-mode 1) ; use C-c left/right to go over layouts
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  (setq scroll-margin 5)
  (setq compilation-always-kill t) ;; make rerunning compilation buffer better, i dont get asked each time to quit process between runs
  (setq set-mark-command-repeat-pop t)
  ;; start window management
  (setq switch-to-buffer-obey-display-actions t
	switch-to-buffer-in-dedicated-window 'pop)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (electric-pair-mode 1)
  (global-subword-mode -1)
  (global-superword-mode 1)
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
  :after orderless
  :custom
  ;; Make the popup appear quicker
  (corfu-popupinfo-delay '(0.5 . 0.5))
  ;; Always have the same width
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  ;; Have Corfu wrap around when going up
  (corfu-cycle t)
  (corfu-preselect-first t)
  :config
  ;; TODO move to :bind
  (define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down) ;; corfu-next
  (define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up)  ;; corfu-previous
  (setq corfu-auto t
	corfu-quit-no-match 'separator) 
  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  (corfu-history-mode)
  (corfu-popupinfo-mode))
;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
(use-package corfu
  :init
  (global-corfu-mode))

;; Add extensions
(use-package cape
  :ensure t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("M-<tab>" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  ;; (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )

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

(use-package flymake
  :ensure nil
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)
         ("C-c f l" . flymake-show-buffer-diagnostics)
         ("C-c f p" . flymake-show-project-diagnostics)))

(use-package gptel
  :ensure t
  ;;   :hook ((gptel-post-stream . gptel-auto-scroll)
  ;; (gptel-post-response-functions . gptel-end-of-response))
  :bind
  ( :map global-map
    ("C-c f r" . gptel-rewrite)
    ("C-c f m" . gptel-menu)
    ("C-c f a" . gptel-add)
    ("C-c f c" . gptel-context-remove-all)
    ("C-c f A" . gptel-add-file)
    ("C-c f f" . gptel-send-with-options)
    ("C-c f F" . gptel-send))
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
  ;; instead of enabling full desktop mode i just want to save register and kill-ring between restarts
  (setq savehist-additional-variables '(register-alist kill-ring))
  (savehist-mode 1))

;; used mostly since i want to save registry to get better to navigate buffers
(use-package desktop
  :ensure nil  ; built-in package
  :config
  (desktop-save-mode 1)
  :custom
  (desktop-dirname user-emacs-directory)
  (desktop-base-file-name "desktop")
  (desktop-base-lock-name "desktop.lock")
  (desktop-path (list user-emacs-directory))
  (desktop-save t)
  (desktop-auto-save-timeout 30)
  (desktop-load-locked-desktop t))

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
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-j" . consult-register-load)
         ("M-i" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-j" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
	 ("M-g a" . consult-yank-pop)
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'nil)
  (setq consult-preview-key "C-.")
  (consult-customize
   consult-theme consult-man consult-org-agenda :preview-key '(:debounce 0.2 any))
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;;  consult-theme :preview-key '(:debounce 0.2 any)
  ;;  consult-ripgrep consult-git-grep consult-grep consult-man
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-bookmark consult--source-file-register
  ;;  consult--source-recent-file consult--source-project-recent-file
  ;;  ;; :preview-key "M-."
  ;;  :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package embark-consult
  :ensure t
  :after consult)


(use-package embark
  :ensure t

  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config
  ;; Integrate Avy and Embark

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)
   ("C-c C-d" . helpful-at-point)
   ("C-h F" . helpful-function)))

;;---NOTE TAKING---start
(use-package org
  :ensure t
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-startup-indented t)
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
  ("C-c n n" . (lambda () (interactive) (org-capture nil "n")))
  ("C-c n l" . consult-org-agenda)
  ("C-c n t" . org-todo-list)
  ("C-c n c" . org-capture))
;;---NOTE TAKING---end

(use-package cider
  :ensure t
  :config
  (setq cider-repl-pop-to-buffer-on-connect nil))
;;---PROGRAMMING---end
(use-package isearch
  :ensure nil
  :bind
  ("C-M-s" . isearch-forward-other-window)
  ("C-M-r" . isearch-backward-other-window)
  :config
  (defun isearch-forward-other-window (prefix)
    "Function to isearch-forward in other-window."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
        (let ((next (if prefix -1 1)))
          (other-window next)
          (isearch-forward)
          (other-window (- next))))))

  (defun isearch-backward-other-window (prefix)
    "Function to isearch-backward in other-window."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
        (let ((next (if prefix 1 -1)))
          (other-window next)
          (isearch-backward)
          (other-window (- next))))))            )
;; redundant use embark export
;; Extend isearch with commands
;; (use-package isearch
;;   :ensure nil
;;   :defer t
;;   :config
;;   (defun my-occur-from-isearch ()
;;     (interactive)
;;     (let ((query (if isearch-regexp
;; 		     isearch-string
;; 		   (regexp-quote isearch-string))))
;;       (isearch-update-ring isearch-string isearch-regexp)
;;       (let (search-nonincremental-instead)
;;         (ignore-errors (isearch-done t t)))
;;       (occur query)))
;;   (defun my-project-search-from-isearch ()
;;     (interactive)
;;     (let ((query (if isearch-regexp
;; 		     isearch-string
;; 		   (regexp-quote isearch-string))))
;;       (isearch-update-ring isearch-string isearch-regexp)
;;       (let (search-nonincremental-instead)
;;         (ignore-errors (isearch-done t t)))
;;       (project-find-regexp query)))

;;   :bind
;;   (:map isearch-mode-map
;; 	("C-o" . my-occur-from-isearch)
;; 	("C-f" . my-project-search-from-isearch)
;; 	("C-d" . isearch-forward-symbol-at-point)))

(use-package just-mode
  :ensure t)

(use-package justl
  :ensure t
  ;; :config
  ;; (setq justl-per-recipe-buffer t)
  ;; bind to r as in run
  :bind (("C-c r m" . justl)
	 ("C-c r d" . justl-exec-default-recipe)
	 ("C-c r r" . justl-exec-recipe-in-dir)))

;; each language will setup eglot config in its module
(use-package eglot
  :ensure t
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  :bind
  (:map eglot-mode-map
	("C-c l a" . eglot-code-actions)
	("C-c l r" . eglot-rename)
	("C-c l f" . eglot-format)
	("C-c l d" . eglot-find-declaration)
	("C-c l i" . eglot-find-implementation)
	("C-c l t" . eglot-find-typeDefinition)
	("C-c l h" . eldoc)
	("C-c l s" . eglot-shutdown)
	("C-c l R" . eglot-reconnect)))

;; disable other language mode formatters and use apheleia for all formatting
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1)
  :hook
  (prog-mode . apheleia-mode))

;; redundant use M-g n/p
;; (use-package flymake
;;   :bind (:map prog-mode-map
;;               ("M-n" . flymake-goto-next-error)
;;               ("M-p" . flymake-goto-prev-error)))

(use-package git-gutter
  :ensure t
  :bind-keymap ("C-x v j" . my/git-gutter-repeat-map)
  :bind
  (:repeat-map my/git-gutter-repeat-map
	       ("n" . git-gutter:next-hunk)
	       ("p" . git-gutter:previous-hunk)
	       ("k" . git-gutter:revert-hunk)
	       ("=" . git-gutter:popup-hunk)
	       ("m" . git-gutter:mark-hunk)
	       ("s" . git-gutter:stage-hunk)
	       :exit
	       ("d" . vc-dir)
	       ("v" . vc-next-action))
  :config
  (setq git-gutter:ask-p nil)
  (global-git-gutter-mode +1))

;; For a more ergonomic Emacs and `dape' experience
(use-package repeat
  :custom
  (repeat-mode +1))

(defun my/cider-jack-in-babashka (&optional project-dir)
  "Start a utility CIDER REPL backed by Babashka, not related to a
specific project."
  (interactive)
  (when (get-buffer "*babashka-repl*")
    (kill-buffer "*babashka-repl*"))
  (when (get-buffer "*bb-playground*")
    (kill-buffer "*bb-playground*"))
  (let ((project-dir (or project-dir user-emacs-directory)))
    (nrepl-start-server-process
     project-dir
     "bb --nrepl-server 0"
     (lambda (server-buf)
       (set-process-query-on-exit-flag
        (get-buffer-process server-buf) nil)
       (cider-nrepl-connect
        (list :repl-buffer server-buf
              :repl-type 'clj
              :host (plist-get nrepl-endpoint :host)
              :port (plist-get nrepl-endpoint :port)
              :session-name "babashka"
              :repl-init-function (lambda ()
                                    (setq-local cljr-suppress-no-project-warning t
                                                cljr-suppress-middleware-warnings t
                                                process-query-on-exit-flag nil)
                                    (set-process-query-on-exit-flag
                                     (get-buffer-process (current-buffer)) nil)
                                    (rename-buffer "*babashka-repl*")
                                    ;; Create and link playground buffer
                                    (let ((playground-buffer (get-buffer-create "*bb-playground*")))
                                      (with-current-buffer playground-buffer
                                        (clojure-mode)
					(insert ";; Babashka Playground\n\n")
					(insert "(ns bb-malli\n  (:require [babashka.deps :as deps]))\n")
					(insert"(deps/add-deps '{:deps {metosin/malli {:mvn/version \"0.9.0\"}}})\n")
					(insert"(require '[malli.core :as malli])\n\n")
					(insert ";; Your code here\n")
					(goto-char (point-max)) ; Move cursor to end
                                        (sesman-link-with-buffer playground-buffer '("babashka")))
                                      (switch-to-buffer playground-buffer)))))))))

(defun my/switch-to-bb-playground ()
  "Switch to *bb-playground* buffer if it exists, otherwise start babashka REPL and switch to playground."
  (interactive)
  (if (get-buffer "*bb-playground*")
      (switch-to-buffer "*bb-playground*")
    (my/cider-jack-in-babashka)))

(use-package gud
  :ensure nil  ; built-in package
  :config
  ;; Enable mouse support in GUD buffers
  (setq gud-tooltip-mode t)
  
  ;; Show main source buffer when using GUD
  (setq gud-chdir-before-run nil)
  
  ;; Use gdb-many-windows layout by default
  (setq gdb-many-windows t)
  (setq gdb-show-main t)
  
  ;; Better variable display
  ;; (setq gdb-use-separate-io-buffer t)
  ;; (setq gdb-display-io-nopopup t)
  :bind-keymap ("C-c d d" . gud-repeat-map)  
  :bind (("C-c d g" . gdb)
         ("C-c d r" . gud-run)
         ("C-c d n" . gud-next)
         ("C-c d s" . gud-step)
	 ("C-c d b" . gud-break)
	 ("C-c d k" . gud-remove)
         ("C-c d c" . gud-cont)
         ("C-c d f" . gud-finish)
         ("C-c d u" . gud-until)
         ("C-c d <up>" . gud-up)
         ("C-c d <down>" . gud-down))
  
  ;; Repeat map for debugging navigation
  (:repeat-map gud-repeat-map
               ;; Step commands (most commonly repeated)
               ("n" . gud-next)
               ("s" . gud-step)
               ("c" . gud-cont)
               ("f" . gud-finish)
               ("u" . gud-until)
               ;; Stack navigation
               ("<up>" . gud-up)
               ("<down>" . gud-down)
               ;; Quick inspection
               ("p" . gud-print)
               ("w" . gud-watch)
	       ("b" . gud-break)
               ("k" . gud-remove)
               ("r" . gud-run)
               :exit
               ;; Exit repeat mode for setup commands
               ("q" . gud-quit))
  :commands (gdb gud-gdb))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width 80)
  (setq nov-variable-pitch t)
  :hook
  (nov-mode . visual-line-mode))

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-window-height 30)
  (setq popper-display-function #'popper-display-popup-at-bottom)
  ;; (setq popper-group-function #'popper-group-by-project) 
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
	  "*vc-git.*\\*$"
	  "*vc-change-log*"
	  "*vc-diff*"
	  "*vc-log*"
	  "*just.*\\*$"
	  "*eldoc*"
	  "\\*AICHAT\\*"
          "\\*Async Shell Command\\*"
	  Man-mode
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; load my local packages
(add-to-list 'load-path "~/.config/emacs/lisp/")
(use-package my-zig
  :ensure nil
  :if (file-exists-p "~/.config/emacs/lisp/my-zig.el"))
(use-package my-ts
  :ensure nil
  :if (file-exists-p "~/.config/emacs/lisp/my-ts.el"))
(use-package my-go
  :ensure nil
  :if (file-exists-p "~/.config/emacs/lisp/my-go.el"))
(use-package my-rust
  :ensure nil
  :if (file-exists-p "~/.config/emacs/lisp/my-rust.el"))
;; (use-package my-layer
;;   :ensure nil
;;   :if (file-exists-p "~/.config/emacs/lisp/my-layer.el"))
;; use the built in gdb debugger instead
;; (use-package my-debugger
;;   :ensure nil
;;   :if (file-exists-p "~/.config/emacs/lisp/my-debugger.el"))
(use-package my-roc
  :ensure nil
  :if (file-exists-p "~/.config/emacs/lisp/my-roc.el"))
(use-package my-odin
  :ensure nil
  :if (file-exists-p "~/.config/emacs/lisp/my-odin.el"))
