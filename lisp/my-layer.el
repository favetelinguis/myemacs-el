;;; my-layer.el --- A simple hello world package

;;; Commentary:
;; How much can I imporve if I focus on:
;; Adding and junping to registry
;; Modal editing with meow
;; movement with avy

;;; Code:

(defun toggle-previous-buffer ()
  "Toggle between current and previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(use-package meow
  :ensure t
  :init
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    ;; when i use 1.6 i should use this
    ;;(meow-motion-define-key
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
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
     `("p" . ,project-prefix-map)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet)
     )
    (meow-normal-define-key
     '("0" . (lambda () (interactive) (point-to-register ?0)))
     '("1" . (lambda () (interactive) (point-to-register ?1)))
     '("2" . (lambda () (interactive) (point-to-register ?2)))
     '("3" . (lambda () (interactive) (point-to-register ?3)))
     '("4" . (lambda () (interactive) (point-to-register ?4)))
     '("5" . (lambda () (interactive) (point-to-register ?5)))
     '("6" . (lambda () (interactive) (point-to-register ?6)))
     '("7" . (lambda () (interactive) (point-to-register ?7)))
     '("8" . (lambda () (interactive) (point-to-register ?8)))
     '("9" . (lambda () (interactive) (point-to-register ?9)))
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '(":" . ignore)
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
     '("f" . avy-goto-word-1)
     '("F" . avy-goto-line)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . ignore)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . scroll-up)
     '("J" . meow-next-expand)
     '("k" . scroll-down)
     '("K" . meow-prev-expand)
     '("l" . consult-register-load)
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
     '("t" . toggle-previous-buffer)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit) 
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("SPC" . meow-keypad) 
     '("<escape>" . ignore)))
  :custom
  (meow-use-clipboard t)
  :config
  (meow-global-mode 1)
  (setq meow-expand-hint-counts nil)
  (meow-setup))

(use-package avy
  :ensure t
  :config
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)

  ;; Activate highlight over all windows
  ;;(setq avy-all-windows t)
  )

(provide 'my-layer)
;;; my-layer.el ends here

