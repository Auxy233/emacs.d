;;; init-evil.el --- sweet devil mode
;;; Commentary:
;; use `evil' for editing and manipulate buffers
;; use `ctrl' to invoke hydra
;; use `M-' to invke Emacs style KB for editor management

;;; Code:
(require 'init-const)

(use-package meow
  :demand t
  :init
  (meow-global-mode 1)
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . meow-motion-origin-command)
     '("k" . meow-motion-origin-command)
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
     '("0" . meow-digit-argument))
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
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("C" . meow-change-save)
     '("d" . meow-delete)
     '("x" . meow-line)
     '("f" . meow-find)
     '("F" . meow-find-expand)
     '("g" . meow-keyboard-quit)
     '("G" . meow-goto-line)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("m" . meow-join)
     '("M" . delete-indentation)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("T" . meow-till-expand)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("o" . meow-block)
     '("O" . meow-block-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("q" . meow-quit)
     '("r" . meow-replace)
     '("R" . meow-replace-save)
     '("n" . meow-search)
     '("N" . meow-pop-search)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("u" . undo)
     '("v" . meow-visit)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("y" . meow-save)
     '("p" . meow-yank)
     '("z" . meow-pop-selection)
     '("Z" . meow-pop-all-selection)
     '("&" . meow-query-replace)
     '("%" . meow-query-replace-regexp)
     '("<escape>" . meow-last-buffer)))
  (meow-setup)
  (meow-setup-line-number)
  (meow-setup-indicator))

;; please be aware of KB conflicts
(when sevil-use-evil
  (use-package evil
    :hook (after-init . evil-mode)
    :custom
    (evil-want-keybinding nil)
    (evil-want-integration t)
    :config

    (use-package evil-args)

    (use-package evil-matchit)

    (use-package evil-surround
      :hook (evil-mode . global-evil-surround-mode))

    ;; some modes are not natively supported by evil
    (use-package evil-collection
      :hook (evil-mode . evil-collection-init)
      :custom
      (evil-collection-company-setup t)
      (evil-collection-calendar-want-org-bindings nil)
      (evil-collection-outline-bind-tab-p t)
      (evil-collection-setup-minibuffer t)
      (evil-collection-setup-debugger-keys t))))

(provide 'init-evil)
;;; init-evil.el ends here
