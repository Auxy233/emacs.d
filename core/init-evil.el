;;; init-evil.el --- sweet devil mode
;;; Commentary:
;; use evil for keybindings

;;; Code:
(require 'init-const)

(use-package evil
  :hook (after-init . evil-mode)
  :custom
  (evil-want-keybinding nil)
  (evil-want-integration t)
  :config

  (use-package evil-magit
    :after evil magit)

  ;; start defining keybinding
  (use-package general)

  ;; I write lisp
  (progn
    (keyboard-translate ?\( ?\[)
    (keyboard-translate ?\[ ?\()
    (keyboard-translate ?\) ?\])
    (keyboard-translate ?\] ?\)))

  (general-define-key
   :states '(normal visual)
   "C-r" 'undo-fu-only-redo
   "u" 'undo-fu-only-undo
   ";" 'evil-ex
   ":" 'evil-repeat-find-char

   ;; avy jump
   "gs" 'evil-avy-goto-char-timer
   "go" 'evil-avy-goto-word-or-subword-1
   "gl" 'evil-avy-goto-line)

  ;; the comma leader key serves for emacs managemnt
  (general-create-definer edit-leader-def
    :prefix ","
    :states '(normal visual emacs))

  (edit-leader-def
    ;; meta options
    "," 'counsel-M-x
    "TAB" 'next-buffer

    ;; applications
    "ac" 'calendar
    "ai" 'erc

    ;; buffers
    "bb" 'ivy-switch-buffer
    "bi" 'ibuffer
    "bk" 'kill-this-buffer
    "bn" 'next-buffer
    "bp" 'previous-buffer
    "br" 'revert-buffer

    ;; flycheck
    "Cc" 'flycheck-buffer
    "CC" 'flycheck-clear
    "Cx" 'flycheck-compile
    "Cn" 'flycheck-next-error
    "Cp" 'flycheck-previous-error
    "Cl" 'flycheck-list-errors
    "Cw" 'flycheck-copy-errors-as-kill
    "Cs" 'flycheck-select-checker
    "C?" 'flycheck-describe-checker
    "Ch" 'flycheck-display-error-at-point
    "Ce" 'flycheck-explain-error-at-point
    "CH" 'display-local-help
    "Ci" 'flycheck-manual
    "CV" 'flycheck-version
    "Cv" 'flycheck-verify-setup
    "Cd" 'flycheck-disable-checker

    ;; counsel
    "cB" 'counsel-bookmarked-directory
    "cF" 'counsel-faces
    "cL" 'counsel-load-library
    "cO" 'counsel-find-file-extern
    "cP" 'counsel-package
    "ca" 'counsel-apropos
    "ce" 'counsel-colors-emacs
    "cf" 'counsel-find-library
    "ch" 'counsel-command-history
    "ci" 'counsel-git
    "cj" 'counsel-git-grep
    "cl" 'counsel-locate
    "cm" 'counsel-imenu
    "cM" 'counsel-minibuffer-history
    "co" 'counsel-outline
    "cp" 'counsel-pt
    "cr" 'counsel-rg
    "cs" 'ivy-resume
    "ct" 'counsel-load-theme
    "cu" 'counsel-unicode-char
    "cv" 'counsel-set-variable
    "cw" 'counsel-colors-web
    "cz" 'counsel-fzf

    ;; files
    "fF" 'find-file-other-window
    "fJ" 'dired-jump-other-window
    "fd" 'delete-file
    "ff" 'find-file
    "fj" 'dired-jump
    "fm" 'make-directory
    "fr" 'rename-file
    "ft" 'treemacs

    ;; gits
    "gB" 'magit-blame-addition
    "gC" 'magit-commit-create
    "gD" 'magit-dispatch
    "gG" 'magit-status-here
    "gb" 'magit-branch-checkout
    "gc" 'magit-branch-and-checkout
    "gd" 'magit-diff
    "gf" 'magit-find-file
    "gg" 'magit-status
    "gi" 'magit-init
    "gp" 'magit-file-popup
    "gr" 'magit-rebase-interactive

    ;; hl-todo
    "hb" 'hl-todo-previous
    "hf" 'hl-todo-next
    "ho" 'hl-todo-occur
    "hi" 'hl-todo-insert

    ;; projectile
    "p" 'projectile-command-map

    "r" 'recentf-open-files

    ;; symbol overlays
    "sp" 'symbol-overlay-put
    "sf" 'symbol-overlay-jump-next
    "sb" 'symbol-overlay-jump-prev
    "sF" 'symbol-overlay-switch-forward
    "sB" 'symbol-overlay-switch-backward
    "sr" 'symbol-overlay-remove-all

    ;; misc
    "xa" 'swiper-all
    "xh" 'shell-pop
    "xi" 'imenu
    "xj" 'evil-show-jumps
    "xm" 'evil-show-marks
    "xr" 'counsel-rg
    "xs" 'swiper

    ;; windows
    "w" 'evil-window-map
    "w-" 'split-window-vertically
    "w/" 'split-window-horizontally
    "wr" 'winner-redo
    "wu" 'winner-undo)

  ;; `SPC' is shadowed in some modes (e.g. calendar)
  ;; so we use it for programming only, rather than managing emacs
  (general-create-definer prog-leader-def
    :prefix "SPC"
    :states '(normal visual))

  ;; bind `lsp-mode' to `SPC'
  (evil-define-key '(normal visual) lsp-mode-map (kbd "SPC") lsp-command-map)

  (prog-leader-def
    ;; lsp is binded here as well
    ;; so avoid using following characters:
    ;; `s` `=` `F` `T` `g` `h` `r` `a` `G`
    "SPC" 'counsel-M-x
    "xc" 'quickrun
    "xd" 'docker))

;; some modes are not natively supported by evil
(use-package evil-collection
  :hook (evil-mode . evil-collection-init)
  :custom
  (evil-collection-company-setup t)
  (evil-collection-calendar-want-org-bindings t)
  (evil-collection-outline-bind-tab-p t)
  (evil-collection-setup-minibuffer t)
  (evil-collection-setup-debugger-keys t)
  )

(use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))

(provide 'init-evil)
;;; init-evil.el ends here
