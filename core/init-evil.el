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
  (when sevil-swap-parentheses-square-bracket
    (progn
      (keyboard-translate ?\( ?\[)
      (keyboard-translate ?\[ ?\()
      (keyboard-translate ?\) ?\])
      (keyboard-translate ?\] ?\))))

  (general-define-key
   :states '(normal visual)
   "C-r" 'undo-fu-only-redo
   "u" 'undo-fu-only-undo

   ;; avy jump
   "gc" 'evil-avy-goto-char-timer
   "go" 'evil-avy-goto-word-or-subword-1
   "gl" 'evil-avy-goto-line
   "gw" 'evil-avy-goto-word-1
   "gr" 'avy-resume
   "gp" 'avy-prev

   ;; tranpose
   "tt" 'transpose-chars
   "tl" 'transpose-lines
   "tw" 'transpose-words)

  ;; less tired vim
  (when sevil-swap-colon
    (general-define-key
     :states '(normal visual)
     ";" 'evil-ex
     ":" 'evil-repeat-find-char))

  ;; the comma leader key serves for emacs managemnt
  (general-create-definer edit-leader-def
    :prefix ","
    :states '(normal visual emacs))

  (edit-leader-def
    ;; meta options
    "," 'counsel-M-x
    "TAB" 'next-buffer

    ;; applications
    "a" '(:ignore t :which-key "apps")
    "ac" 'calendar
    "ai" 'erc
    "ar" 'recentf-open-files

    ;; buffers
    "b" '(:ignore t :which-key "buffer")
    "bb" 'ivy-switch-buffer
    "bi" 'ibuffer
    "bk" 'kill-this-buffer
    "bn" 'next-buffer
    "bp" 'previous-buffer
    "br" 'revert-buffer

    ;; flycheck
    "C" '(:ignore t :which-key "check")
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
    "c" '(:ignore t :which-key "counsel")
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
    "f" '(:ignore t :which-key "files")
    "fF" 'find-file-other-window
    "fJ" 'dired-jump-other-window
    "fd" 'delete-file
    "ff" 'find-file
    "fj" 'dired-jump
    "fm" 'make-directory
    "fr" 'rename-file
    "ft" 'treemacs

    ;; git
    "g" '(:ignore t :which-key "git")
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
    "h" '(:ignore t :which-key "hltodo")
    "hb" 'hl-todo-previous
    "hf" 'hl-todo-next
    "ho" 'hl-todo-occur
    "hi" 'hl-todo-insert

    ;; projectile
    "p" '(:keymap projectile-command-map :which-key "projectile")

    ;; symbol overlays
    "s" '(:ignore t :which-key "symbol")
    "sp" 'symbol-overlay-put
    "sf" 'symbol-overlay-jump-next
    "sb" 'symbol-overlay-jump-prev
    "sF" 'symbol-overlay-switch-forward
    "sB" 'symbol-overlay-switch-backward
    "sr" 'symbol-overlay-remove-all

    ;; misc
    "x" '(:ignore t :which-key "misc")
    "xa" 'swiper-all
    "xh" 'shell-pop
    "xi" 'imenu
    "xj" 'evil-show-jumps
    "xm" 'evil-show-marks
    "xr" 'counsel-rg
    "xs" 'swiper

    ;; windows
    "w" '(:keymap evil-window-map :which-key "window")
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
  (evil-collection-calendar-want-org-bindings nil)
  (evil-collection-outline-bind-tab-p t)
  (evil-collection-setup-minibuffer t)
  (evil-collection-setup-debugger-keys t)
  )

(use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))

;; Tips for next keystroke
(when sevil-use-which-key
  (use-package which-key
    :hook (after-init . which-key-mode)
    :custom
    (which-key-idle-delay 0.5)
    (which-key-add-column-padding 1)))

(provide 'init-evil)
;;; init-evil.el ends here
