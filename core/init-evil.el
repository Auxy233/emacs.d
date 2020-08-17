;;; init-evil.el --- sweet devil mode
;;; Commentary:
;; use `evil' for editing and manipulate buffers
;; use `ctrl' to invoke hydra
;; use `s-' to invke Emacs style KB for editor management

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
    (evil-collection-setup-debugger-keys t)
    ))

;; I write lisp
(when sevil-swap-parentheses-square-bracket
  (progn
    (keyboard-translate ?\( ?\[)
    (keyboard-translate ?\[ ?\()
    (keyboard-translate ?\) ?\])
    (keyboard-translate ?\] ?\))))

;; start defining keybinding
(use-package general
  :config
  ;; emacs key maps
  (general-define-key
   "s-s" 'isearch-forward

   "s-q" 'evil-force-normal-state

   ;; applications
   "s-a" '(:keymap nil :which-key "apps")
   "s-a s-c" 'calendar
   "s-a s-i" 'erc
   "s-a s-s" 'shell-pop
   "s-a s-r" 'recentf-open-files

   ;; buffers
   "s-b" '(:keymap nil :which-key "buffer")
   "s-b s-b" 'ivy-switch-buffer
   "s-b s-i" 'ibuffer
   "s-b s-k" 'kill-this-buffer
   "s-b s-n" 'next-buffer
   "s-b s-p" 'previous-buffer
   "s-b s-r" 'revert-buffer

   ;; files
   "s-f" '(:keymap nil :which-key "files")
   "s-f f" 'find-file-other-window
   "s-f j" 'dired-jump-other-window
   "s-f s-d" 'delete-file
   "s-f s-f" 'find-file
   "s-f s-j" 'dired-jump
   "s-f s-m" 'make-directory
   "s-f s-r" 'rename-file
   "s-f s-t" 'treemacs
   "s-f s-a" 'treemacs-add-project
   "s-f s-l" 'list-directory

   "s-l" 'lsp-command-map

   "s-p" 'projectile-command-map

   "s-w" 'persp-mode-map

   ;; emacs management
   "s-e" '(:keymap nil :which-key "emacs")
   "s-e q" 'kill-emacs
   "s-e f" 'delete-frame
   "s-e s" 'save-buffer)

  ;; evil key maps
  (general-define-key
   :states '(normal visual)
   "C-r" 'undo-fu-only-redo
   "u" 'undo-fu-only-undo

   ;; avy jump
   "gc" 'evil-avy-goto-char-timer
   "go" 'evil-avy-goto-word-or-subword-1
   "gi" 'evil-avy-goto-line
   "gl" 'goto-line
   "gw" 'evil-avy-goto-word-1
   "gr" 'avy-resume
   "gp" 'avy-prev)

  ;; less tired vim
  (when sevil-swap-colon
    (general-define-key
     :states '(normal visual)
     ";" 'evil-ex
     ":" 'evil-repeat-find-char))

  ;; the comma leader key serves for emacs managemnt
  (general-create-definer edit-leader-def
    :prefix "SPC"
    :states '(normal visual emacs))

  (edit-leader-def
    ;; meta options
    "SPC" 'counsel-M-x
    "TAB" 'next-buffer

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

    ;; delete
    "d" '(:ignore t :which-key "delete")
    "dc" 'avy-zap-to-char-dwim
    "dC" 'avy-zap-up-to-char-dwim

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

    ;; symbol overlays
    "s" '(:ignore t :which-key "symbol")
    "sp" 'symbol-overlay-put
    "sf" 'symbol-overlay-jump-next
    "sb" 'symbol-overlay-jump-prev
    "sF" 'symbol-overlay-switch-forward
    "sB" 'symbol-overlay-switch-backward
    "sr" 'symbol-overlay-remove-all

    ;; tranpose
    "t" '(:ignore t :which-key "transpose")
    "ta" 'align
    "tt" 'transpose-chars
    "tl" 'transpose-lines
    "tw" 'transpose-words

    ;; url
    "u" '(:ignore t :which-key "url")
    "u." 'browse-url-at-point
    "ub" 'browse-url-of-buffer
    "ur" 'browse-url-of-region
    "uu" 'browse-url
    "ue" 'browse-url-emacs
    "uv" 'browse-url-of-file

    ;; misc
    "x" '(:ignore t :which-key "misc")
    "xa" 'swiper-all
    "xb" 'dap-debug
    "xi" 'imenu
    "xj" 'evil-show-jumps
    "xm" 'evil-show-marks
    "xr" 'counsel-rg
    "xs" 'swiper
    "xc" 'quickrun
    "xd" 'docker

    ;; yasnippet
    "yy" 'yas-visit-snippet-file
    "yc" 'yas--snippet-create

    ;; windows
    "w" '(:keymap evil-window-map :which-key "window")
    "w-" 'split-window-vertically
    "w/" 'split-window-horizontally
    "wr" 'winner-redo
    "wu" 'winner-undo))

;; Tips for next keystroke
(when sevil-use-which-key
  (use-package which-key
    :hook (after-init . which-key-mode)
    :custom
    (which-key-idle-delay 0.5)
    (which-key-add-column-padding 1)))

(provide 'init-evil)
;;; init-evil.el ends here
