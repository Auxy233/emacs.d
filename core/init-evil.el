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

  (use-package evil-args)

  (use-package evil-matchit)

  (use-package evil-magit
    :after (evil magit))

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
    (evil-collection-setup-debugger-keys t)))

;; start defining keybinding
(use-package general
  :config
  ;; emacs key maps
  (general-define-key
   "M-s" 'isearch-forward

   "M-q" 'evil-force-normal-state

   ;; buffers
   "M-b" '(:keymap nil :which-key "buffer")
   "M-b b" 'ivy-switch-buffer
   "M-b i" 'ibuffer
   "M-b k" 'kill-this-buffer
   "M-b K" 'kill-matching-buffers
   "M-b n" 'next-buffer
   "M-b p" 'previous-buffer
   "M-b r" 'revert-buffer

   ;; files
   "M-f" '(:keymap nil :which-key "files")
   "M-f M-f" 'find-file-other-window
   "M-f M-j" 'dired-jump-other-window
   "M-f d" 'delete-file
   "M-f f" 'find-file
   "M-f j" 'dired-jump
   "M-f m" 'make-directory
   "M-f r" 'rename-file
   "M-f t" 'treemacs
   "M-f a" 'treemacs-add-project
   "M-f l" 'list-directory
   "M-f s" 'save-some-buffers
   "M-f R" 'recentf-open-files

   "M-p" 'projectile-command-map

   "M-w" 'persp-mode-map

   ;; emacs management
   "M-e" '(:keymap nil :which-key "emacs")
   "M-e q" 'kill-emacs
   "M-e f" 'delete-frame
   "M-e u" 'sevil-update-all
   "M-e s" 'save-buffer)

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

  ;; I write lisp
  (general-define-key
   :state '(insert)
   "s-[" 'paredit-open-round
   "s-]" 'paredit-close-round)

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
    "SPC" 'amx
    "TAB" 'next-buffer

    ;; applications
    "a" '(:keymap nil :which-key "apps")
    "ac" 'calendar
    "ai" 'erc
    "as" 'shell-pop
    "ar" 'recentf-open-files

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

    ;; movement
    "m" '(:ignore t :which-key "move")
    "ms" 'evil-inner-arg
    "mS" 'evil-outer-arg
    "ma" 'evil-backward-arg
    "md" 'evil-forward-arg
    "mw" 'evil-jump-out-args

    ;; paredit
    "p" 'enable-paredit-mode
    "P" 'disable-paredit-mode

    ;; symbol overlays
    "s" '(:ignore t :which-key "symbol")
    "sp" 'symbol-overlay-put
    "sf" 'symbol-overlay-jump-next
    "sb" 'symbol-overlay-jump-prev
    "sF" 'symbol-overlay-switch-forward
    "sB" 'symbol-overlay-switch-backward
    "sr" 'symbol-overlay-remove-all

    ;; transform
    "t" '(:ignore t :which-key "transform")
    ;;; align
    "ta" 'align
    ;;; word transpose
    "tt" 'transpose-chars
    "tl" 'transpose-lines
    "tw" 'transpose-words
    ;;; comment
    "tc" 'comment-line
    "tk" 'comment-kill
    "ti" 'comment-indent

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
