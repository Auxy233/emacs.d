;;; init-evil.el --- sweet devil mode
;;; Commentary:
;; modal editing

;;; Code:
(require 'init-const)

;; it's not idiomatic to map ctrl-c...
(when sevil-use-evil
  (use-package evil
    :hook
    (after-init . evil-mode)
    :custom
    (evil-want-keybinding nil)
    (evil-want-integration t)
    (evil-undo-system 'undo-fu)
    (evil-want-fine-undo t)
    :config
    (defmacro lead-k (k)
      (let ((leader-kbd (concat "<leader>" k)))
        `(kbd ',leader-kbd)))
    
    (evil-set-leader '(normal visual) (kbd "SPC"))

    ;; try to match the emacs KB
    (evil-define-key '(normal visual) 'global
      "gs" 'evil-avy-goto-char-timer
      "go" 'evil-avy-goto-word-or-subword-1
      "gl" 'evil-avy-goto-line
      "gc" 'dumb-jump-go
      
      (lead-k "a") 'align-regexp
      (lead-k "o") 'ace-window
      (lead-k "w") 'hydra-ace/body
      ;; code
      (lead-k "cc") 'compile
      (lead-k "cr") 'recompile
      (lead-k "cx") 'quickrun
      (lead-k "cl") 'comment-line
      (lead-k "cd") 'comment-dwim
      (lead-k "ck") 'comment-kill
      (lead-k "cb") 'comment-box
      (lead-k "ci") 'flycheck-list-errors
      (lead-k "cp") 'flycheck-previous-error
      (lead-k "cn") 'flycheck-next-error
      ;; dired
      (lead-k "d") 'dired
      ;; edit
      (lead-k "ed") 'diff-vc-revisions
      ;; git
      (lead-k "g") 'magit-status
      ;; menu
      (lead-k "ib") 'ibuffer
      (lead-k "im") 'imenu
      ;; project
      (lead-k "pr") 'project-find-regexp
      (lead-k "ps") 'project-search
      (lead-k "pd") 'project-dired
      (lead-k "pf") 'project-find-file
      (lead-k "pc") 'project-compile
      (lead-k "pb") 'project-switch-to-buffer
      (lead-k "pp") 'project-switch-project
      ;; flycheck
      (lead-k "f") 'flycheck-list-errors
      (lead-k ".") 'flyspell-correct-at-point
      ;; tab
      (lead-k "/") 'hs-hide-all
      (lead-k "\\") 'hs-show-all
      ;; spell check
      (lead-k "s") 'hydra-spell/body
      ;; transform
      (lead-k "ta") 'align 
      (lead-k "tc") 'transpose-chars
      (lead-k "tl") 'transpose-lines
      (lead-k "tw") 'transpose-words
      ;; yasnippet
      (lead-k "y") 'yas-visit-from-menu
      ;; misc
      (lead-k "0") 'delete-window
      (lead-k "1") 'delete-other-windows
      (lead-k "<tab>") 'next-buffer
      (lead-k "xr") 'counsel-recentf
      (lead-k "xs") 'eshell
      (lead-k "xt") 'treemacs)
    
    (use-package evil-args)

    (use-package evil-matchit)

    (use-package evil-surround
      :hook (evil-mode . global-evil-surround-mode))

    ;; some modes are not natively supported by evil
    (use-package evil-collection
      :hook (evil-mode . evil-collection-init)
      :custom
      (evil-collection-company-setup nil)
      (evil-collection-calendar-want-org-bindings nil)
      (evil-collection-outline-bind-tab-p t)
      (evil-collection-setup-minibuffer nil)
      (evil-collection-setup-debugger-keys t))))


(provide 'init-evil)
;;; init-evil.el ends here
