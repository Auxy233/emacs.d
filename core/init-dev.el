;;; init-dev.el --- development related tools
;;; Commentary:
;; currently includes git, docker, and projectile

;;; Code:
(require 'init-const)

;; compilation mode
(use-package compile
  :ensure nil
  :bind (("C-c c c" . compile)
         ("C-c c r" . recompile))
  :custom
  (compilation-scroll-output t))

;; quickrun codes
(use-package quickrun
  :defer 1
  :bind (("C-c c x" . quickrun))
  :custom (quickrun-focus-p nil))

;; use magit for git VC
(use-package magit
   :bind (("C-x g" . magit-status)
          ("C-x M-g" . magit-dispatch)
          ("C-c M-g" . magit-file-popup))
  :mode (("\\COMMIT_EDITMSG\\'" . text-mode)
         ("\\MERGE_MSG\\'" . text-mode))
  :config
  ;; access git forges from magit
  (when (executable-find "sqlite")
        (use-package forge)))

;; walk through git revisions of a file
(use-package git-timemachine
  :bind (:map vc-prefix-map
              ("t" . git-timemachine)))

;; git related modes
(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

;; project managemnt
(use-package project
  :config
  (define-key ctl-x-map "p" project-prefix-map))
 
;; use ripgrep to power up search speed
(use-package ripgrep
  :defer t)

;; syntax checking
(use-package flycheck
  :diminish
  :hook (after-init . global-flycheck-mode)
  :bind (("C-c !" . flycheck-list-errors))
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-indication-mode 'right-fringe)
  (flycheck-temp-prefix ".flycheck")
  (flycheck-keymap-prefix (kbd "C-c f"))
  :config
  (use-package flycheck-pos-tip
    :hook
    (flycheck-mode . flycheck-pos-tip-mode))
  (use-package flycheck-color-mode-line
    :hook
    (flycheck-mode . flycheck-color-mode-line-mode))
  (use-package flycheck-rust
    :hook
    (rust-mode . flycheck-rust-setup))
  (use-package flycheck-ocaml
    :hook
    (tuareg-mode . flycheck-ocaml-setup))
  (use-package flycheck-haskell
    :hook
    (haskell-mode . flycheck-haskell-setup)))

;; docker
(use-package dockerfile-mode)

(provide 'init-dev)
;;; init-dev.el ends here
