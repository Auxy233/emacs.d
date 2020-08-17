;;; init-dev.el --- development related tools
;;; Commentary:
;; currently includes git, docker, and projectile

;;; Code:
(require 'init-const)

;; compilation mode
(use-package compile
  :ensure nil
  :custom
  (compilation-scroll-output t))

;; quickrun codes
(use-package quickrun
  :defer 1
  :custom (quickrun-focus-p nil))

;; use magit for git VC
(use-package magit
  :mode (("\\COMMIT_EDITMSG\\'" . text-mode)
         ("\\MERGE_MSG\\'" . text-mode))
  :config
  ;; access git forges from magit
  (use-package forge)

  ;; show TODOs in magit
  (use-package magit-todos
    :config
    (general-define-key
     :keymaps magit-todos-section-map
     "j" . nil)
    (general-define-key
     :keymaps magit-todos-item-section-map
     "j" . nil)
    :hook
    (magit-status-mode . magit-todos-mode))

  ;; walk through git revisions of a file
  (use-package git-timemachine
    :config
    (general-define-key
     :keymaps vc-prefix-map
     "t" . git-timemachine)))

;; git related modes
(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

;; project managemnt
(use-package projectile
  :diminish
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-use-git-grep t)
  :config
  (projectile-update-mode-line)         ; Update mode-line at the first time
  (setq projectile-git-submodule-command nil))

;; use ripgrep to power up search speed
(use-package ripgrep
  :defer t)

;; syntax checking
(use-package flycheck
  :diminish
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; set fringe style
  (setq flycheck-indication-mode 'right-fringe)
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  ;; display Flycheck errors in GUI tooltips
  (if (display-graphic-p)
      (if emacs/>=26p
          (use-package flycheck-posframe
            :custom-face (flycheck-posframe-border-face ((t (:inherit default))))
            :hook (flycheck-mode . flycheck-posframe-mode)
            :init (setq flycheck-posframe-border-width 1
                        flycheck-posframe-inhibit-functions
                        '((lambda (&rest _) (bound-and-true-p company-backend)))))
        (use-package flycheck-pos-tip
          :defines flycheck-pos-tip-timeout
          :hook (global-flycheck-mode . flycheck-pos-tip-mode)
          :config (setq flycheck-pos-tip-timeout 30)))
    (use-package flycheck-popup-tip
      :hook (flycheck-mode . flycheck-popup-tip-mode))))

;; docker
(use-package docker
  :defines docker-image-run-arguments
  :config
  (use-package docker-tramp :after (docker))
  (use-package dockerfile-mode :after (docker)))

(provide 'init-dev)
;;; init-dev.el ends here
