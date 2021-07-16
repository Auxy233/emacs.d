;;; init-racket.el --- Racket
;;; Commentary:
;; require `Dr.Racket'

;;; Code:
;; Racket
(use-package racket-mode
  :mode "\\.rkt\\'"
  :hook ((racket-mode . rainbow-delimiters-mode)
         (racket-mode . paredit-mode)
         ;; (racket-mode . racket-repl-mode)
         (racket-mode . racket-xp-mode))
  :bind (:map
         racket-mode-map
         ("C-c C-s" . racket-check-syntax-mode)
         (")" . racket-insert-closing)
         ("]" . racket-insert-closing)
         ("}" . racket-insert-closing)
         ("C-M-\\" . racket-unicode-input-method-enable)
         :map
         paredit-mode-map
         ("[" . paredit-open-round)
         ("(" . paredit-open-square)
         ("M-[" . paredit-wrap-square)
         ("M-{" . paredit-wrap-curly)))

(use-package scribble-mode
  :mode "\\.scrbl\\'"
  :hook ((scribble-mode . rainbow-delimiters-mode)))

(use-package pollen-mode
  :mode "\\.\\(p\\|pp\\|pm\\|pmd\\|poly\\|ptree\\)\\'"
  :requires company-pollen)

(use-package geiser-guile
  :hook ((geiser-mode . paredit-mode))
  :bind (:map
         paredit-mode-map
         ("[" . paredit-open-round)
         ("(" . paredit-open-square)
         ("M-[" . paredit-wrap-square)
         ("M-{" . paredit-wrap-curly)))

(provide 'init-racket)
;;; init-racket ends here
