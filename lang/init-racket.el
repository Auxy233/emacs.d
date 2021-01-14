;;; init-racket.el --- Racket
;;; Commentary:
;; require `Dr.Racket'

;;; Code:
;; Racket
(use-package racket-mode
  :mode "\\.rkt$"
  :hook ((racket-mode . rainbow-delimiters-mode))
  :bind (:map racket-mode-map
              ("C-c C-s" . racket-check-syntax-mode)
              ("[" . paredit-open-round)
              ("(" . paredit-open-bracket)
              (")" . racket-insert-closing)
              ("]" . racket-insert-closing)
              ("}" . racket-insert-closing)
              ("C-M-\\" . racket-unicode-input-method-enable)))

(use-package scribble-mode
  :mode "\\.scrbl\\'"
  :hook ((scribble-mode . rainbow-delimiters-mode)))

(use-package pollen-mode
  :mode "\\.\\(p\\|pp\\|pm\\|pmd\\|poly\\|ptree\\)\\'"
  :requires company-pollen)

;; Scheme
(use-package geiser
  :init
  (defvar geiser-active-implementations)
  (setq geiser-active-implementations '(chez)))

(provide 'init-racket)
;;; init-racket ends here
