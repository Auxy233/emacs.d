;;; init-julia.el --- Julia
;;; Commentary:
;; Programming Language for Statistics

;; probably add LSP later
(use-package julia-mode
  :config
  (require 'julia-mode)
  (use-package julia-repl
    :hook (julia-mode . julia-repl-mode)
    :config
    (require 'julia-repl)))

(provide 'init-julia)
;;; init-julia ends here
