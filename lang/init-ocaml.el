;;; init-ocaml.el --- OCaml
;;; Commentary:
;; require `dune', `ocaml', `utop', and `ocamllsp'

;;; Code:
;; OCaml
(use-package tuareg
  :mode ("\\.ml[4ilpy]?$" . tuareg-mode)
  :hook (tuareg-mode . lsp-deferred))
(use-package dune :after tuareg)
(use-package utop
  :after tuareg
  :bind (:map tuareg-mode-map)
  :when (executable-find "utop"))

;; SML
(use-package sml-mode
  :mode ("\\.sml$" . sml-mode))

(provide 'init-ocaml)
;;; init-ocaml.el ends here
