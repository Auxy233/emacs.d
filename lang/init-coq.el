;;; init-coq.el --- Coq Theorem Prover
;;; Commentary:
;; require `coq' related binaries

;;; Code:

;; HACK for some reasons PG cannot be loaded with correct location
;; so set path manually
(when sys/mac-x-p
    (setq
     pg-init--script-full-path (concat user-emacs-directory "straight/repos/PG/proof-general.el")
     pg-init--pg-root (file-name-directory pg-init--script-full-path)))

(use-package proof-general
  :mode ("\\.v$" . coq-mode)
  :custom
  (proof-splash-enable nil)
  (coq-project-filename "_CoqProject"))

(use-package company-coq
  :after proof-general
  :hook (coq-mode . company-coq-mode)
  :custom
  (company-coq-features/prettify-symbols-in-terminals t))

(provide 'init-coq)
;;; init-coq ends here
