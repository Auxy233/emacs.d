;;; init-rust.el --- Rust
;;; Commentary:
;; require `rust-analyzer'

;;; Code:

(use-package rust-mode
  :init (setq rust-format-on-save t)
  :hook (rust-mode . lsp-deferred)
  :config
  (use-package cargo
    :diminish cargo-minor-mode
    :hook (rust-mode . cargo-minor-mode))
  (use-package flycheck-rust
    :after rust-mode
    :hook (flycheck-mode . flycheck-rust-setup)))

(provide 'init-rust)
;;; init-rust.el ends here
