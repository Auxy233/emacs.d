;;; init-cpp.el --- C/C++
;;; Commentary:
;; require `clangd'

;;; Code:

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :hook ((c-mode c++-mode) . lsp-deferred)
  :bind (:map c-mode-base-map
         ("C-c c" . compile))
  :init (setq-default c-basic-offset 4)
  :config
  (use-package modern-cpp-font-lock
    :diminish
    :init (modern-c++-font-lock-global-mode t)))

(use-package cmake-mode
  :defines (company-backends)
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :config
  (with-eval-after-load 'company-mode
    (add-to-list 'company-backends 'company-cmake)))

(provide 'init-cpp)
