;;; init-helm.el --- helm configuration
;;; Commentary:
;; helm fuzzy complement framework
;; heavier but more powerful

;;; Code:

(use-package helm-mode
  :defer t
  :hook (after-init . helm-mode))

(provide 'init-helm)
;; init-helm.el ends here
