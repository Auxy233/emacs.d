;;; init-agda.el --- Agda
;;; Commentary:
;; Agda Proof Assistance

;;; Code:

(when (executable-find "agda-mode")
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))

(provide 'init-agda)
;;; init-agda ends here
