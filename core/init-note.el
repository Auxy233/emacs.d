;;; init-note --- initialize org mode settings
;;; Commentary:
;; take note by org or markdown

;;; Code:

(use-package org
  :bind (("C-c o o" . hydra-ox/body))
  :hook ((org-mode text-mode markdown-mode) . auto-fill-mode)
  :custom
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-src-tab-acts-natively t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-confirm-babel-evaluate nil)
  (org-babel-load-languages
   '((C . t)
     (lisp . t)
     (python . t)
     (scheme . t)
     (ocaml . t))))

;; pretty symbols
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package literate-calc-mode
  :ensure t
  :bind ("C-x x n" . literate-calc-eval-buffer))

(provide 'init-note)
;;; init-note.el ends here
