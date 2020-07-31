;;; init-note --- initialize org mode settings
;;; Commentary:
;; take note by org or markdown

;;; Code:

(use-package org
  :hook ((org-mode text-mode markdown-mode) . auto-fill-mode)
  :custom
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-confirm-babel-evaluate nil)
  (org-babel-load-languages
   '((C . t)
     (lisp . t)
     (python . t)
     (ocaml . t))))

;; pretty symbols
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(provide 'init-note)
;;; init-note.el ends here
