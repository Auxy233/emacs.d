;;; init-note --- initialize org mode settings
;;; Commentary:
;; take note by org or markdown

;;; Code:

(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex))

(use-package org
  :hook
  ((org-mode text-mode markdown-mode) . auto-fill-mode)
  :custom
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-src-tab-acts-natively t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  ;; latex in org
  (org-highlight-latex-and-related '(native script entities))
  (org-export-with-latex t)
  ;; literature programming
  (org-confirm-babel-evaluate nil)
  (org-babel-load-languages
   '((C . t)
     (lisp . t)
     (python . t)
     (scheme . t)
     (ocaml . t)))
  :config
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2.5)))

;; pretty symbols
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package literate-calc-mode
  :ensure t
  :bind ("C-x x n" . literate-calc-eval-buffer))

(provide 'init-note)
;;; init-note.el ends here
