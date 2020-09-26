;;; init-clisp.el --- Common Lisp
;;; Commentary:
;; require https://github.com/roswell/roswell/

;;; Code:

(use-package sly-asdf
  :defer t
  :after (sly company))

(use-package sly-quicklisp
  :after sly)

(use-package sly
  :custom
  (inferior-lisp-program sevil-lisp-compiler)
  :init
  (setq sly-complete-symbol*-fancy t)
  (setq sly-contribs '(sly-fancy
                         sly-indentation
                         sly-autodoc
                         sly-sbcl-exts
                         sly-scratch))
  :config
  (sly-setup '(sly-fancy sly-asdf sly-quicklisp)))

(use-package common-lisp-snippets
  :defer t
  :after (yasnippet))

(provide 'init-clisp)
;;; init-clisp.el ends here
