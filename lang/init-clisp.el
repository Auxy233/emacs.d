;;; init-clisp.el --- Common Lisp
;;; Commentary:
;; require https://github.com/roswell/roswell/

;;; Code:

(use-package slime-company
  :defer t
  :after (slime company))

(use-package slime
  :custom
  (inferior-lisp-program "ros -Q run")
  :init
  (setq slime-complete-symbol*-fancy t)
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (setq slime-contribs '(slime-fancy
                         slime-indentation
                         slime-autodoc
                         slime-sbcl-exts
                         slime-scratch))
  :config
  (slime-setup '(slime-company slime-fancy slime-asdf slime-quicklisp)))

(use-package common-lisp-snippets
  :defer t
  :after (yasnippet))

(provide 'init-clisp)
;;; init-clisp.el ends here
