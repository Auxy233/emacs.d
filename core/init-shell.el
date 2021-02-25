;;; init-shell.el --- shell configurations
;;; Commentary:
;;

;;; Code:

(use-package eshell)

(use-package aweshell
  :straight (aweshell :type git :host github
                      :repo "manateelazycat/aweshell"))

(use-package shell
  :ensure nil
  :hook (shell-mode . (lambda ()
                        (term-mode-common-init)
                        (my/buffer-auto-close)))
  :bind (:map shell-mode-map
         ("M-r" . counsel-shell-history)))

;; Shell Pop
(use-package shell-pop
  :bind (("<f9>" . shell-pop))
  :custom
  (shell-pop-window-size 35)
  (shell-pop-shell-type
   '("eshell" "*eshell*" (lambda ()
                           (eshell)))))
        
(use-package fish-mode
  :mode "\\.fish$")

(provide 'init-shell)
;;; init-shell.el ends here
