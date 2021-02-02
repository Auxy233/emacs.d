;;; init-shell.el --- shell configurations
;;; Commentary:
;;

;;; Code:

(use-package eshell)

;; display extra information for prompt
(use-package eshell-prompt-extras
  :after esh-opt
  :defines eshell-highlight-prompt
  :commands (epe-theme-lambda epe-theme-dakrone epe-theme-pipeline)
  :init (setq eshell-highlight-prompt nil
                eshell-prompt-function #'epe-theme-lambda))

;; fish-like history autosuggestions
(use-package esh-autosuggest
  :defines ivy-display-functions-alist
  :bind (:map eshell-mode-map
              ([remap eshell-pcomplete] . completion-at-point))
  :hook ((eshell-mode . esh-autosuggest-mode)
         (eshell-mode . eshell-setup-ivy-completion))
  :init (defun eshell-setup-ivy-completion ()
          "Setup `ivy' completion in `eshell'."
          (setq-local ivy-display-functions-alist
                      (remq (assoc 'ivy-completion-in-region
                                   ivy-display-functions-alist)
                            ivy-display-functions-alist))))

;; `eldoc' support
(use-package esh-help
  :init (setup-esh-help-eldoc))

;; `cd' to frequent directory in `eshell'
(use-package eshell-z
  :hook (eshell-mode . (lambda () (require 'eshell-z))))

(use-package shell
  :ensure nil
  :hook (shell-mode . (lambda ()
                        (term-mode-common-init)
                        (my/buffer-auto-close)))
  :bind (:map shell-mode-map
         ("M-r" . counsel-shell-history)))

;; Better term
;; @see https://github.com/akermu/emacs-libvterm#installation
(when (and module-file-suffix           ; dynamic module
           (executable-find "cmake")
           (executable-find "libtool")
           (executable-find "make"))
  (use-package vterm))

;; Shell Pop
(use-package shell-pop
  :bind (("<f9>" . shell-pop))
  :init
  (setq shell-pop-window-size 35
        shell-pop-shell-type
        (cond ((fboundp 'vterm) '("vterm" "*vterm*" (lambda () (vterm))))
              (t '("eshell" "*eshell*" (lambda () (eshell))))
              (t '("terminal" "*terminal*" (lambda () (term shell-pop-term-shell)))))))

(provide 'init-shell)
;;; init-shell.el ends here
