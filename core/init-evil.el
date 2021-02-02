;;; init-evil.el --- sweet devil mode
;;; Commentary:
;; use `evil' for editing and manipulate buffers
;; use `ctrl' to invoke hydra
;; use `M-' to invke Emacs style KB for editor management

;;; Code:
(require 'init-const)

;; please be aware of KB conflicts
(when sevil-use-evil
  (use-package evil
    :hook (after-init . evil-mode)
    :custom
    (evil-want-keybinding nil)
    (evil-want-integration t)
    :config

    (use-package evil-args)

    (use-package evil-matchit)

    (use-package evil-surround
      :hook (evil-mode . global-evil-surround-mode))

    ;; some modes are not natively supported by evil
    (use-package evil-collection
      :hook (evil-mode . evil-collection-init)
      :custom
      (evil-collection-company-setup t)
      (evil-collection-calendar-want-org-bindings nil)
      (evil-collection-outline-bind-tab-p t)
      (evil-collection-setup-minibuffer t)
      (evil-collection-setup-debugger-keys t))))

(provide 'init-evil)
;;; init-evil.el ends here
