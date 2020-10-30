;;; init-evil.el --- sweet devil mode
;;; Commentary:
;; use `evil' for editing and manipulate buffers
;; use `ctrl' to invoke hydra
;; use `M-' to invke Emacs style KB for editor management

;;; Code:
(require 'init-const)

(when sevil-use-evil
  (use-package evil
    :hook (after-init . evil-mode)
    :custom
    (evil-want-keybinding nil)
    (evil-want-integration t)
    :config

    (use-package evil-args)

    (use-package evil-matchit)

    (use-package evil-magit
      :after (evil magit))

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

;; Tips for next keystroke
(when sevil-use-which-key
  (use-package which-key
    :hook (after-init . which-key-mode)
    :custom
    (which-key-idle-delay 0.5)
    (which-key-add-column-padding 1)))

(provide 'init-evil)
;;; init-evil.el ends here
