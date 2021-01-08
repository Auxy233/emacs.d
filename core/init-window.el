;;; init-winodw.el --- Emacs window management
;;; Commentary:
;;

;;; Code:

;; Directional window-selection routines
(use-package windmove
  :ensure nil
  :hook (after-init . windmove-default-keybindings))

;; restore old window configurations
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :custom (winner-dont-bind-my-key nil)
  :init (setq winner-boring-buffers
              '("*Completions*"
                "*Compile-Log*"
                "*inferior-lisp*"
                "*Fuzzy Completions*"
                "*Apropos*"
                "*Help*"
                "*cvs*"
                "*Buffer List*"
                "*Ibuffer*"
                "*esh command on file*")))

;; quickly switch windows
(use-package ace-window
  :bind (([remap other-window] . ace-window)
         ("M-o" . ace-window)
         ("C-x |" . split-window-right)
         ("C-x -" . split-window-below)
         ("C-c w" . ace-window-hydra/body)))

(provide 'init-window)
;;; init-window.el ends here
