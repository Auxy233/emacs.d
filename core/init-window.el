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
  :after (hydra)
  :bind
  (([remap other-window] . ace-window)
   ("M-o" . ace-window)
   ("C-x /" . split-window-right)
   ("C-x -" . split-window-below)
   ("C-c w" . hydra-ace/body)))

;; don't know why I have to put it outside to make if effective
(defhydra hydra-ace ()
  ("TAB" other-window "switch")
  ("x" ace-delete-window "delete")
  ("m" ace-delete-other-windows "maximize")
  ("s" ace-swap-window "swap")
  ("a" ace-select-window "select")
  ("F" toggle-frame-fullscreen "fullscreen\n")
  ("b" shrink-window-horizontally "←")
  ("n" enlarge-window "↓")
  ("p" shrink-window "↑")
  ("f" enlarge-window-horizontally "→\n")
  ("u" winner-undo "undo")
  ("r" winner-redo "redo\n")
  ("|" balance-windows "balance")
  ("r" split-window-right "horizontally")
  ("R" split-window-horizontally-instead "horizon instead")
  ("v" split-window-below "vertically")
  ("V" split-window-vertically-instead "vertical instead")
  ("t" toggle-window-split "toggle\n")
  ("+" text-scale-increase "in")
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-increase 0) "reset"))

(provide 'init-window)
;;; init-window.el ends here
