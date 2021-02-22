;;; init-ui.el --- initialize UI system
;;; Commentary:

;;; Code:
;; setting for emacs UI
;; beautify mac

;; adjust font according to your computer
(require 'init-const)

;; check if selected font is installed
(when (font-installed-p (sevil/font-info))
  (set-frame-font (sevil/font-info) nil t))

;; title
(setq frame-title-format '("|- %b")
      icon-title-format frame-title-format)

;; inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; themes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; HACK load it outside use-package due to some bugs
(load-theme 'doom-solarized-light t)

;; a minimum modeline, doom-modeline is too heavy
(use-package mini-modeline
  :diminish
  :custom-face
  ; use dracula color
  (mini-modeline-mode-line ((t (:background "#bd93f9" :box nil :height 0.14))))
  :config
  (mini-modeline-mode t))

;; display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

(add-hook 'window-setup-hook #'window-divider-mode)

(when sys/macp
  ;; Render thinner fonts
  (setq ns-use-thin-smoothing t)
  ;; Don't open a file in a new frame
  (setq ns-pop-up-frames nil))

(use-package hl-line
   :ensure nil
   :custom-face (hl-line ((t (:extend t))))
   :hook
   ((after-init . global-hl-line-mode)
    ((term-mode vterm-mode) . hl-line-unload-function)))

;; highlight matching parens
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :hook (after-init . global-hl-todo-mode)
  :init
  (setq hl-todo-keyword-faces
        '(("FIXME" . (face-foreground 'error))
          ("ISSUE" . (face-foreground 'error))
          ("BUG" .( (face-foreground 'error)))
          ("DEBUG" . (face-foreground 'warning))
          ("TRICK" . (face-foreground 'warning))
          ("HACK" . (face-foreground 'warning)))))

;; highlight uncommitted changes using VC
;; issue: diff-hl-flydiff greatly affects performance
(use-package diff-hl
  :ensure t
  :hook ((after-init . global-diff-hl-mode)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode         . diff-hl-dired-mode-unless-remote)))

;; pulse current line
(use-package pulse
  :ensure nil
  :preface
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (defun recenter-and-pulse (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (pulse-line))
  :hook ((counsel-grep-post-action
          dumb-jump-after-jump
          bookmark-after-jump
          imenu-after-jump
          goto-line-preview
          goto-char-preview
          pop-to-mark-command
          pop-global-mark
          goto-last-change) . recenter-and-pulse)
  :custom-face
  (pulse-highlight-start-face ((t (:inherit highlight))))
  (pulse-highlight-face ((t (:inherit highlight)))))

(provide 'init-ui)
;;; init-ui.el ends here
