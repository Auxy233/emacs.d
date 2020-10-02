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
(setq frame-title-format '("Sweet Devil - %b")
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
(load-theme 'doom-dracula t)

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

(provide 'init-ui)
;;; init-ui.el ends here
