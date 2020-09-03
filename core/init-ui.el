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

;; fira code setting
(when (and (font-installed-p "Fira Code") (string= sevil-font-name "Fira Code"))
  ;; Follow fira code instruction to enable it
  ;; despite fira code font, also install this:
  ;; https://github.com/tonsky/FiraCode/issues/211#issuecomment-239058632
  (defun fira-code-mode--make-alist (list)
    "Generate prettify-symbols alist from LIST."
    (let ((idx -1))
      (mapcar
       (lambda (s)
         (setq idx (1+ idx))
         (let* ((code (+ #Xe100 idx))
                (width (string-width s))
                (prefix ())
                (suffix '(?\s (Br . Br)))
                (n 1))
           (while (< n width)
             (setq prefix (append prefix '(?\s (Br . Bl))))
             (setq n (1+ n)))
           (cons s (append prefix suffix (list (decode-char 'ucs code))))))
       list)))

  (defconst fira-code-mode--ligatures
    '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
      "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
      "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
      "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
      ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
      "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
      "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
      "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
      ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
      "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
      "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
      "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
      ))

  (defvar fira-code-mode--old-prettify-alist)

  (defun fira-code-mode--enable ()
    "Enable Fira Code ligatures in current buffer."
    (setq-local fira-code-mode--old-prettify-alist prettify-symbols-alist)
    (setq-local prettify-symbols-alist (append (fira-code-mode--make-alist fira-code-mode--ligatures) fira-code-mode--old-prettify-alist))
    (prettify-symbols-mode t))

  (defun fira-code-mode--disable ()
    "Disable Fira Code ligatures in current buffer."
    (setq-local prettify-symbols-alist fira-code-mode--old-prettify-alist)
    (prettify-symbols-mode -1))

  (define-minor-mode fira-code-mode
    "Fira Code ligatures minor mode"
    :lighter " Fira"
    (setq-local prettify-symbols-unprettify-at-point 'right-edge)
    (if fira-code-mode
        (fira-code-mode--enable)
      (fira-code-mode--disable)))

  (defun fira-code-mode--setup ()
    "Setup Fira Code Symbols"
    (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))

  ;; enable by default
  (add-hook 'prog-mode-hook 'fira-code-mode))

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
(load-theme 'doom-one-light t)

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
