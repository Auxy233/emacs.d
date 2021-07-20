;;; init-edit.el --- henhanced file editing
;;; Commentary:
;; config as an editor

;;; Code:
(require 'init-const)

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package align
  :ensure nil
  :bind (("C-c a" . align-regexp)))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Work with large file
(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))

;; Click to browse URL or to send to e-mail address
(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;; Jump to things in Emacs tree-style
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("C-c C-j" . avy-resume)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :hook (after-init . avy-setup-default)
  :config (setq avy-all-windows t
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

(use-package rect
  :straight nil
  :after (hydra)
  :bind (("C-c t" . hydra-rect/body))
  :config
  (defhydra hydra-rect ()
    "rectangle"
    ("h" backward-char "←")
    ("j" next-line "↓")
    ("k" previous-line "↑")
    ("l" forward-char "→")
    ("w" copy-rectangle-as-kill "copy")
    ("y" yank-rectangle "yank")
    ("t" string-rectangle "string")
    ("d" kill-rectangle "kill")
    ("c" clear-rectangle "clear")
    ("o" open-rectangle "open")
    ("N" rectangle-number-lines "number lines")
    ("e" rectangle-exchange-point-and-mark "exchange")
    ("u" undo "undo")
    ("r" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1))
     "reset")))

(use-package newcomment
  :ensure nil
  :straight nil
  :bind
  (("C-c c l" . comment-line)
   ("C-c c d" . comment-dwim)
   ("C-c c k" . comment-kill)
   ("C-c c b" . comment-box))
  :custom
  ;; `auto-fill' inside comments
  (comment-auto-fill-only-comments t))

;; Pass a URL to a WWW browser
(use-package browse-url
  :ensure nil
  :defines dired-mode-map)

;; Kill text between the point and the character CHAR
(use-package avy-zap :after (zap))

;; Quickly follow links
(use-package ace-link
  :defines (org-mode-map
            gnus-summary-mode-map
            gnus-article-mode-map
            ert-results-mode-map))

;; Show number of matches in mode-line while searching
(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         ("M-s r" . anzu-query-replace)
         ("M-s s" . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

(use-package ediff
  :ensure nil
  :hook
  ;; restore window layout when done
  (ediff-quit . winner-undo)
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(defhydra hydra-spell ()
  ("h" backward-char "←")
  ("j" next-line "↓")
  ("k" previous-line "↑")
  ("l" forward-char "→")
  ("V" scroll-up "up")
  ("v" scroll-down "down")
  ("a" flyspell-auto-correct-word)
  ("b" flyspell-buffer "buffer")
  ("r" flyspell-region "region")
  ("n" flyspell-correct-next "next")
  ("p" flyspell-correct-previous "previous")
  ("." flyspell-correct-at-point "here")
  ("m" flyspell-correct-move "move"))

;; On-the-fly spell checker
(use-package flyspell
  :ensure nil
  :diminish
  :bind (("C-c s" . hydra-spell/body)
         ("C-c ." . flyspell-correct-at-point))
  :hook (((text-mode outline-mode) . flyspell-mode)
         ;; (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :custom 
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  :config
  (use-package flyspell-correct
    :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))
  (use-package flyspell-correct-ivy)
  (use-package flyspell-correct-popup))

;; Framework for mode-specific buffer indexes
(use-package imenu
  :bind (("C-." . imenu))
  :ensure nil)

;; Undo/Redo
(use-package undo-fu
  :bind (([remap undo] . undo-fu-only-undo)
         ([remap undo-only] . undo-fu-only-undo)
         ("C-+" . undo-fu-only-redo)))

(use-package undo-tree)

;; Preview when `goto-char'
(use-package goto-char-preview
  :bind (([remap goto-char] . goto-char-preview)))

;; Preview when `goto-line'
(use-package goto-line-preview
  :bind (([remap goto-line] . goto-line-preview)))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-; m" . mc/mark-all-like-this)
         ("C-; l" . mc/edit-lines)
         ("C-; SPC" . mc/mark-all-in-region)
         ("C-; s" . mc/sort-regions)
         ("C-; r" . mc/reverse-regions)
         ("C-; e" . mc/edit-ends-of-lines)
         ("C-; a" . mc/edit-beginnings-of-lines)))
  
;; Hideshow
(use-package hideshow
  :bind (("<C-tab>" . hs-toggle-hiding)
         ("C-c /"  . hs-hide-all)
         ("C-c \\" . hs-show-all))
  :ensure nil
  :hook (prog-mode . hs-minor-mode))

;; Narrow/Widen
(use-package fancy-narrow
  :diminish
  :hook (after-init . fancy-narrow-mode))

(use-package paredit)

(use-package dumb-jump
  :ensure t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate t)
  :bind (("M-g j" . dumb-jump-go)
         ("M-g J" . dumb-jump-go-other-window))
  :custom
  (dumb-jump-quiet t)
  (dumb-jump-aggressive t)
  (dumb-jump-selector 'ivy)
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-disable-obsolete-warnings t))

(bind-key "C-c o w" 'overwrite-mode)
(bind-key "C-c r" 'replace-regexp)

(provide 'init-edit)
;;; init-edit.el ends here
