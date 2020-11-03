;;; init-util.el --- some utilities for better interactions
;;; Commentary:
;;
;;; Code:

;; Emacs structures opened files via buffer
;; ibuffer can manage them (like tab management)
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :hook ((ibuffer-mode . ibuffer-auto-mode)
         (ibuffer-mode . (lambda ()
                           (ibuffer-switch-to-saved-filter-groups "Default"))))
  :config
  (use-package ibuffer-projectile
    :hook ((ibuffer . (lambda ()
                        (ibuffer-projectile-set-filter-groups)
                        (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic))))))
  :custom
  (ibuffer-expert t)
  (ibuffer-movement-cycle nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filter-groups
   '(("Default"
      ("Emacs" (or (name . "\\*scratch\\*")
                   (name . "\\*dashboard\\*")
                   (name . "\\*compilation\\*")
                   (name . "\\*Backtrace\\*")
                   (name . "\\*Packages\\*")
                   (name . "\\*Messages\\*")
                   (name . "\\*Customize\\*")))
      ("Prog" (derived-mode . prog-mode))
      ("REPL" (or (mode . gnuplot-comint-mode)
                  (mode . inferior-emacs-lisp-mode)
                  (mode . inferior-python-mode)))
      ("Text" (and (derived-mode . text-mode)
                   (not (starred-name))))
      ("Term" (or (mode . vterm-mode)
                  (mode . term-mode)
                  (mode . shell-mode)
                  (mode . eshell-mode)))
      ("Config" (or (mode . yaml-mode)
                    (mode . conf-mode)))
      ("Mail" (or (mode . message-mode)
                  (mode . bbdb-mode)
                  (mode . mail-mode)
                  (mode . mu4e-compose-mode)))
      ("Images" (or (mode . image-mode)
                    (mode . image-dired-display-image-mode)
                    (mode . image-dired-thumbnail-mode)))
      ("Dired" (mode . dired-mode))
      ("Magit" (name . "magit"))
      ("IRC" (or (mode . rcirc-mode)
                 (mode . erc-mode)))
      ("EBrowse" (or (mode . ebrowse-tree-mode)
                     (mode . ebrowse-member-mode)))
      ("News" (or (name . "\\*newsticker\\*")
                  (name . "\\*elpher\\*")))
      ("Help" (or (name . "\\*Help\\*")
                  (name . "\\*Apropos\\*")
                  (name . "\\*info\\*")))))))

;; directory operations
(use-package dired
  :ensure nil
  :straight nil
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode)
              ("C-c +" . dired-create-empty-file))
  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-auto-revert-buffer t)
  :config
  (when sys/macp
    ;; suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil))

  (when (or (and sys/macp (executable-find "gls"))
            (and (not sys/macp) (executable-find "ls")))
    ;; using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)

    ;; show directory first
    (setq dired-listing-switches "-alh --group-directories-first")

    ;; quick sort dired buffers via hydra
    (use-package dired-quick-sort
      :bind (:map dired-mode-map)))

  ;; show git info in dired
  (use-package dired-git-info
    :bind (:map dired-mode-map
                (")" . dired-git-info-mode)))

  ;; allow rsync from dired buffers
  (use-package dired-rsync
    :bind (:map dired-mode-map
                ("C-c C-r" . dired-rsync)))

  ;; colourful dired
  (use-package diredfl
    :hook (dired-mode . diredfl-mode))

  ;; extra Dired functionality
  (use-package dired-aux :ensure nil :straight nil)
  (use-package dired-x
    :ensure nil
    :straight nil
    :hook (dired-mode . dired-omit-mode)
    :demand
    :custom
    (dired-omit-files (rx (or ".git" ".svn"
                              ".cache"
                              ".ccls-cache" ".clangd"
                              ".elc" ".pyc" ".o" ".swp")))))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))

;; kill & mark things easily
(use-package easy-kill-extras
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp] . easy-mark-sexp)
   ([remap mark-word] . easy-mark-word)

   ;; Integrate `zap-to-char'
   ([remap zap-to-char] . easy-mark-to-char)
   ([remap zap-up-to-char] . easy-mark-up-to-char)

   ;; Integrate `expand-region'
   :map easy-kill-base-map
   ("o" . easy-kill-er-expand)
   ("i" . easy-kill-er-unexpand))
  :init
  (setq kill-ring-max 200
        save-interprogram-paste-before-kill t ; Save clipboard contents before replacement
        easy-kill-alist '((?w word           " ")
                          (?s sexp           "\n")
                          (?l list           "\n")
                          (?f filename       "\n")
                          (?d defun          "\n\n")
                          (?D defun-name     " ")
                          (?e line           "\n")
                          (?b buffer-file-name)

                          (?^ backward-line-edge "")
                          (?$ forward-line-edge "")
                          (?h buffer "")
                          (?< buffer-before-point "")
                          (?> buffer-after-point "")
                          (?f string-to-char-forward "")
                          (?F string-up-to-char-forward "")
                          (?t string-to-char-backward "")
                          (?T string-up-to-char-backward ""))))

;; read pdf handy
;; only recomment under linux, others are too slow
(use-package pdf-tools
  :defer t
  :config
  (setq-default pdf-view-display-size 'fit-width)
  :bind (:map pdf-view-mode-map
   ("<s-spc>" . pdf-view-scroll-down-or-next-page)
   ("g"  . pdf-view-first-page)
   ("G"  . pdf-view-last-page)
   ("l"  . image-forward-hscroll)
   ("h"  . image-backward-hscroll)
   ("j"  . pdf-view-next-page)
   ("k"  . pdf-view-previous-page)
   ("e"  . pdf-view-goto-page)
   ("u"  . pdf-view-revert-buffer)
   ("al" . pdf-annot-list-annotations)
   ("ad" . pdf-annot-delete)
   ("aa" . pdf-annot-attachment-dired)
   ("am" . pdf-annot-add-markup-annotation)
   ("at" . pdf-annot-add-text-annotation)
   ("y"  . pdf-view-kill-ring-save)
   ("i"  . pdf-misc-display-metadata)
   ("s"  . pdf-occur)
   ("b"  . pdf-view-set-slice-from-bounding-box)
   ("r"  . pdf-view-reset-slice)))

(provide 'init-util)
;;; init-util.el ends here
