;;; init-util.el --- some utilities for better interactions
;;; Commentary:
;;
;;; Code:

;; Emacs structures opened files via buffer
;; ibuffer can manage them (like tab management)
(use-package ibuffer
  :ensure nil
  :functions my-ibuffer-find-file
  :commands (ibuffer-find-file
             ibuffer-current-buffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  ;; display buffer icons on GUI
  (with-eval-after-load 'counsel
    (defun my-ibuffer-find-file ()
      (interactive)
      (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                 (if (buffer-live-p buf)
                                     (with-current-buffer buf
                                       default-directory)
                                   default-directory))))
        (counsel-find-file default-directory)))
    (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))

  ;; group ibuffer's list by project root
  (use-package ibuffer-projectile
    :functions ibuffer-do-sort-by-alphabetic
    :hook ((ibuffer . (lambda ()
                        (ibuffer-projectile-set-filter-groups)
                        (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic)))))))

;; directory operations
(use-package dired
  :ensure nil
  :straight nil
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

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
      :init (general-define-key :keymaps dired-mode-map)))

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
  (general-define-key
   :keymaps pdf-view-mode-map
   "<s-spc>" 'pdf-view-scroll-down-or-next-page
   "g"  'pdf-view-first-page
   "G"  'pdf-view-last-page
   "l"  'image-forward-hscroll
   "h"  'image-backward-hscroll
   "j"  'pdf-view-next-page
   "k"  'pdf-view-previous-page
   "e"  'pdf-view-goto-page
   "u"  'pdf-view-revert-buffer
   "al" 'pdf-annot-list-annotations
   "ad" 'pdf-annot-delete
   "aa" 'pdf-annot-attachment-dired
   "am" 'pdf-annot-add-markup-annotation
   "at" 'pdf-annot-add-text-annotation
   "y"  'pdf-view-kill-ring-save
   "i"  'pdf-misc-display-metadata
   "s"  'pdf-occur
   "b"  'pdf-view-set-slice-from-bounding-box
   "r"  'pdf-view-reset-slice))

(provide 'init-util)
;;; init-util.el ends here
