;;; init-lsp.el --- The completion engine and language server
;;; Commentary:
;; handy coniguration for different PL, use it as much as possible

;;; Code:
(require 'init-const)

(use-package company
  :diminish
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :bind (("M-/" . my/company-complete)
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . my/company-yasnippet)
         ;; ("C-c C-y" . my-company-yasnippet)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :preface
  (defun my/company-yasnippet ()
    "Hide the current completeions and show snippets."
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))
  (defun my/company-complete ()
    "Bring up the completion popup. If only one result, complete it."
    (interactive)
    (require 'company)
    (when (ignore-errors
            (/= (point)
                (cdr (bounds-of-thing-at-point 'symbol))))
      (save-excursion (insert " ")))
    (when (and (company-manual-begin)
               (= company-candidates-length 1))
      (company-complete-common)))
  (setq
   company-tooltip-align-annotations t
   company-tooltip-limit 12
   company-idle-delay 0
   company-echo-delay (if (display-graphic-p) nil 0)
   company-minimum-prefix-length 3
   company-require-match nil
   company-dabbrev-ignore-case nil
   company-show-numbers nil
   company-dabbrev-downcase nil
   company-global-modes '(not
                          erc-mode message-mode help-mode
                          gud-mode eshell-mode shell-mode
                          company-backends '(company-capf company-files)
                          company-frontends '(company-pseudo-tooltip-frontend
                                              company-echo-metadata-frontend))))

;; Better sorting and filtering
(use-package company-prescient
  :hook ((company-mode . company-prescient-mode)
         (company-mode . prescient-persist-mode)))


;; Popup documentation for completion candidates
(when (and (not emacs/>=26p) (display-graphic-p))
  (use-package company-quickhelp
    :defines company-quickhelp-delay
    :bind (:map company-active-map
                ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
    :hook (global-company-mode . company-quickhelp-mode)))

(use-package lsp-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :commands lsp
  :custom
  (lsp-enable-folding nil) ;; use hideshow
  (lsp-enable-text-document-color nil)
  (lsp-log-io nil)
  (lsp-enable-indentation nil)
  (lsp-enable-links nil)
  (lsp-prefer-capf t))

(use-package lsp-ui
  :commands (lsp-ui-mode lsp-ui-peek-find-definistions lsp-ui-peek-find-references)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-ivy
  :after lsp-mode
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
              ("C-s-." . lsp-ivy-global-workspace-symbol)))

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :diminish
  :functions dap-hydra/nil
  :bind (:map lsp-mode-map
              ("<f5>" . dap-debug)
              ("M-<f5>" . dap-hydra))
  :hook ((python-mode . (lambda () (require 'dap-python)))
         ((c-mode c++-mode objc-mode swift) . (lambda () (require 'dap-lldb)))))

(provide 'init-lsp)
;;; init-lsp.el ends here
