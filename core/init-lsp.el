;;; init-lsp.el --- The completion engine and language server
;;; Commentary:
;; handy coniguration for different PL, use it as much as possible

;;; Code:
(require 'init-const)

(use-package popup)

(use-package company
  :diminish
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :bind (("M-/" . company-complete-selection)
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . my/company-yasnippet)
         ("<C-return>" . company-complete-selection)
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
  :init
  (setq
   company-tooltip-align-annotations t
   company-tooltip-limit 12
   company-idle-delay 0
   company-echo-delay (if (display-graphic-p) nil 0)
   company-minimum-prefix-length 3
   company-require-match nil
   company-dabbrev-ignore-case nil
   company-show-numbers t
   company-dabbrev-downcase nil
   company-global-modes '(not
                          erc-mode message-mode help-mode
                          gud-mode eshell-mode shell-mode
                          company-backends '(company-capf company-files)
                          company-frontends '(company-pseudo-tooltip-frontend
                                              company-echo-metadata-frontend))))

;; Better sorting and filtering
(use-package company-prescient
  :hook ((global-company-mode . company-prescient-mode)
         (global-company-mode . prescient-persist-mode)))

(use-package company-quickhelp
  :after company
  :hook (global-company-mode . company-quickhelp-mode)
  :init (setq company-quickhelp-delay 0.5))

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
  (lsp-prefer-capf t)
  (lsp-keymap-prefix "M-l")
  :init
  (when sevil-use-which-key
    (with-eval-after-load 'lsp-mode
      (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))))

(use-package lsp-ui
  :commands (lsp-ui-mode lsp-ui-peek-find-definistions lsp-ui-peek-find-references)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :diminish
  :after lsp
  :hook ((python-mode . (lambda () (require 'dap-python)))
         ((c-mode c++-mode objc-mode swift) . (lambda () (require 'dap-lldb)))))

(provide 'init-lsp)
;;; init-lsp.el ends here
