;;; init-haskell.el --- Hasekll
;;; Commentary:
;; require `HLS'

;;; Code:

(use-package haskell-mode
  :hook
  ((haskell-mode . haskell-indentation-mode)
   (haskell-mode . lsp-deferred))
  :init
  (unless (fboundp 'align-rules-list)
    (defvar align-rules-list nil))
  (add-to-list 'align-rules-list
               '(haskell-types
                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-assignment
                 (regexp . "\\(\\s-+\\)=\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-arrows
                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))

  :bind (:map haskell-mode-map
         ("C-c h"   . hoogle)
         ("C-o"     . open-line)
         ("C-c C-c" . haskell-compile)
         :map haskell-cabal-mode-map
         ("C-c h"   . hoogle)
         ("C-c C-c" . haskell-compile))
  :custom
  (haskell-compile-cabal-build-command "stack build --ghc-options=-Wall")
  (haskell-compile-command "ghc -dynamic -Wall -ferror-spans -fforce-recomp -c %s")
  (haskell-process-suggest-haskell-docs-imports t)
  (haskell-process-suggest-remove-import-lines  t)
  (haskell-process-suggest-hoogle-imports       t)
  (haskell-tags-on-save                         t)
  (inhibit-startup-screen                       t))

(use-package lsp-haskell
  :hook (haskell-mode . lsp)
  :after (lsp haskell-mode)
  :custom
  (lsp-haskell-process-path-hie "haskell-language-server-wrapper"))

(provide 'init-haskell)
;;; init-haskell ends here
