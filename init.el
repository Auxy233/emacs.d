;;; init.el --- initialize all emacs settings

;;; Commentary:
;; The startup file
;; Emacs config by Galois Neko :P
;; Special thanks to seagle0128, purcell, and doom-emacs

;;; Code:
;; version checking
(when (version< emacs-version "27")
  (error "This Emacs is too old to support"))

;; comment out for debugging
(setq debug-on-error t)

;; initialize straight
(defvar bootstrap-version)

(setq straight-vc-git-default-clone-depth 1
      straight-check-for-modifications nil
      straight-use-package-by-default t
      straight-recipes-gnu-elpa-use-mirror t)

;; loading bootstrap file
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; should set before loading `use-package'
(eval-and-compile
  (setq use-package-defaults nil)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

;; use `use-package' macro to beautify config
(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))

;; required by `use-package'
(use-package diminish)
(use-package bind-key)

;; profiler
(use-package esup
  :defer 1
  :commands (esup))

;; load the customized config
;; core for core utility such as UI and completions
;; lang for programming language settings
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lang" user-emacs-directory))

;; initialize constants and helper functions
(require 'init-const)
(require 'init-boot)

;;; better editing
(require 'init-evil)
(require 'init-edit)
(require 'init-ivy)
(require 'init-persp)

;;; better UI
(require 'init-ui)
(require 'init-highlight)
(require 'init-window)
(require 'init-treemacs)

;;; better integration
(require 'init-eshell)
(require 'init-shell)
(require 'init-note)
(require 'init-dev)
(require 'init-lsp)
(require 'init-util)

;;; programming languages environment
(sevil-load-lang cpp)
(sevil-load-lang python)
(sevil-load-lang coq)
(sevil-load-lang ocaml)
(sevil-load-lang racket)
(sevil-load-lang clisp)
(sevil-load-lang idris)
(sevil-load-lang rust)
(sevil-load-lang haskell)
(sevil-load-lang latex)
;;; init.el ends here
