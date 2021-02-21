;;; custom.el --- sample custom

;;; Commentary:
;; customize your Emacs

;;; Code:
(setq
 sevil-scheme '(chez)
 sevil-font-name "JetBrains Mono" ; default font
 sevil-font-size "10" ; font size
 sevil-use-linum nil ; show line number in side bar
 sevil-use-which-key t ; show key hints
 sevil-use-evil nil ; use evil
 sevil-lisp-compiler "sbcl"
 sevil-language-list ; the langauges you want to enable
 '(agda clisp coq cpp haskell idris latex ocaml racket rust erlang))

(setq debug-on-error nil)

(provide 'custom.el)
;;; custom.el ends here
