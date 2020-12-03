;;; custom.el --- sample custom

;;; Commentary:
;; customize your Emacs

;;; Code:
(setq
 sevil-font-name "Fira Code" ; default font
 sevil-font-size "13" ; font size
 sevil-use-linum t ; show line number in side bar
 sevil-use-which-key t ; show key hints
 sevil-use-evil nil ; use evil
 sevil-lisp-compiler "sbcl"
 sevil-language-list ; the langauges you want to enable
 '(agda clisp coq cpp haskell idris latex ocaml racket rust erlang cogent))

(provide 'custom.el)
;;; custom.el ends here
