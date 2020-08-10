;;; custom.el --- sample custom

;;; Commentary:
;; customize your Emacs

;;; Code:
(setq
 sevil-font-name "Fira Code" ; default font
 sevil-font-size "13" ; font size
 sevil-use-linum t ; show line number in side bar
 sevil-use-which-key t ; show key hints
 sevil-swap-colon t ; swap ; and : for evil
 sevil-swap-parentheses-square-bracket t ; swap () and [] keys
 sevil-language-list ; the langauges you want to enable
 '(clisp coq cpp haskell idris latex ocaml python racket rust))

(provide 'custom.el)
;;; custom.el ends here
