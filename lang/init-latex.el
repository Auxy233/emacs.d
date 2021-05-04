;;; init-clisp.el --- latex
;;; Commentary:
;; I know, latex is not a programming language generally speaking
;; but it is turing-complete
;; require `latex' related binaries

;;; Code:
(use-package auctex
  :defer t
  :hook
  ((LaTeX-mode . visual-line-mode)
   (LaTeX-mode . flycheck-mode)
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . company-mode))
  :custom
  (preview-scale-function (quote preview-scale-from-face))
  (preview-scale-function 3)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex)
  (setq TeX-view-program-selection
        (quote (((output-dvi style-pstricks) "dvips and gv")
                (output-dvi "xdvi")
                (output-pdf "Zathura")
                (output-html "xdg-open"))))
  (setq-default TeX-master nil
                TeX-engine 'xetex)

  (use-package company-auctex
    :after (auctex company)
    :init
    (company-auctex-init))

  (use-package auctex-latexmk
    :after (auctex)
    :init
    (auctex-latexmk-setup)
    :custom
    (auctex-latexmk-inherit-TeX-PDF-mode t)))

(provide 'init-latex)
;;; init-latex.el ends here
