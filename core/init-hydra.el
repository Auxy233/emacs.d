;;; init-hydra.el --- hydra tweaks -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package hydra
  :bind (("C-c h c" . hydra-copy/body)
         ("C-c h m" . hydra-macro/body)
         ("C-c h w" . hydra-other-window-scroll/body)))

(provide 'init-hydra)
;; init-hydra.el ends here
