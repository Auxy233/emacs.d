;;; init-shell.el --- shell configurations
;;; Commentary:
;;

;;; Code:
(use-package shell
  :ensure nil
  :commands comint-send-string comint-simple-send comint-strip-ctrl-m
  :hook ((shell-mode . ansi-color-for-comint-mode-on)
         (shell-mode . n-shell-mode-hook)
         (comint-output-filter-functions . comint-strip-ctrl-m))
  :init
  (setq system-uses-terminfo nil)

  (defun n-shell-simple-send (proc command)
    "Various PROC COMMANDs pre-processing before sending to shell."
    (cond
     ;; Checking for clear command and execute it.
     ((string-match "^[ \t]*clear[ \t]*$" command)
      (comint-send-string proc "\n")
      (erase-buffer))
     ;; Checking for man command and execute it.
     ((string-match "^[ \t]*man[ \t]*" command)
      (comint-send-string proc "\n")
      (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
      (setq command (replace-regexp-in-string "[ \t]+$" "" command))
      ;;(message (format "command %s command" command))
      (funcall 'man command))
     ;; Send other commands to the default handler.
     (t (comint-simple-send proc command))))

  (defun n-shell-mode-hook ()
    "Shell mode customizations."
    (local-set-key '[up] 'comint-previous-input)
    (local-set-key '[down] 'comint-next-input)
    (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
    (setq comint-input-sender 'n-shell-simple-send)))

;; ANSI & XTERM 256 color support
(use-package xterm-color
  :defines (compilation-environment
            eshell-preoutput-filter-functions
            eshell-output-filter-functions)
  :functions (compilation-filter my-advice-compilation-filter)
  :init
  ;; For shell
  (setenv "TERM" "xterm-256color")
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'shell-mode-hook
            (lambda ()
              ;; Disable font-locking in this buffer to improve performance
              (font-lock-mode -1)
              ;; Prevent font-locking from being re-enabled in this buffer
              (make-local-variable 'font-lock-function)
              (setq font-lock-function (lambda (_) nil))
              (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

  ;; For eshell
  (with-eval-after-load 'esh-mode
    (add-hook 'eshell-before-prompt-hook
              (lambda ()
                (setq xterm-color-preserve-properties t)))
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions
          (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

  ;; For compilation buffers
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my-advice-compilation-filter (f proc string)
    (funcall f proc
             (if (eq major-mode 'rg-mode) ; compatible with `rg'
                 string
               (xterm-color-filter string))))
  (advice-add 'compilation-filter :around #'my-advice-compilation-filter)
  (advice-add 'gud-filter :around #'my-advice-compilation-filter)

  ;; For prolog inferior
  (with-eval-after-load 'prolog
    (add-hook 'prolog-inferior-mode-hook
              (lambda ()
                (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))))

;; Better term
;; @see https://github.com/akermu/emacs-libvterm#installation
(when (and module-file-suffix           ; dynamic module
           (executable-find "cmake")
           (executable-find "libtool")
           (executable-find "make"))
  (use-package vterm))

;; Shell Pop
(use-package shell-pop
  :bind (("<f9>" . shell-pop))
  :init
  (setq shell-pop-window-size 35
        shell-pop-shell-type
        (cond ((fboundp 'vterm) '("vterm" "*vterm*" (lambda () (vterm))))
              (t '("eshell" "*eshell*" (lambda () (eshell))))
              (t '("terminal" "*terminal*" (lambda () (term shell-pop-term-shell)))))))

(provide 'init-shell)
;;; init-shell.el ends here
