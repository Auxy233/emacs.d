;;; init-persp.el --- Load back files
;;; Commentary:
;; Workspace for Emacs.

;;; Code:
(use-package persp-mode
  :diminish
  :defines (recentf-exclude ivy-ignore-buffers ivy-sort-functions-alist)
  :commands (get-current-persp persp-contain-buffer-p)
  :hook ((after-init . persp-mode))
  :init
  (setq-default persp-keymap-prefix (kbd "C-c p"))
  (setq
   persp-nil-name "default"
   persp-set-last-persp-for-new-frames nil
   persp-kill-foreign-buffer-behaviour 'kill
   persp-auto-resume-time 0)
  :config
  (add-to-list 'persp-common-buffer-filter-functions
               (lambda (b)
                 "Ignore temporary buffers."
                 (let ((bname (file-name-nondirectory (buffer-name b))))
                   (or (string-prefix-p "*" bname)
                       (string-prefix-p ".newsrc" bname)
                       (string-prefix-p "magit" bname)
                       (string-prefix-p "Pfuture-Callback" bname)
                       (string-match-p "\\.elc\\|\\.tar\\|\\.gz\\|\\.zip\\'" bname)
                       (eq (buffer-local-value 'major-mode b) 'erc-mode)
                       (eq (buffer-local-value 'major-mode b) 'rcirc-mode)
                       (eq (buffer-local-value 'major-mode b) 'nov-mode)
                       (eq (buffer-local-value 'major-mode b) 'vterm-mode)))))

  ;; Don't save persp configs in `recentf'
  (with-eval-after-load 'recentf
    (push persp-save-dir recentf-exclude))

  ;; Ivy Integraticon
  (with-eval-after-load 'ivy
    (add-to-list 'ivy-ignore-buffers
                 #'(lambda (b)
                     (when persp-mode
                       (let ((persp (get-current-persp)))
                         (if persp
                             (not (persp-contain-buffer-p b persp))
                           nil)))))))

(provide 'init-persp)
;;; init-persp.el ends here
