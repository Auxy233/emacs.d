;;; init-python.el --- Python, try avoiding it
;;; Commentary:
;; require `pyflakes', `autopep8', `pyls', `yapf'

;;; Code:
(use-package python
  :ensure nil
  :hook
  (inferior-python-mode .
                        (lambda ()
                          (process-query-on-exit-flag
                           (get-process "Python"))))
  (python-mode . lsp-deferred)
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :config
  ;; default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))
  ;; Live Coding in Python
  (use-package live-py-mode)

  ;; format using YAPF
  (use-package yapfify
    :diminish yapf-mode
    :hook (python-mode . yapf-mode)))

(provide 'init-python)
;;; init-python.el ends here
