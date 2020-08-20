;;; init-const.el --- constatn might be used later
;;; Commentary:
;; Some constants which might be used later

;;; Code:
;; customized constants
(defgroup sevil-setting nil
  "My Emacs config setting"
  :group 'convenience)

(defcustom sevil-font-name "Fira Code"
  "My font name."
  :type '(string)
  :group 'sevil-setting)

(defcustom sevil-font-size "13"
  "My font size."
  :group 'sevil-setting
  :type '(integer))

(defcustom sevil-use-linum nil
  "Show line number."
  :group 'sevil-setting
  :type '(boolean))

(defcustom sevil-use-which-key t
  "Show key hints."
  :group 'sevil-setting
  :type '(boolean))

(defcustom sevil-language-list
  '(clisp
    coq
    cpp
    haskell
    idris
    latex
    ocaml
    python
    racket
    rust)
  "The list of languages tocustom-file."
  :group 'sevil-setting
  :type '(list symbol))

(defcustom sevil-swap-colon t
  "Swap ; and : usage."
  :group 'sevil-setting
  :type '(boolean))

;; helper constants
(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defmacro sevil-load-lang (lang)
  "Load a LANG when it's in language list."
  (when (member lang sevil-language-list)
    (let ((lang-file
           (intern
            (concat "init-" (symbol-name lang)))))
      `(require ',lang-file))))

;; check if a font exits
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun check-module (list)
  "Check if a module installed cooreponding LIST."
  (let ((module-name (first list))
        (dependencies (rest list)))
    (seq-remove (lambda (e) (executable-find e)) dependencies)))

(defun sevil-update-all ()
  "Update all the pacakges."
  (interactive
   (progn
     (straight-pull-all)
     (straight-rebuild-all))))

(defun sevil/font-info ()
  "Get the current font name with size."
  (concat sevil-font-name " " sevil-font-size))

;; load custom file
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))

(unless (file-exists-p custom-file)
  (copy-file
   (expand-file-name "sample-custom.el" user-emacs-directory)
   custom-file))

(load-file custom-file)

(provide 'init-const)
;;; init-const.el ends here
