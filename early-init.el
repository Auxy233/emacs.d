;;; early-init.el --- pre-initialize setting

;;; Commentary:
;; introduced in Emacs 27
;; only use it for internal setting, no pacakge init allowed here

;;; Code:
;; defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; disable pacakge for speed up
(setq package-enable-at-startup nil)

;; suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

;; explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

(setq locale-coding-system 'utf-8
      default-process-coding-system '(utf-8 . utf-8))

;; mouse & smooth Scroll
;; scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))

(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

;; shorter options
(fset 'yes-or-no-p 'y-or-n-p)

;; better gui
(push '(menu-bar-lines . 0) default-frame-alist) ;; remove mini menu
(push '(tool-bar-lines . 0) default-frame-alist) ;; remove tool icon
(push '(vertical-scroll-bars) default-frame-alist) ;; no scroll bar
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
;;; early-init ends here
