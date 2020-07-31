;;; early-init.el --- pre-initialize setting

;;; Commentary:
;; introduced in Emacs 27
;; only use it for internal setting, no pacakge init allowed here

;;; Code:
;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Disable pacakge for speed up
(setq package-enable-at-startup nil)

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

;; better gui
(push '(menu-bar-lines . 0) default-frame-alist) ;; remove mini menu
(push '(tool-bar-lines . 0) default-frame-alist) ;; remove tool icon
(push '(vertical-scroll-bars) default-frame-alist) ;; no scroll bar
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
;;; early-init ends here
