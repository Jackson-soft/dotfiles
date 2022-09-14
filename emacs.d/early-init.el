;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
	  gc-cons-percentage 0.5
	  read-process-output-max (* 1024 1024)
	  load-prefer-newer t
	  inhibit-compacting-font-caches t)  ;; Don’t compact font caches during GC.

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
	  frame-resize-pixelwise t)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(cursor-type . bar) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)
(push '(font . "Fira Code-17") default-frame-alist)
(when (eq system-type 'darwin)
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (push '(ns-appearance . dark) default-frame-alist)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
