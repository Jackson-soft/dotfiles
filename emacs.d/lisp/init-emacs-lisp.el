;; init-emacs-lisp.el --- Initialize Emacs Lisp configurations. -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs Lisp configurations.
;;

;;; Code:

;; A better *Help* buffer
(use-package helpful
  :bind (([remap describe-function] . helpful-callable)  ;; C-h f
		 ("C-h C" . helpful-command)
		 ([remap describe-key] . helpful-key)  ;; C-h k
		 ([remap describe-variable] . helpful-variable)  ;; C-h v
		 ([remap describe-symbol] . helpful-symbol)
		 :map emacs-lisp-mode-map
		 ("C-c C-d" . helpful-at-point))
  :hook (helpful-mode . cursor-sensor-mode)
  )

(provide 'init-emacs-lisp)

;;; init-emacs-lisp.el ends here
