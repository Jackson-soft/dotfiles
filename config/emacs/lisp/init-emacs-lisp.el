;; init-emacs-lisp.el --- Initialize Emacs Lisp configurations. -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs Lisp configurations.
;;

;;; Code:

(use-package help
  :ensure nil
  :custom
  (help-window-select t)
  )

;; A better *Help* buffer
(use-package helpful
  :bind (([remap describe-function] . helpful-callable)  ;; C-h f
		 ([remap describe-command]  . helpful-command) ;; C-h x
		 ([remap describe-key] . helpful-key)  ;; C-h k
		 ([remap describe-variable] . helpful-variable)  ;; C-h v
		 ([remap describe-symbol] . helpful-symbol) ;; C-h o
		 :map emacs-lisp-mode-map
		 ("C-c C-d" . helpful-at-point))
  :hook (helpful-mode . cursor-sensor-mode)
  )

(provide 'init-emacs-lisp)

;;; init-emacs-lisp.el ends here
