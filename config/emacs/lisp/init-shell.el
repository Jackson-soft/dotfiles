;;; init-shell.el --- Initialize eshell configurations.  -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Eshell configurations.
;;
;;; Code:

(use-package sh-script
  :ensure nil
  :mode (("\\.zsh\\'" . bash-ts-mode)
		 ("\\.sh\\'" . bash-ts-mode)
		 ("\\.*shrc\\'" . bash-ts-mode)
		 ("\\.zshenv\\'" . bash-ts-mode))
  :bind (:map sh-mode-map
			  ("C-c C-e" . sh-execute-region))
  )

(use-package vterm
  :hook (vterm-mode . (lambda() (display-line-numbers-mode -1)))
  :config
  (setq vterm-always-compile-module t)
  )

(use-package vterm-toggle
  :bind ("C-`" . vterm-toggle)
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
			   '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
				 (display-buffer-reuse-window display-buffer-in-direction)
				 (direction . bottom)
				 (dedicated . t)
				 (reusable-frames . visible)
				 (window-height . 0.3)))
  )

(provide 'init-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
