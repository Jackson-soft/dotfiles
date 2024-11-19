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

(use-package toggle-term
  :bind (("M-o f" . toggle-term-find)
         ("M-o t" . toggle-term-term)
         ("M-o v" . toggle-term-vterm)
         ("M-o a" . toggle-term-eat)
         ("M-o s" . toggle-term-shell)
         ("M-o e" . toggle-term-eshell)
         ("M-o i" . toggle-term-ielm)
         ("M-o o" . toggle-term-toggle))
  :config
  (setq toggle-term-size 25
        toggle-term-switch-upon-toggle t)
  )

(provide 'init-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
