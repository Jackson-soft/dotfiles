;;; init-shell.el --- Initialize shell configurations.  -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Shell configurations.
;;
;;; Code:

;; \`sh-mode' → \`bash-ts-mode' 的升级交给 `treesit-enabled-modes' 统一处理
(use-package sh-script
  :ensure nil
  :mode
  ("\\.zsh\\'" . sh-mode)
  ("\\.sh\\'" . sh-mode)
  ("\\.*shrc\\'" . sh-mode)
  ("\\.zshenv\\'" . sh-mode)
  :bind
  (:map sh-mode-map
        ("C-c C-e" . sh-execute-region))
  )

(use-package ghostel
  :bind
  (("C-x m" . ghostel)
   :map ghostel-semi-char-mode-map
   ("C-s"  . consult-line)
   ("C-k"  . my/ghostel-send-C-k-and-kill)
   ;; I'm used to go up/down the shell history with M-n/p from eshell
   ;; Simulate this behavior in ghostel by sending C-p and C-n
   ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
   ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl")))
   :map project-prefix-map
   ("m" . ghostel-project)
   ("M" . ghostel-project-list-buffers))
  :config
  (defun my/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel.
Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))

  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer)))

(provide 'init-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
