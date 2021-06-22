;;; init-python.el --- init python  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Python configurations.
;;
;;; Code:

;; sudo npm i -g pyright

(use-package python
  :ensure nil
  :config
  (setq python-shell-interpreter "python3")
  )

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  :config
  (setq lsp-pyright-python-executable-cmd "python3")
  )

(provide 'init-python)

;;; init-python.el ends here
