;;; init-script --- script  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; script configurations.
;;
;;; Code:

(use-package flymake-sqlfluff
  :hook
  (sql-mode . flymake-sqlfluff-load)
  )

(use-package yaml-pro
  :mode
  ("\\.ya?ml\\'" . yaml-pro-ts-mode)
  )

(use-package protobuf-ts-mode
  :mode
  ("\\.proto\\'" . protobuf-ts-mode))

;; Graphviz DOT 文件（dot/gv）
(use-package graphviz-dot-mode
  :mode
  ("\\.dot\\'" . graphviz-dot-mode)
  ("\\.gv\\'"  . graphviz-dot-mode)
  )

(use-package nginx-mode
  :mode
  ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)
  )

(use-package flymake-hadolint
  :hook
  (dockerfile-ts-mode . flymake-hadolint-setup)
  )

(when (memq system-type '(gnu/linux))
  (use-package systemd :defer t)
  )

;; Emacs 31 内置的 tree-sitter HTML mode（内嵌 JS/CSS）
(use-package mhtml-ts-mode
  :ensure nil
  :mode
  ("\\.html?\\'" . mhtml-ts-mode)
  )

(use-package restclient
  :mode
  ("\\.http\\'" . restclient-mode)
  )

(provide 'init-script)

;;; init-script.el ends here
