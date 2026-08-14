;;; init-go.el --- golang ---  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Golang configurations.
;;
;;; Code:

(use-package go-ts-mode
  :ensure nil
  :custom
  (go-ts-indent-offset tab-width)
  :config
  (use-package gotest
    :bind
    (:map go-ts-mode-map
          ("C-c g f" . go-test-current-file)
          ("C-c g t" . go-test-current-test)
          ("C-c g p" . go-test-current-project)
          ("C-c g b" . go-test-current-benchmark)
          ("C-c g x" . go-run))
    :custom
    (go-test-verbose t)
    (go-test-args "-count=1")
    )

  (use-package go-tag
    :ensure nil
    :bind
    (:map go-ts-mode-map
          ("C-c g a" . go-tag-add)
          ("C-c g r" . go-tag-remove))
    :custom
    (go-tag-args (list "-transform" "camelcase"))
    )
  )

;; go.work 文件（Emacs 31 内置）
(use-package go-work-ts-mode
  :ensure nil
  :mode "go\\.work\\'"
  )

(provide 'init-go)

;;; init-go.el ends here
