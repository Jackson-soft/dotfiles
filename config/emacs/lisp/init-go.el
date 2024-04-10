;;; init-go.el --- golang ---  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Golang configurations.
;;
;;; Code:

(use-package go-ts-mode
  :ensure nil
  :config
  (setq go-ts-mode-indent-offset tab-width)

  (use-package gotest
    :bind (:map go-ts-mode-map
                ("C-c g f" . go-test-current-file)
                ("C-c g t" . go-test-current-test)
                ("C-c g p" . go-test-current-project)
                ("C-c g b" . go-test-current-benchmark)
                ("C-c g x" . go-run))
    :config
    (setq go-test-verbose t
		  go-test-args "-count=1")
    )

  (use-package go-tag
    :ensure nil
    :bind (:map go-ts-mode-map
                ("C-c g a" . go-tag-add)
                ("C-c g r" . go-tag-remove))
    :config
    (setq go-tag-args (list "-transform" "camelcase"))
    )
  )

(provide 'init-go)

;;; init-go.el ends here
