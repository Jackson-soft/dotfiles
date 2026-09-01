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
  ;; Emacs 31: 内置测试命令的 flags，等效于原 gotest 的 verbose + -count=1
  (go-ts-mode-test-flags '("-v" "-count=1"))
  :bind
  (:map go-ts-mode-map
        ;; Emacs 31 内置测试命令，替代 gotest 的 go-test-current-{test,file,project}
        ("C-c g t" . go-ts-mode-test-function-at-point)
        ("C-c g f" . go-ts-mode-test-this-file)
        ("C-c g p" . go-ts-mode-test-this-package)
        )
  )

(use-package gotest
  :bind
  (:map go-ts-mode-map
        ("C-c g b" . go-test-current-benchmark)
        ("C-c g x" . go-run)
        )
  )

(use-package go-tag
  :ensure nil
  :bind
  (:map go-ts-mode-map
        ("C-c g a" . go-tag-add)
        ("C-c g r" . go-tag-remove)
        )
  :custom
  (go-tag-args (list "-transform" "camelcase"))
  )

;; go.work 文件（Emacs 31 内置）
(use-package go-work-ts-mode
  :ensure nil
  :mode "go\\.work\\'"
  )

(provide 'init-go)

;;; init-go.el ends here
