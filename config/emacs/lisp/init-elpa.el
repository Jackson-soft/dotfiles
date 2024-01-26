;;; init-elpa.el --- elpa   -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;;; ELPA configurations.
;;
;;; Code:

;; Should set before loading `use-package'
(setq use-package-always-ensure t
	  use-package-expand-minimally t
	  use-package-compute-statistics t
	  use-package-enable-imenu-support t)

;; packages
(use-package package
  :ensure nil
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  )

(use-package diminish)

;; OSX GUI 下自动导入 PATH

(use-package exec-path-from-shell
  :init(exec-path-from-shell-initialize)
  )

(provide 'init-elpa)

;;; init-elpa.el ends here
