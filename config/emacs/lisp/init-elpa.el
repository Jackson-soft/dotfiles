;;; init-elpa.el --- elpa   -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;;; ELPA configurations.
;;
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; (package-initialize)

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t
		use-package-expand-minimally t
		use-package-compute-statistics t
		use-package-enable-imenu-support t)
  (require 'use-package))

(use-package diminish)

;; OSX GUI 下自动导入 PATH
(use-package exec-path-from-shell
  :when (eq system-type 'darwin)
  :hook (after-init . exec-path-from-shell-initialize)
  )

(provide 'init-elpa)

;;; init-elpa.el ends here
