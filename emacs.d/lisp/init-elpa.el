;;; init-elpa.el --- elpa   -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;;; ELPA configurations.
;;
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t
        use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; OSX GUI 下自动导入 PATH
(use-package exec-path-from-shell
  :when (eq system-type 'darwin)
  :init (exec-path-from-shell-initialize)
  )

(provide 'init-elpa)

;;; init-elpa.el ends here
