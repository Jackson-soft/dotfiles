;;; init.el --- Emacs configurations.  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;;; initialize configuration
;;
;;; Code:

;; Load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)) ;; 存放使用编辑器接口产生的配置信息

;; elpa
(require 'init-elpa)

;; Preferences
(require 'init-basic)
(require 'init-ui)
(require 'init-edit)
(require 'init-highlight)

(require 'init-buffer)
(require 'init-completion)
(require 'init-vcs)
(require 'init-dired)

;; dev
(require 'init-prog)
(require 'init-lsp)

;; language
(require 'init-go)
(require 'init-cpp)
(require 'init-org)
(require 'init-shell)
(require 'init-script)
(require 'init-emacs-lisp)

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
