;;; init-cpp.el --- C/C++ ---; -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; C/C++ configurations.
;;
;;; Code:

;; C/C++ mode
(use-package cc-mode
  :ensure nil
  :custom
  (c-doc-comment-style '((c-mode . doxygen)
						 (c++-mode . doxygen)))
  (c-default-style "stroustrup")
  )

(use-package c-ts-mode
  :ensure nil
  :config
  (setq c-ts-mode-indent-offset tab-width)
  )

;; Highlight "#if 0" as comments
(use-package hideif
  :ensure nil
  :hook ((c-mode c++-mode) . hide-ifdef-mode)
  :config
  (setq hide-ifdef-initially t
		hide-ifdef-shadow t)
  )

(use-package cmake-ts-mode
  :ensure nil
  :config
  (setq cmake-ts-mode-indent-offset tab-width)
  )

;; cmake-ide
(use-package cmake-ide
  :hook (cmake-mode . cmake-ide-setup)
  :bind (:map c++-mode-map
			  ("C-x c b" . cmake-ide-compile)
			  ("C-x c r" . cmake-ide-run-cmake))
  :config
  (setq cmake-ide-project-dir (project-root (project-current t))
		cmake-ide-build-dir (concat cmake-ide-project-dir "build")
		cmake-ide-cmake-args "-G Ninja -DCMAKE_EXPORT_COMPILE_COMMANDS=YES")
  )

;; Parser generator
(use-package bison-mode)

(provide 'init-cpp)

;;; init-cpp.el ends here
