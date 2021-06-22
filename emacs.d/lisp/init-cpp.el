;;; init-cpp.el --- C/C++ ---; -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; C/C++ configurations.
;;
;;; Code:

;; C/C++ mode
(use-package cc-mode
  :ensure nil
  :mode (("\\.h\\'" . c++-mode)
         ("\\.cxx\\'" . c++-mode))
  :config
  (setq-default c-basic-offset tab-width)
  (setq c-doc-comment-style '((c-mode . doxygen)
                              (c++-mode . doxygen)))
  )

;; Highlight "#if 0" as comments
(use-package hideif
  :ensure nil
  :hook ((c-mode c++-mode) . hide-ifdef-mode)
  :config
  (setq hide-ifdef-initially t
        hide-ifdef-shadow t)
  )

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode)
  )

;; cmake mode
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  )

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate)
  )

;; Parser generator
(use-package bison-mode
  :mode (("\\.l\\'" . flex-mode)
         ("\\.y\\'" . bison-mode))
  )

(provide 'init-cpp)

;;; init-cpp.el ends here
