;;; init-cpp.el --- C/C++ ---; -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; C/C++ configurations.
;;
;;; Code:

;; C/C++ mode
(use-package cc-mode
  :ensure nil
  :config
  (setq-default c-basic-offset tab-width
                c-doc-comment-style '((c-mode . doxygen)
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

;; cmake mode
(use-package cmake-mode)

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate)
  )

;; cmake-ide
(use-package cmake-ide
  :hook (cmake-mode . cmake-ide-setup)
  )

;; Parser generator
(use-package bison-mode)

(provide 'init-cpp)

;;; init-cpp.el ends here
