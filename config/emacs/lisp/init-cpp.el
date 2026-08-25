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
  (c-doc-comment-style '((c-ts-mode . doxygen)
                         (c++-ts-mode . doxygen)))
  (c-default-style "stroustrup")
  )

(use-package c-ts-mode
  :ensure nil
  :custom
  (c-ts-indent-offset tab-width)
  ;; Emacs 31: 需配合 `treesit-language-source-alist' 中的 doxygen 语法
  (c-ts-mode-enable-doxygen t)
  )

;; Highlight "#if 0" as comments
(use-package hideif
  :ensure nil
  :hook
  ((c-ts-mode c++-ts-mode) . hide-ifdef-mode)
  :custom
  (hide-ifdef-initially t)
  (hide-ifdef-shadow t)
  )

(use-package cmake-ts-mode
  :ensure nil
  :custom
  (cmake-ts-indent-offset tab-width)
  )

(provide 'init-cpp)

;;; init-cpp.el ends here
