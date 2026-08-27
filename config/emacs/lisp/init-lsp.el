;;; init-lsp.el --- lsp -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Language Server Protocol configurations.
;;
;;; Code:

;; LSP servers:
;; npm i -g vscode-langservers-extracted bash-language-server
;;
;; Flymake linters:
;; brew install hadolint shellcheck jq
;; npm i -g markdownlint-cli eslint js-yaml

(use-package flymake
  :ensure nil
  :hook
  (prog-mode . flymake-mode)
  :bind
  (("C-c n" . flymake-goto-next-error)
   ("C-c p" . flymake-goto-prev-error))
  :custom
  (flymake-show-diagnostics-at-end-of-line t)
  )

;; flymake linter
(use-package flymake-collection
  :hook
  (flymake-mode . flymake-collection-hook-setup)
  )

(use-package flyover
  :hook
  (flymake-mode . flyover-mode)
  :custom
  (flyover-checkers '(flymake))
  )

(use-package corfu
  :hook
  ((after-init . global-corfu-mode)
   (global-corfu-mode . corfu-popupinfo-mode)
   (global-corfu-mode . corfu-history-mode))
  :custom
  (corfu-cycle t)                ;; 循环选择候选项
  (corfu-auto t)                 ;; 自动弹出补全
  (corfu-auto-delay 0.1)         ;; 轻微延迟，减少无效补全请求
  (corfu-auto-prefix 2)          ;; 输入 2 个字符触发，减少噪音
  (corfu-preselect 'prompt)      ;; 预选提示
  (corfu-on-exact-match nil)     ;; 精确匹配时不自动补全
  )

(use-package nerd-icons-corfu
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  )

;; Add extensions
(use-package cape
  :init
  ;; 只添加常用且轻量的全局补全源
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  :config
  ;; 将其余补全源仅添加到相关 mode
  (defun my/cape-setup-elisp ()
    (setq-local completion-at-point-functions
                (append completion-at-point-functions
                        (list #'cape-elisp-symbol #'cape-elisp-block))))
  (defun my/cape-setup-prog ()
    (setq-local completion-at-point-functions
                (append completion-at-point-functions
                        (list #'cape-keyword))))
  (add-hook 'emacs-lisp-mode-hook #'my/cape-setup-elisp)
  (add-hook 'prog-mode-hook #'my/cape-setup-prog)
  )

(use-package eglot
  :ensure nil
  :hook
  (((json-ts-mode go-ts-mode dockerfile-ts-mode c-ts-mode c++-ts-mode cmake-ts-mode lua-ts-mode bash-ts-mode yaml-pro-ts-mode protobuf-ts-mode graphviz-dot-mode markdown-ts-mode mhtml-ts-mode) . eglot-ensure))
  :bind
  (:map eglot-mode-map
        ("C-c e a" . eglot-code-actions)
        ("C-c e r" . eglot-rename)
        ("C-c e f" . eglot-format))
  :custom
  (eglot-report-progress nil)
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs '(graphviz-dot-mode . ("dot-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(protobuf-ts-mode . ("buf" "lsp")))
  (add-to-list 'eglot-server-programs '(cmake-ts-mode . ("neocmakelsp" "--stdio")))
  (add-to-list 'eglot-server-programs '((c++-ts-mode c-ts-mode) . ("clangd"
                                                                   "-j=5"
                                                                   "--background-index"
                                                                   "--clang-tidy"
                                                                   "--compile-commands-dir=build"
                                                                   "--completion-style=detailed"
                                                                   "--pch-storage=disk"
                                                                   "--all-scopes-completion"
                                                                   "--header-insertion=iwyu"
                                                                   "--header-insertion-decorators")))
  )

;; Configure Tempel
(use-package tempel
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.  `tempel-expand'
    ;; only triggers on exact matches. We add `tempel-expand' *before* the main
    ;; programming mode Capf, such that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand completion-at-point-functions))

    ;; Alternatively use `tempel-complete' if you want to see all matches.  Use
    ;; a trigger prefix character in order to prevent Tempel from triggering
    ;; unexpectly.
    ;; (setq-local corfu-auto-trigger "/"
    ;;             completion-at-point-functions
    ;;             (cons (cape-capf-trigger #'tempel-complete ?/)
    ;;                   completion-at-point-functions))
  )

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  )

;; Optional: Add tempel-collection if you want ready-made templates.
(use-package tempel-collection
  :after tempel)

(provide 'init-lsp)

;;; init-lsp.el ends here
