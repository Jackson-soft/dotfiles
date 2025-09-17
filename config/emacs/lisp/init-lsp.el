;;; init-lsp.el --- lsp -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Language Server Protocol configurations.
;;
;;; Code:

;; lsp tools
;; npm i -g vscode-langservers-extracted
;; npm i -g bash-language-server

;; company-ispell dictionary
;; sudo pacman -S words

;; flycheck tools
;; html -> sudo pacman -S tidy
;; json -> brew install jq
;; javascript/typescritp -> sudo npm i -g eslint
;; markdown -> sudo npm i -g markdownlint-cli
;; python3 -> sudo python3 -m pip install -U mypy
;; yaml -> sudo npm i -g js-yaml
;; dockerfile -> brew install hadolint
;; shell -> brew install shellcheck

(use-package flymake
  :ensure nil
  :hook
  (prog-mode . flymake-mode)
  :bind
  (("M-n" . flymake-goto-next-error)
   ("M-p" . flymake-goto-prev-error))
  :custom
  (flymake-show-diagnostics-at-end-of-line t)
  )

;; flymake linter
(use-package flymake-collection
  :hook
  (flymake-mode . flymake-collection-hook-setup)
  )

(use-package flyover
  :diminish
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
  (corfu-auto-delay 0.0)         ;; 无延迟
  (corfu-auto-prefix 1)          ;; 输入 1 个字符就触发
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
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  )

(use-package eglot
  :ensure nil
  :hook
  (((json-ts-mode go-ts-mode dockerfile-ts-mode c-ts-mode c++-ts-mode cmake-ts-mode lua-ts-mode bash-ts-mode yaml-pro-ts-mode protobuf-ts-mode graphviz-dot-mode markdown-mode) . eglot-ensure))
  :bind
  (:map eglot-mode-map
		("C-c e a" . eglot-code-actions)
		("C-c e r" . eglot-rename)
		("C-c e f" . eglot-format))
  :custom
  (eglot-report-progress nil)
  (eglot-autoshutdown t)
  (eglot-stay-out-of '(company))
  (eglot-ignored-server-capabilities '(:documentLinkProvider
									   :hoverProvider
									   :inlayHintProvider
									   :documentOnTypeFormattingProvider))
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

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (use-package yasnippet-snippets)
  )

(provide 'init-lsp)

;;; init-lsp.el ends here
