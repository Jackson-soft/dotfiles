;;; init-lsp.el --- lsp -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Language Server Protocol configurations.
;;
;;; Code:

;; lsp tools
;; npm i -g vscode-css-languageserver-bin
;; npm i -g vscode-html-languageserver-bin
;; npm i -g bash-language-server

;; company-ispell dictionary
;; sudo pacman -S words

;; flycheck tools
;; html -> sudo pacman -S tidy
;; json -> brew install jq
;; javascript/typescritp -> sudo npm i -g eslint
;; lua -> sudo luarocks install luacheck
;; markdown -> sudo npm i -g markdownlint-cli markdownlint
;; python3 -> sudo python3 -m pip install -U mypy
;; yaml -> sudo npm i -g js-yaml
;; dockerfile -> brew install hadolint
;; shell -> brew install shellcheck

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-check-syntax-automatically '(save mode-enabled))
  )

(use-package company
  :hook (after-init . global-company-mode)
  :bind (("M-/" . company-complete)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ([tab]     . company-complete-common-or-cycle)
         ([backtab] . company-select-previous-or-abort)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :custom
  (company-dabbrev-code-ignore-case nil)
  (company-dabbrev-code-everywhere t)
  :config
  (setq company-tooltip-align-annotations t ;; aligns annotation to the right
        company-minimum-prefix-length 1
        company-tooltip-limit 12
        company-backends '(company-capf company-ispell
                                        company-files
                                        (company-dabbrev-code company-etags company-keywords)
                                        company-clang
                                        company-cmake
                                        ))
  )

(use-package lsp-mode
  :hook (((web-mode json-mode go-mode dockerfile-mode c-mode c++-mode
                    css-mode sh-mode yaml-mode sql-mode markdown-mode gfm-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (dired-mode . lsp-dired-mode))
  :config
  (setq lsp-keep-workspace-alive nil         ;; auto kill lsp server
        lsp-restart 'auto-restart
        lsp-auto-guess-root t
        lsp-semantic-tokens-enable t
        lsp-lens-enable t               ;; enable lens
        )

  (use-package lsp-clangd
    :ensure nil
    :config
    (setq lsp-clients-clangd-args '("-j=2"
                                    "--background-index"
                                    "--clang-tidy"
                                    "--cross-file-rename"
                                    "--completion-style=bundled"
                                    "--pch-storage=memory"
                                    "--header-insertion=iwyu"
                                    "--header-insertion-decorators"))
    )
  )

(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets)
  )

(provide 'init-lsp)

;;; init-lsp.el ends here
