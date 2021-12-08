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
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-checkers '(c/c++-clang
                            dockerfile-hadolint
                            emacs-lisp
                            emacs-lisp-checkdoc
                            go-build
                            go-test
                            json-jq
                            lua
                            markdown-markdownlint-cli
                            protobuf-protoc
                            python-mypy
                            sh-shellcheck
                            sh-zsh
                            systemd-analyze
                            yaml-jsyaml
                            ))
  )

(use-package company
  :hook (after-init . global-company-mode)
  :bind ("M-/" . company-complete)
  :custom
  (company-dabbrev-code-ignore-case nil)
  (company-dabbrev-code-everywhere t)
  :config
  (setq company-tooltip-align-annotations t ;; aligns annotation to the right
        company-minimum-prefix-length 1
        company-require-match 'company-explicit-action-p
        company-tooltip-limit 12
        company-tooltip-width-grow-only t
        company-backends '(company-files
                           company-cmake
                           company-capf
                           company-ispell
                           (company-dabbrev-code company-keywords company-etags)
                           company-dabbrev
                           ))
  )

(use-package lsp-mode
  :hook (((web-mode json-mode go-mode dockerfile-mode c-mode c++-mode lua-mode
                    css-mode sh-mode yaml-mode sql-mode nginx-mode) . lsp-deferred)
         ((go-mode c++-mode c-mode) . lsp-save-hooks)
         (lsp-mode . lsp-enable-which-key-integration)
         (dired-mode . lsp-dired-mode))
  :config
  (setq lsp-restart 'auto-restart
        lsp-auto-guess-root t
        lsp-semantic-tokens-enable t
        lsp-dired-mode t
        )

  (defun lsp-save-hooks ()
    (add-hook 'before-save-hook 'lsp-format-buffer t t)
    (add-hook 'before-save-hook 'lsp-organize-imports t t))

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

  (use-package lsp-diagnostics
    :ensure nil
    :config
    (setq lsp-diagnostics-disabled-modes '(go-mode
                                           sh-mode))
    )

  (use-package lsp-lua
    :ensure nil
    :config
    (setq lsp-clients-lua-language-server-command "lua-language-server"
          lsp-lua-hint-enable t)
    )

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "lua-language-server")
    :major-modes '(lua-mode)
    :priority 0
    :server-id 'lua-language-server)
   )
  )

(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets)
  )

(provide 'init-lsp)

;;; init-lsp.el ends here
