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
  :hook (prog-mode . flymake-mode)
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error))
  )

;; flymake linter
(use-package flymake-collection
  :hook (flymake-mode . flymake-collection-hook-setup)
  )

;; https://company-mode.github.io/manual/
;; (use-package company
;;   ;; :hook (after-init . global-company-mode)
;;   :hook (prog-mode . company-mode)
;;   :bind (:map company-active-map
;;            ("C-s"     . company-filter-candidates)
;;            ([tab]     . company-complete-common-or-cycle)
;;            ([backtab] . company-select-previous-or-abort))
;;   :custom
;;   (company-dabbrev-code-ignore-case nil)
;;   (company-dabbrev-code-everywhere t)
;;   (company-files-exclusions '(".git/" ".DS_Store"))
;;   :config
;;   (setq company-tooltip-align-annotations t ;; aligns annotation to the right
;;      company-minimum-prefix-length 1
;;      company-require-match 'company-explicit-action-p
;;      company-tooltip-limit 12
;;      company-tooltip-width-grow-only t
;;      company-tooltip-flip-when-above t
;;      company-transformers '(company-sort-by-occurrence)
;;      company-backends '(company-files
;;                         company-capf
;;                         company-ispell
;;                         (company-dabbrev-code company-keywords)
;;                         company-dabbrev
;;                         ))
;;   )

(use-package corfu
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode))
  :config
  (setq corfu-auto t                 ;; Enable auto completion
        corfu-cycle t                ;; Enable cycling for `corfu-next/previous`
        corfu-preview-current nil
        corfu-auto-prefix 2)
  )

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  )

;; Add extensions
(use-package cape
  :init
  (setq cape-dict-case-fold t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  )

(use-package eglot
  :ensure nil
  :hook (((json-ts-mode js-mode web-mode go-ts-mode dockerfile-ts-mode c-ts-mode c++-ts-mode cmake-ts-mode lua-mode
                        css-mode bash-ts-mode yaml-mode protobuf-ts-mode graphviz-dot-mode markdown-mode) . eglot-ensure))
  :bind (:map eglot-mode-map
			  ("C-c e a" . eglot-code-actions)
			  ("C-c e r" . eglot-rename)
			  ("C-c e f" . eglot-format))
  :config
  (use-package consult-eglot
    :bind (:map eglot-mode-map
                ("M-s s" . consult-eglot-symbols)))

  (setq eglot-report-progress nil
        eglot-autoshutdown t
        eglot-ignored-server-capabilities '(:documentLinkProvider
                                            :inlayHintProvider
                                            :documentOnTypeFormattingProvider))

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
  :hook (prog-mode . yas-minor-mode)
  :config
  (use-package yasnippet-snippets)
  )

;; (use-package eglot-tempel
;;   :after eglot
;;   )

;; ;; Configure Tempel
;; (use-package tempel
;;   :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
;;          ("M-*" . tempel-insert))
;;   ;; :hook (prog-mode . tempel-abbrev-mode)
;;   ;; :hook (eglot-managed-mode . (lambda ()
;;   ;;                               (setq-local completion-at-point-functions
;;   ;;                                           (list (cape-super-capf
;;   ;;                                                  #'eglot-completion-at-point
;;   ;;                                                  #'tempel-expand
;;   ;;                                                  #'cape-keyword)))))

;;   :init
;;   (defun tempel-setup-capf ()
;;     (setq-local completion-at-point-functions
;;                 (cons #'tempel-expand
;;                       completion-at-point-functions)))
;;   :hook ((prog-mode . tempel-setup-capf)
;;          (text-mode . tempel-setup-capf))
;;   )

;; (use-package tempel-collection
;;   :after tempel
;;   )

(provide 'init-lsp)

;;; init-lsp.el ends here
