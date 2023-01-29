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

;; https://www.flycheck.org/en/latest/
;; (use-package flycheck
;;   :hook (prog-mode . flycheck-mode)
;;   :config
;;   (setq flycheck-emacs-lisp-load-path 'inherit
;;		flycheck-check-syntax-automatically '(save mode-enabled)
;;		flycheck-checkers '(c/c++-clang
;;							dockerfile-hadolint
;;							emacs-lisp
;;							emacs-lisp-checkdoc
;;							go-build
;;							go-test
;;							json-jq
;;							lua
;;							markdown-markdownlint-cli
;;							protobuf-protoc
;;							python-mypy
;;							sh-shellcheck
;;							sh-zsh
;;							systemd-analyze
;;							yaml-jsyaml
;;							))
;;   )


(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  )

;; flymake linter
(use-package flymake-collection
  :hook (flymake-mode . flymake-collection-hook-setup)
  )

;; https://company-mode.github.io/manual/
(use-package company
  ;; :hook (after-init . global-company-mode)
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
			  ("C-s"     . company-filter-candidates)
			  ([tab]     . company-complete-common-or-cycle)
			  ([backtab] . company-select-previous-or-abort))
  :custom
  (company-dabbrev-code-ignore-case nil)
  (company-dabbrev-code-everywhere t)
  (company-files-exclusions '(".git/" ".DS_Store"))
  :config
  (setq company-tooltip-align-annotations t ;; aligns annotation to the right
		company-minimum-prefix-length 1
		company-require-match 'company-explicit-action-p
		company-tooltip-limit 12
		company-tooltip-width-grow-only t
		company-tooltip-flip-when-above t
		;; company-show-quick-access 'left
		company-transformers '(company-sort-by-occurrence)
		company-backends '(company-files
						   company-cmake
						   company-capf
						   company-ispell
						   (company-dabbrev-code company-keywords)
						   company-dabbrev
						   ))
  )

;; (use-package corfu
;;   :init
;;   (global-corfu-mode)
;;   :config
;;   (setq corfu-cycle t                ;; Enable cycling for `corfu-next/previous'
;;         corfu-auto t                 ;; Enable auto completion
;;		corfu-quit-no-match t        ;; Automatically quit if there is no match
;;         corfu-auto-prefix 2)

;;   ;; Add extensions
;;   (use-package cape
;;     :bind (("C-c p p" . completion-at-point) ;; capf
;;            ("C-c p t" . complete-tag)        ;; etags
;;            ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
;;            ("C-c p f" . cape-file)
;;            ("C-c p k" . cape-keyword)
;;            ("C-c p s" . cape-symbol)
;;            ("C-c p a" . cape-abbrev)
;;            ("C-c p i" . cape-ispell)
;;            ("C-c p l" . cape-line)
;;            ("C-c p r" . cape-rfc1345))
;;     :init
;;     ;; Add `completion-at-point-functions', used by `completion-at-point'.
;;     (dolist (backend '(cape-file cape-dabbrev cape-symbol cape-keyword cape-abbrev cape-ispell cape-line cape-rfc1345))
;;       (add-to-list 'completion-at-point-functions backend))
;;     )

;;   (use-package kind-icon
;;     :custom
;;     (kind-icon-default-face 'corfu-default) ;; to compute blended backgrounds correctly
;;     :config
;;     (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter)
;;     )
;;   )

;; (use-package corfu-doc
;;   :hook (corfu-mode . corfu-doc-mode)
;;   )

;;; Dabbrev (dynamic word completion)
(use-package dabbrev
  :ensure nil
  :bind (("M-/" . dabbrev-expand)
		 ("C-x M-/" . dabbrev-completion))
  :config
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_"
		dabbrev-abbrev-skip-leading-regexp "[$*/=~']"
		dabbrev-backward-only nil
		dabbrev-case-distinction 'case-replace
		dabbrev-case-fold-search nil
		dabbrev-case-replace 'case-replace
		dabbrev-check-other-buffers t
		dabbrev-eliminate-newlines t
		dabbrev-upcase-means-case-search t)
  )

(use-package eglot
  :ensure nil
  :hook (((json-mode js-mode web-mode go-mode dockerfile-ts-mode c-mode c++-mode c++-ts-mode cmake-ts-mode lua-mode
					 css-mode sh-mode yaml-ts-mode protobuf-ts-mode graphviz-dot-mode) . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format))
  :config
  (add-to-list 'eglot-server-programs '(graphviz-dot-mode . ("dot-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(protobuf-mode . ("bufls" "serve")))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("clangd"
															 "-j=2"
															 "--background-index"
															 "--clang-tidy"
															 "--cross-file-rename"
															 "--completion-style=bundled"
															 "--pch-storage=memory"
															 "--header-insertion=iwyu"
															 "--header-insertion-decorators")))
  )

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (use-package yasnippet-snippets)
  )

(provide 'init-lsp)

;;; init-lsp.el ends here
