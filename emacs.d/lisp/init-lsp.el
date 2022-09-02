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
;;   :hook (after-init . corfu-global-mode)
;;   :config
;;   (setq corfu-cycle t                ;; Enable cycling for `corfu-next/previous'
;;         corfu-auto t                 ;; Enable auto completion
;;         corfu-auto-prefix 2)

;;   ;; Add extensions
;;   (use-package cape
;;     ;; Bind dedicated completion commands
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
;;     (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
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

;; https://emacs-lsp.github.io/lsp-mode/
(use-package lsp-mode
  :hook (((json-mode js-mode web-mode go-mode dockerfile-mode c-mode c++-mode cmake-mode lua-mode
					 css-mode sh-mode yaml-mode nginx-mode markdown-mode graphviz-dot-mode) . lsp-deferred)
		 ((go-mode c++-mode c-mode lua-mode) . lsp-save-hooks)
		 (dired-mode . lsp-dired-mode))
  :custom
  (lsp-clients-clangd-args '("-j=2"
							 "--background-index"
							 "--clang-tidy"
							 "--cross-file-rename"
							 "--completion-style=bundled"
							 "--pch-storage=memory"
							 "--header-insertion=iwyu"
							 "--header-insertion-decorators"))
  (lsp-diagnostics-disabled-modes '(go-mode
									sh-mode))
  :config
  (setq lsp-restart 'auto-restart
		lsp-auto-guess-root t
		lsp-semantic-tokens-enable t
		)

  (defun lsp-save-hooks ()
	(add-hook 'before-save-hook 'lsp-format-buffer t t)
	(add-hook 'before-save-hook 'lsp-organize-imports t t))

  (use-package lsp-ui
	:hook (lsp-mode . lsp-ui-mode)
	:bind ("C-c u" . lsp-ui-imenu)
	:config
	(setq lsp-ui-sideline-ignore-duplicate t)
	)

  (use-package lsp-lua
	:ensure nil
	:config
	(setq lsp-lua-hint-enable t)
	)
  )

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (use-package yasnippet-snippets)
  )

(provide 'init-lsp)

;;; init-lsp.el ends here
