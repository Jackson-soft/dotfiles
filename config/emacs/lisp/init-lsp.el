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
		company-transformers '(company-sort-by-occurrence)
		company-backends '(company-files
						   company-capf
						   company-ispell
						   (company-dabbrev-code company-keywords)
						   company-dabbrev
						   ))
  )

;; (use-package corfu
;;   :hook ((after-init . global-corfu-mode)
;;		 (corfu-mode . corfu-popupinfo-mode))
;;   :bind (:map corfu-map
;;			  ("SPC" . corfu-insert-separator))   ;; Configure SPC for separator insertion
;;   :custom
;;   (corfu-auto t)                 ;; Enable auto completion
;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto-prefix 2)
;;   (corfu-quit-no-match t)
;;   (corfu-quit-at-boundary t)
;;   (corfu-scroll-margin 5)
;;   )

;; ;; Add extensions
;; (use-package cape
;;   :bind (("C-c p p" . completion-at-point) ;; capf
;;          ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
;;		 ("C-c p h" . cape-history)
;;          ("C-c p f" . cape-file)
;;          ("C-c p k" . cape-keyword)
;;          ("C-c p s" . cape-symbol)
;;          ("C-c p a" . cape-abbrev)
;;          ("C-c p l" . cape-line)
;;		 ("C-c p w" . cape-dict)
;;          ("C-c p r" . cape-rfc1345))
;;   :init
;;   ;; Add `completion-at-point-functions', used by `completion-at-point'.
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   (add-to-list 'completion-at-point-functions #'cape-history)
;;   (add-to-list 'completion-at-point-functions #'cape-keyword)
;;   (add-to-list 'completion-at-point-functions #'cape-rfc1345)
;;   (add-to-list 'completion-at-point-functions #'cape-abbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-dict)
;;   (add-to-list 'completion-at-point-functions #'cape-symbol)
;;   (add-to-list 'completion-at-point-functions #'cape-line)
;;   (add-to-list 'completion-at-point-functions #'cape-elisp-block)
;;   )

;; (use-package kind-icon
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default) ;; to compute blended backgrounds correctly
;;   :config
;;   (setq kind-icon-use-icons nil)
;;   (setq kind-icon-mapping
;;		`(
;;           (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
;;           (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
;;           (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
;;           (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
;;           (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
;;           (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
;;           (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
;;           (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
;;           (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
;;           (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
;;           (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
;;           (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
;;           (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
;;           (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
;;           (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
;;           (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
;;           (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
;;           (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
;;           (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
;;           (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
;;           (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
;;           (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
;;           (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
;;           (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
;;           (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
;;           (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
;;           (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
;;           (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
;;           (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
;;           (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
;;           (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
;;           (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
;;           (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
;;           (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
;;           (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
;;           (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)))
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
  :hook (((json-mode js-mode web-mode go-ts-mode dockerfile-ts-mode c-mode c++-mode c++-ts-mode cmake-ts-mode lua-mode
					 css-mode sh-mode yaml-ts-mode protobuf-ts-mode graphviz-dot-mode markdown-mode) . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format))
  :config
  (setq eglot-report-progress nil)
  (add-to-list 'eglot-server-programs '(graphviz-dot-mode . ("dot-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(protobuf-ts-mode . ("bufls" "serve")))
  (add-to-list 'eglot-server-programs '(cmake-ts-mode . ("neocmakelsp" "--stdio")))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("clangd"
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

;; (use-package yasnippet
;;   :hook (prog-mode . yas-minor-mode)
;;   :config
;;   (use-package yasnippet-snippets)
;;   )

(use-package eglot-tempel
  :after eglot
  )

;; Configure Tempel
(use-package tempel
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :hook (prog-mode . tempel-abbrev-mode)

  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  )

(use-package tempel-collection
  :after tempel
  )

(provide 'init-lsp)

;;; init-lsp.el ends here
