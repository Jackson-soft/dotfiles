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

;; Add extensions
(use-package cape
  :init
  (setq cape-dict-case-fold t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  )

(use-package kind-icon
  :after corfu
  :init
  (setq kind-icon-mapping
        '((array          "a"   :icon "symbol-array"       :face font-lock-type-face              :collection "vscode")
          (boolean        "b"   :icon "symbol-boolean"     :face font-lock-builtin-face           :collection "vscode")
          (color          "#"   :icon "symbol-color"       :face success                          :collection "vscode")
          (command        "cm"  :icon "chevron-right"      :face default                          :collection "vscode")
          (constant       "co"  :icon "symbol-constant"    :face font-lock-constant-face          :collection "vscode")
          (class          "c"   :icon "symbol-class"       :face font-lock-type-face              :collection "vscode")
          (constructor    "cn"  :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
          (enum           "e"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
          (enummember     "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
          (enum-member    "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
          (event          "ev"  :icon "symbol-event"       :face font-lock-warning-face           :collection "vscode")
          (field          "fd"  :icon "symbol-field"       :face font-lock-variable-name-face     :collection "vscode")
          (file           "f"   :icon "symbol-file"        :face font-lock-string-face            :collection "vscode")
          (folder         "d"   :icon "folder"             :face font-lock-doc-face               :collection "vscode")
          (function       "f"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
          (interface      "if"  :icon "symbol-interface"   :face font-lock-type-face              :collection "vscode")
          (keyword        "kw"  :icon "symbol-keyword"     :face font-lock-keyword-face           :collection "vscode")
          (macro          "mc"  :icon "lambda"             :face font-lock-keyword-face)
          (magic          "ma"  :icon "lightbulb-autofix"  :face font-lock-builtin-face           :collection "vscode")
          (method         "m"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
          (module         "{"   :icon "file-code-outline"  :face font-lock-preprocessor-face)
          (numeric        "nu"  :icon "symbol-numeric"     :face font-lock-builtin-face           :collection "vscode")
          (operator       "op"  :icon "symbol-operator"    :face font-lock-comment-delimiter-face :collection "vscode")
          (param          "pa"  :icon "gear"               :face default                          :collection "vscode")
          (property       "pr"  :icon "symbol-property"    :face font-lock-variable-name-face     :collection "vscode")
          (reference      "rf"  :icon "library"            :face font-lock-variable-name-face     :collection "vscode")
          (snippet        "S"   :icon "symbol-snippet"     :face font-lock-string-face            :collection "vscode")
          (string         "s"   :icon "symbol-string"      :face font-lock-string-face            :collection "vscode")
          (struct         "%"   :icon "symbol-structure"   :face font-lock-variable-name-face     :collection "vscode")
          (text           "tx"  :icon "symbol-key"         :face font-lock-doc-face               :collection "vscode")
          (typeparameter  "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
          (type-parameter "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
          (unit           "u"   :icon "symbol-ruler"       :face font-lock-constant-face          :collection "vscode")
          (value          "v"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
          (variable       "va"  :icon "symbol-variable"    :face font-lock-variable-name-face     :collection "vscode")
          (t              "."   :icon "question"           :face font-lock-warning-face           :collection "vscode")))
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  )


;; dabbrev (dynamic word completion)
(use-package dabbrev
  :ensure nil
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
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
			  ("C-c e a" . eglot-code-actions)
			  ("C-c e r" . eglot-rename)
			  ("C-c e f" . eglot-format))
  :config
  (setq eglot-report-progress nil
        eglot-autoshutdown t
        eglot-ignored-server-capabilities '(:documentLinkProvider
                                            :inlayHintProvider
                                            :documentOnTypeFormattingProvider))

  (add-to-list 'eglot-server-programs '(graphviz-dot-mode . ("dot-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(protobuf-ts-mode . ("bufls" "serve")))
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
