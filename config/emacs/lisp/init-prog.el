;; init-prog.el --- Initialize programming configurations. -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;;
;; General programming configurations.
;;

;;; Code:

(use-package surround
  :bind
  ("M-'" . surround-keymap)
  )

(use-package treesit
  :ensure nil
  :config
  (setopt treesit-language-source-alist
		  '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
			(c          . ("https://github.com/tree-sitter/tree-sitter-c"))
			(cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
			(css        . ("https://github.com/tree-sitter/tree-sitter-css"))
			(cmake      . ("https://github.com/uyha/tree-sitter-cmake"))
			(dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
			(dot        . ("https://github.com/rydesun/tree-sitter-dot"))
			(doxygen    . ("https://github.com/tree-sitter-grammars/tree-sitter-doxygen"))
			(elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
			(go         . ("https://github.com/tree-sitter/tree-sitter-go"))
			(gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod"))
			(gosum      . ("https://github.com/amaanq/tree-sitter-go-sum"))
			(gowork     . ("https://github.com/omertuc/tree-sitter-go-work"))
			(gitcommit  . ("https://github.com/gbprod/tree-sitter-gitcommit"))
			(html       . ("https://github.com/tree-sitter/tree-sitter-html"))
			(http       . ("https://github.com/rest-nvim/tree-sitter-http"))
			(java       . ("https://github.com/tree-sitter/tree-sitter-java"))
			(javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
			(json       . ("https://github.com/tree-sitter/tree-sitter-json"))
			(lua        . ("https://github.com/tree-sitter-grammars/tree-sitter-lua"))
			(make       . ("https://github.com/tree-sitter-grammars/tree-sitter-make"))
			(markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
			(markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src"))
			(python     . ("https://github.com/tree-sitter/tree-sitter-python"))
			(proto      . ("https://github.com/treywood/tree-sitter-proto"))
			(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
			(tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
			(rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
			(sql        . ("https://github.com/derekstride/tree-sitter-sql"))
			(vue        . ("https://github.com/tree-sitter-grammars/tree-sitter-vue"))
			(yaml       . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
			(toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))))
  )

(use-package project
  :ensure nil
  :custom
  (project-mode-line t)
  (project-kill-buffers-display-buffer-list t)
  )

(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ugrep)
  (xref-history-storage 'xref-window-local-history)
  )

(use-package imenu
  :ensure nil
  :custom
  (imenu-auto-rescan t)
  )

(use-package eldoc-mouse
  :hook
  ((eglot-managed-mode emacs-lisp-mode) . eldoc-mouse-mode)
  :bind
  (("C-h ." . eldoc-mouse-pop-doc-at-cursor))
  :custom
  (eldoc-mouse-mouse-timer 0.5) ;; 鼠标悬停延迟 0.5 秒
  (org-appear-autoentities t)
  (org-appear-autolinks t)
  )

;; show trailing white spaces
(use-package whitespace
  :ensure nil
  :hook
  ((prog-mode text-mode) . whitespace-mode)
  :custom
  (whitespace-action '(auto-cleanup))
  (whitespace-style
   '(face tabs trailing lines-tail newline empty indentation::tab space-before-tab space-after-tab))
  )

;; format
(use-package apheleia
  :hook
  (prog-mode . apheleia-mode)
  :bind
  ("C-c M-f" . apheleia-format-buffer)
  :config
  (add-to-list 'apheleia-formatters '(sql-format . ("sqlfluff" "fix" "--dialect" "postgres" "--disable-progress-bar" "-f" "-n" "-")))

  (dolist (alist '((markdown-ts-mode . prettier-markdown)
				   (gfm-mode . prettier-markdown)
				   (dockerfile-ts-mode . shfmt)
				   (protobuf-ts-mode . clang-format)
				   (emacs-lisp-mode . lisp-indent)
				   (sql-mode . sql-format)))
	(add-to-list 'apheleia-mode-alist alist))
  )

;; 彩虹括号
(use-package prism
  :hook
  (prog-mode . prism-mode)
  )

(use-package symbol-overlay
  :bind
  (("M-i" . symbol-overlay-put)
   ("M-C" . symbol-overlay-remove-all)
   ("M-n" . symbol-overlay-switch-forward)
   ("M-p" . symbol-overlay-switch-backward))
  :hook
  (prog-mode . symbol-overlay-mode)
  )

;; 注释
(use-package newcomment
  :ensure nil
  :bind
  ([remap comment-dwim] . comment-or-uncomment)
  :custom
  (comment-auto-fill-only-comments t)
  :config
  (defun comment-or-uncomment ()
	"Comment or uncomment the current line or region.

If the region is active and `transient-mark-mode' is on, call
`comment-or-uncomment-region'.
Else, if the current line is empty, insert a comment and indent
it.
Else, call `comment-or-uncomment-region' on the current line."
	(interactive)
	(if (region-active-p)
		(comment-or-uncomment-region (region-beginning) (region-end))
	  (if (save-excursion
			(beginning-of-line)
			(looking-at "\\s-*$"))
		  (comment-dwim nil)
		(comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
  )

;; 折叠
(use-package hideshow
  :ensure nil
  :hook
  (prog-mode . hs-minor-mode)
  :bind
  ("C-<tab>" . hs-toggle-hiding)
  )

;; Edit comment/string/docstring/code block in separate buffer
(use-package separedit
  :bind
  (:map prog-mode-map
		("C-c '" . separedit))
  :custom
  (separedit-remove-trailing-spaces-in-comment t)
  (separedit-default-mode 'markdown-mode)
  )

(use-package expreg
  :bind
  (("C-=" . expreg-expand)
   ("C--" . expreg-contract))
  )

(provide 'init-prog)

;;; init-prog.el ends here
