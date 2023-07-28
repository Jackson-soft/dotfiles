;; init-prog.el --- Initialize programming configurations. -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;;
;; General programming configurations.
;;

;;; Code:

(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist
		'((bash "https://github.com/tree-sitter/tree-sitter-bash")
		  (c "https://github.com/tree-sitter/tree-sitter-c")
		  (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
		  (cmake "https://github.com/uyha/tree-sitter-cmake")
		  (css "https://github.com/tree-sitter/tree-sitter-css")
		  (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
		  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
		  (go "https://github.com/tree-sitter/tree-sitter-go")
		  (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
		  (html "https://github.com/tree-sitter/tree-sitter-html")
		  (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
		  (json "https://github.com/tree-sitter/tree-sitter-json")
		  (make "https://github.com/alemuller/tree-sitter-make")
		  (markdown "https://github.com/ikatyang/tree-sitter-markdown")
		  (lua "https://github.com/Azganoth/tree-sitter-lua")
		  (python "https://github.com/tree-sitter/tree-sitter-python")
		  (proto "https://github.com/mitchellh/tree-sitter-proto")
		  (toml "https://github.com/tree-sitter/tree-sitter-toml")
		  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
		  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
		  (yaml "https://github.com/ikatyang/tree-sitter-yaml"))
		)
  )

(use-package xref
  :ensure nil
  :config
  (setq xref-search-program 'ripgrep
		xref-history-storage 'xref-window-local-history)
  )

(use-package imenu
  :ensure nil
  :config
  (setq imenu-auto-rescan t)
  )

(use-package eldoc-box
  :hook (eldoc-mode . eldoc-box-hover-mode)
  :diminish eldoc-box-hover-mode
  )

;; show trailing whitespaces
(use-package whitespace
  :ensure nil
  :hook ((prog-mode text-mode) . whitespace-mode)
  :config
  (setq indicate-empty-lines t
		whitespace-action '(auto-cleanup)
		whitespace-style
		'(face             ;; visualize things below
		  trailing         ;; trailing blanks
		  empty            ;; empty lines at beginning/end of buffer
		  space-before-tab ;; spaces before tab
		  space-after-tab))
  )

;; format
(use-package apheleia
  :hook (prog-mode . apheleia-mode)
  :bind ("C-c M-f" . apheleia-format-buffer)
  :config
  (add-to-list 'apheleia-formatters '(sql-format . ("sqlfluff" "fix" "--dialect" "mysql" "--disable-progress-bar" "-f" "-n" "-")))

  (dolist (alist '((markdown-mode . prettier-markdown)
				   (gfm-mode . prettier-markdown)
				   (dockerfile-ts-mode . shfmt)
				   (protobuf-ts-mode . clang-format)
				   (emacs-lisp-mode . lisp-indent)
				   (sql-mode . sql-format)))
    (add-to-list 'apheleia-mode-alist alist))
  )

;; 注释
(use-package newcomment
  :ensure nil
  :bind ([remap comment-dwim] . comment-or-uncomment)
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

  ;; `auto-fill' inside comments.
  ;; The quoted text in `message-mode' are identified as comments, so only
  ;; quoted text can be `auto-fill'ed.
  (setq comment-auto-fill-only-comments t)
  )

;; 折叠
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind ("C--" . hs-toggle-hiding)
  )

;; Edit comment/string/docstring/code block in separate buffer
(use-package separedit
  :bind (:map prog-mode-map
              ("C-c '" . separedit))
  :custom
  (separedit-remove-trailing-spaces-in-comment t)
  (separedit-default-mode 'markdown-mode)
  )

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  )

(provide 'init-prog)

;;; init-prog.el ends here
