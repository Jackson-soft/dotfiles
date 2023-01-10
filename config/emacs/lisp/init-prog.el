;; init-prog.el --- Initialize programming configurations. -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;;
;; General programming configurations.
;;

;;; Code:

(use-package xref
  :ensure nil
  :hook ((xref-after-return xref-after-jump) . recenter)
  :config
  (setq xref-search-program 'ripgrep)
  )

(use-package imenu
  :ensure nil
  :config
  (setq imenu-auto-rescan t)
  )

;; Tree-sitter support
;; @see https://github.com/casouri/tree-sitter-module
;;      https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter
(use-package treesit
  :ensure nil
  :init
  (setq major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (cmake-mode      . cmake-ts-mode)
          (css-mode        . css-ts-mode)
          (dockerfile-mode . dockerfile-ts-mode)
          (go-mode         . go-ts-mode)
          (js-mode         . js-ts-mode)
          (js-json-mode    . json-ts-mode)
          (json-mode       . json-ts-mode)
          (python-mode     . python-ts-mode)
          (sh-mode         . bash-ts-mode)
          (typescript-mode . typescript-ts-mode)))
  )

(use-package eldoc-box
  :hook (eldoc-mode . eldoc-box-hover-mode)
  :diminish eldoc-box-hover-mode
  )

;; Highlight symbols
(use-package symbol-overlay
  :hook ((prog-mode yaml-mode) . symbol-overlay-mode)
  :bind (:map symbol-overlay-mode-map
              ("M-i" . symbol-overlay-put)
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev))
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
  (nconc apheleia-formatters '((pgfmt . ("pg_format" "-"))
							   (cmake-format . ("cmake-format" "--tab-size=4" "-"))))

  (nconc apheleia-mode-alist '((markdown-mode . prettier)
							   (gfm-mode . prettier)
							   (dockerfile-mode . shfmt)
							   (cmake-mode . cmake-format)
							   (protobuf-mode . clang-format)
							   (emacs-lisp-mode . lisp-indent)
							   (go-ts-mode . gofmt)
							   (sql-mode . pgfmt)))
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

;; 注释的风格
(use-package docstr
  :hook (prog-mode . docstr-mode)
  :diminish docstr-mode
  :config
  (docstr-faces-apply)
  (setq docstr-key-support t)
  )

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  )

(provide 'init-prog)

;;; init-prog.el ends here
