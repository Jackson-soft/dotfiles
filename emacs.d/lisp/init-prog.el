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
  (setq xref-search-program 'ripgrep
        xref-show-definitions-function 'xref-show-definitions-completing-read ; for M-.
        xref-show-xrefs-function 'xref-show-definitions-buffer) ; for grep and the like
  )

(use-package imenu
  :ensure nil
  :config
  (setq imenu-auto-rescan t)
  )

(use-package tree-sitter
  :hook (((c-mode c++-mode go-mode sh-mode json-mode) . tree-sitter-mode)
         (tree-sitter-after-on . tree-sitter-hl-mode))
  :config

  (use-package tree-sitter-langs)
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;; show trailing whitespaces
(use-package whitespace
  :ensure nil
  :hook (((prog-mode text-mode) . whitespace-mode)
         (before-save . whitespace-cleanup))
  :config
  (setq indicate-empty-lines t
        whitespace-action '(auto-cleanup)
        whitespace-style
        '(face             ;; visualize things below
          trailing         ;; trailing blanks
          empty            ;; empty lines at beginning/end of buffer
          indentation
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

(provide 'init-prog)

;;; init-prog.el ends here
