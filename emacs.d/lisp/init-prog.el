;; init-prog.el --- Initialize programming configurations. -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;;
;; General programming configurations.
;;

;;; Code:

(use-package xref
  :ensure nil
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
          tabs             ;; tabs (show by face)
          empty            ;; empty lines at beginning/end of buffer
          indentation
          space-before-tab ;; spaces before tab
          space-after-tab
          tab-mark))       ;; tabs (show by symbol)
  )

;; format
(use-package apheleia
  :hook (after-init . apheleia-global-mode)
  :bind ("C-c C-f" . apheleia-format-buffer)
  :config
  (nconc apheleia-formatters '((stylua . ("stylua" "--indent-type=Spaces" "-"))
                               (shfmt . ("shfmt"))
                               (pgfmt . ("pg_format" "-"))
                               (cmake-format . ("cmake-format" "--tab-size=4" "-"))))

  (nconc apheleia-mode-alist '((lua-mode . stylua)
                               (markdown-mode . prettier)
                               (gfm-mode . prettier)
                               (sh-mode . shfmt)
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
