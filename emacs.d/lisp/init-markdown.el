;;; init-markdown.el --- Markdown -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Markdown configurations.
;;
;;; Code:

(use-package markdown-mode
  :mode (("README\\(?:\\.md\\)?\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "multimarkdown"
        markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-asymmetric-header t
        markdown-italic-underscore t
        markdown-fontify-code-blocks-natively t
        markdown-fontify-code-block-default-mode t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t)

  (use-package edit-indirect)
  )

(use-package markdown-toc
  :hook ((markdown-mode gfm-mode) . markdown-toc-mode)
  )

(provide 'init-markdown)

;;; init-markdown.el ends here
