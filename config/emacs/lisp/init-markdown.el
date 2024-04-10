;;; init-markdown.el --- Markdown -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Markdown configurations.
;;
;;; Code:

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :bind (:map markdown-mode-map
			  ("C-c C-e" . markdown-do))
  :config
  (setq markdown-command "pandoc"
		markdown-enable-math t
		markdown-enable-wiki-links t
		markdown-asymmetric-header t
		markdown-italic-underscore t
		markdown-fontify-code-blocks-natively t
		markdown-fontify-code-block-default-mode t
		markdown-header-scaling t
		markdown-make-gfm-checkboxes-buttons t
		markdown-gfm-additional-languages '("sh")
		markdown-gfm-uppercase-checkbox t)

  (use-package edit-indirect)
  )

(provide 'init-markdown)

;;; init-markdown.el ends here
