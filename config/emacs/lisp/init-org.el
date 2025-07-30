;;; init-org.el --- Org ---  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Org configurations.
;;
;;; Code:

(use-package text-mode
  :ensure nil
  :hook (text-mode . visual-line-mode) ;; 根据窗口大小自动折行
  :custom
  (text-mode-ispell-word-completion nil)
  )

(use-package outline
  :ensure nil
  :custom
  (outline-minor-mode-highlight 'override)
  (outline-minor-mode-cycle t)
  )

(use-package org
  :ensure nil
  :custom
  (org-startup-indented t)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)
  (org-hide-macro-markers t)
  (org-pretty-entities t)
  (org-fold-catch-invisible-edits 'show)
  (org-startup-with-inline-images t)
  (org-link-frame-setup '((file . find-file))) ;; 同一个窗口下打开org文件, 默认是在另一个窗口打
  (org-return-follows-link t)
  (org-modules '(org-habit
                 org-id
                 org-protocol
                 org-tempo
                 org-toc
                 org-mac-link
                 ol-eww
                 ol-info))
  :config
  (setopt org-tags-column 0
          org-insert-heading-respect-content t
          org-ellipsis "...#"

          ;; Org Logging
          org-log-into-drawer t
          org-log-done 'time

          org-support-shift-select 'always
          org-highlight-latex-and-related '(native script entities)
          org-export-backends '(md latex html icalendar odt)
          org-yank-adjusted-subtrees t
          ;; prettify
          org-loop-over-headlines-in-active-region t
          org-fontify-todo-headline t
          org-fontify-done-headline t
          org-fontify-whole-heading-line t
          org-adapt-indentation t
          org-special-ctrl-a/e t
          org-special-ctrl-k t
          )

  (use-package org-id
	:ensure nil
	:custom
	(org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
	)

  ;;; Citations
  ;;; Org-Cite
  (use-package oc
	:ensure nil
	:custom
	(org-cite-export-processors '((beamer natbib)
								  (latex biblatex)
								  (t csl)))
	)

  (use-package org-table
	:ensure nil
	:custom
	(org-table-header-line-p t)
	)

  ;; Write codes in org-mode
  (use-package org-src
	:ensure nil
	:hook (org-babel-after-execute . org-redisplay-inline-images)
	:custom
	(org-src-preserve-indentation t)
	)

  ;; export
  (use-package ox
	:ensure nil
	:config
	(setq org-export-with-tags 'not-in-toc
		  org-export-with-author nil
		  org-export-with-toc nil
		  org-export-with-priority t
		  org-export-with-smart-quotes t
		  org-export-with-broken-links 'mark
		  org-export-preserve-breaks t
		  org-export-headline-levels 5
		  org-export-default-language "zh-CN"  ;; 默认是en
		  org-export-coding-system 'utf-8)
	)

  (use-package ox-pandoc
	:custom
	(org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html)))

  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t)
							   (dot . t)
							   (emacs-lisp . t)))
  )

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode)
  )

;; 自动显示隐藏光标所在位置的修饰符号
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autokeywords t
        org-appear-inside-latex t)
  )

;; "prettier" bullets
(use-package org-modern
  :hook (org-mode . org-modern-mode)
  )

(use-package org-modern-indent
  :vc (:url "https://github.com/jdtsmith/org-modern-indent")
  :hook (org-mode-hook . org-modern-indent-mode)
  )

;; 表格对齐
(use-package valign
  :hook ((org-mode markdown-ts-mode) . valign-mode)
  :custom
  (valign-fancy-bar t)
  )

;; dot
(use-package graphviz-dot-mode)

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do))
  :custom
  (markdown-command "multimarkdown")
  )

(provide 'init-org)

;;; init-org.el ends here
