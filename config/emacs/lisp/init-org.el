;;; init-org.el --- Org ---  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Org configurations.
;;
;;; Code:

(use-package text-mode
  :ensure nil
  :hook (text-mode . visual-line-mode) ;; 根据窗口大小自动折行
  )

(use-package outline
  :ensure nil
  :config
  (setq outline-minor-mode-highlight 'override
		outline-minor-mode-cycle t)
  )

(use-package org
  :ensure nil
  :custom
  (org-fontify-quote-and-verse-blocks t)
  (org-link-frame-setup '((file . find-file))) ;; 同一个窗口下打开org文件, 默认是在另一个窗口打
  (org-return-follows-link t)
  :config
  (setq org-modules '(org-tempo
					  org-id
					  org-toc
					  org-habit
					  org-protocol
					  org-mac-link
					  ol-eww
					  ol-info)
		org-pretty-entities t
		;; org-auto-align-tags nil
		org-tags-column 0
		org-startup-with-inline-images t
		org-insert-heading-respect-content t
		org-ellipsis "...#"

		;; Org Logging
		org-log-into-drawer t
		org-log-done 'time

		org-support-shift-select 'always
		org-hide-macro-markers t
		org-hide-emphasis-markers t
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
	:config
	(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
	)

  ;;; Citations
  ;;; Org-Cite
  (use-package oc
	:ensure nil
	:config
	(setq org-cite-export-processors '((beamer natbib)
									   (latex biblatex)
									   (t csl)))
	)

  (use-package org-table
	:ensure nil
	:config
	(setq org-table-header-line-p t)
	)

  ;; Write codes in org-mode
  (use-package org-src
	:ensure nil
	:hook (org-babel-after-execute . org-redisplay-inline-images)
	:config
	(setq org-src-preserve-indentation t)
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
	:config
	(setq org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html)))

  (use-package ob-restclient)

  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t)
							   (dot . t)
							   (restclient . t)
							   (emacs-lisp . t)))
  )

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode)
  )

;; 自动显示隐藏光标所在位置的修饰符号
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t)
  )

;; "prettier" bullets
(use-package org-modern
  :hook (org-mode . org-modern-mode)
  )

;; 表格对齐
(use-package valign
  :hook ((org-mode markdown-mode) . valign-mode)
  :config
  (setq valign-fancy-bar t)
  )

;; (use-package corg
;;   :vc (:url `https://github.com/isamert/corg.el`
;;             :branch "main")
;;   :hook (org-mode . corg-setup)
;;   )

;; dot
(use-package graphviz-dot-mode)

(provide 'init-org)

;;; init-org.el ends here
