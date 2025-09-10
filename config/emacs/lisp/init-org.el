;;; init-org.el --- Org ---  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Org configurations.
;;
;;; Code:

(use-package text-mode
  :ensure nil
  :hook
  (text-mode . visual-line-mode) ;; 根据窗口大小自动折行
  :custom
  (text-mode-ispell-word-completion nil)
  )

(use-package outline
  :ensure nil
  :custom
  (outline-minor-mode-highlight 'override)
  (outline-minor-mode-cycle t)
  )


;; Org 基础配置（你的原配置）
(use-package org
  :ensure nil
  :custom
  ;; 基础显示
  (org-startup-indented t)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)
  (org-hide-macro-markers t)
  (org-pretty-entities t)
  (org-fold-catch-invisible-edits 'show)
  (org-startup-with-inline-images t)
  (org-link-frame-setup '((file . find-file)))
  (org-return-follows-link t)
  (org-ellipsis "...#")
  ;; 模块
  (org-modules '(org-habit org-id org-tempo org-mac-link org-mouse
						   org-protocol org-annotate-file org-expiry
						   org-interactive-query org-collector org-panel
						   org-screen org-toc ol-info))
  ;; 编辑行为
  (org-tags-column 0)
  (org-insert-heading-respect-content t)
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-support-shift-select 'always)
  (org-highlight-latex-and-related '(native script entities))
  (org-yank-adjusted-subtrees t)
  (org-loop-over-headlines-in-active-region t)
  (org-fontify-todo-headline t)
  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-adapt-indentation t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  ;; 导出
  (org-export-backends '(md latex html icalendar odt))
  :config
  ;; org-id
  (use-package org-id
	:ensure nil
	:custom
	(org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

  ;; Org-Cite
  (use-package oc
	:ensure nil
	:custom
	(org-cite-export-processors
	 '((beamer natbib)
	   (latex biblatex)
	   (t csl))))

  ;; 表格
  (use-package org-table
	:ensure nil
	:custom
	(org-table-header-line-p t))

  ;; 代码块
  (use-package org-src
	:ensure nil
	:hook
	(org-babel-after-execute . org-redisplay-inline-images)
	:custom
	(org-src-preserve-indentation t))

  ;; 导出设置
  (use-package ox
	:ensure nil
	:custom
	(org-export-with-tags 'not-in-toc)
	(org-export-with-author nil)
	(org-export-with-toc nil)
	(org-export-with-priority t)
	(org-export-with-smart-quotes t)
	(org-export-with-broken-links 'mark)
	(org-export-preserve-breaks t)
	(org-export-headline-levels 5)
	(org-export-default-language "zh-CN")
	(org-export-coding-system 'utf-8))

  ;; Pandoc 导出
  (use-package ox-pandoc
	:custom
	(org-pandoc-format-extensions
	 '(markdown_github+pipe_tables+raw_html)))

  ;; Babel 语言支持
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
	 (dot . t)
	 (emacs-lisp . t))))

;; Org Modern - 美化外观
(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  :custom
  (org-modern-star '("◉" "○" "✸" "✿")) ;; 自定义标题符号
  (org-modern-table t)                  ;; 美化表格
  (org-modern-list '((?- . "•") (?+ . "◦") (?* . "▪"))) ;; 列表符号
  (org-modern-todo-faces
   '(("TODO"  :background "#ff6c6b" :foreground "white" :weight bold)
	 ("DONE"  :background "#98be65" :foreground "white" :weight bold))))

;; Org Appear - 光标经过时显示隐藏标记
(use-package org-appear
  :hook
  (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoemphasis t)
  (org-appear-autokeywords t))

;; Org Superstar - 美化标题层级符号
(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode)
  :custom
  ;; 不替换列表符号（交给 org-modern 管）
  (org-superstar-remove-leading-stars t)
  (org-superstar-leading-bullet ?\s)
  ;; 不同层级的标题符号
  (org-superstar-headline-bullets-list '("★" "◉" "○" "▷" "✿" "◇")))

(use-package org-make-toc
  :hook
  (org-mode . org-make-toc-mode)
  )

(use-package org-modern-indent
  :vc
  (:url "https://github.com/jdtsmith/org-modern-indent")
  :hook
  (org-mode-hook . org-modern-indent-mode)
  )

;; 表格对齐
(use-package valign
  :hook
  ((org-mode markdown-mode) . valign-mode)
  :custom
  (valign-fancy-bar t)
  )

;; dot
(use-package graphviz-dot-mode)

(use-package markdown-mode
  :mode
  ("README\\.md\\'" . gfm-mode)
  :bind
  (:map markdown-mode-map
		("C-c C-e" . markdown-do))
  :custom
  (markdown-command "multimarkdown")
  )

(provide 'init-org)

;;; init-org.el ends here
