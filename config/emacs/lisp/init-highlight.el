;; init-highlight.el --- Initialize highlighting configurations. -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;;
;; Highlighting configurations.
;;

;;; Code:

;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :hook
  (after-init . global-hl-todo-mode)
  :bind
  (:map hl-todo-mode-map
		("C-c t p" . hl-todo-previous)
		("C-c t n" . hl-todo-next)
		("C-c t i" . hl-todo-insert)
		("C-c t o" . hl-todo-occur)
		("C-c t s" . hl-todo-rgrep))
  )

;; 高亮当前行
(use-package hl-line
  :ensure nil
  :hook
  (after-init . global-hl-line-mode)
  )

;; 高亮未提交的更改
(use-package diff-hl
  :hook
  ;; Magit 集成
  (magit-pre-refresh  . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  ;; Dired 集成
  (dired-mode . diff-hl-dired-mode)
  :init
  ;; 启用全局 diff-hl
  (global-diff-hl-mode 1)
  ;; 在边距显示更改标记
  (diff-hl-margin-mode 1)
  ;; 实时更新更改标记
  (diff-hl-flydiff-mode 1)
  :bind
  ;; 快捷键：查看当前行的 diff 详情
  (("C-c d" . diff-hl-show-hunk)
   ;; 可选：撤销当前 hunk
   ("C-c u" . diff-hl-revert-hunk)))

;; 缩进标识
(use-package indent-bars
  :hook
  (prog-mode . indent-bars-mode) ;; 所有编程模式启用
  :custom
  ;; 启用 Tree-sitter 支持
  (indent-bars-treesit-support t)
  ;; 忽略空行类型
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; 不显示下降线条
  (indent-bars-no-descend-string t)
  ;; 使用字符而不是线条
  (indent-bars-prefer-character t)
  ;; 为特定语法节点包裹缩进线
  (indent-bars-treesit-wrap
   '((c argument_list parameter_list init_declarator)))
  :config
  ;; 加载 Tree-sitter 扩展
  (require 'indent-bars-ts))

;; Dimming Unused Windows
(use-package dimmer
  :hook
  (after-init . dimmer-mode)
  )

(use-package volatile-highlights
  :hook
  (after-init . volatile-highlights-mode)
  )

(provide 'init-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-highlight.el ends here
