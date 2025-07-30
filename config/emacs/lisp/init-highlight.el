;; init-highlight.el --- Initialize highlighting configurations. -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;;
;; Highlighting configurations.
;;

;;; Code:

;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :hook (after-init . global-hl-todo-mode)
  :bind (:map hl-todo-mode-map
			  ("C-c t p" . hl-todo-previous)
			  ("C-c t n" . hl-todo-next)
			  ("C-c t i" . hl-todo-insert)
			  ("C-c t o" . hl-todo-occur)
			  ("C-c t s" . hl-todo-rgrep))
  )

;; 高亮当前行
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode)
  )

;; Highlight uncommitted changes
(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
		 (magit-pre-refresh  . diff-hl-magit-pre-refresh)
		 (magit-post-refresh . diff-hl-magit-post-refresh)
		 (dired-mode . diff-hl-dired-mode))
  :custom
  (diff-hl-margin-mode t)
  (diff-hl-flydiff-mode t)
  )

;; 缩进标识
(use-package indent-bars
  :hook (prog-mode . indent-bars-mode)
  :config
  (require 'indent-bars-ts)
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-no-descend-string t)
  (indent-bars-prefer-character t)
  (indent-bars-treesit-wrap '((c argument_list parameter_list init_declarator)))
  )

;; Dimming Unused Windows
(use-package dimmer
  :hook (after-init . dimmer-mode)
  )

(use-package volatile-highlights
  :hook (after-init . volatile-highlights-mode)
  )

(provide 'init-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-highlight.el ends here
