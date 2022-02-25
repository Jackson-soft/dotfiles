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
              ("C-c t o" . hl-todo-occur))
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
  :config
  (diff-hl-margin-mode)
  (diff-hl-flydiff-mode)
  )

;; 缩进标识
(use-package highlight-indent-guides
  :hook ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top)
  )

;; Dimming Unused Windows
(use-package dimmer
  :hook (after-init . dimmer-mode)
  :config
  (dimmer-configure-which-key)
  )

;; highlighting Doxygen commands and their arguments
(use-package highlight-doxygen
  :hook (cc-mode . highlight-doxygen-mode)
  )

(provide 'init-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-highlight.el ends here
