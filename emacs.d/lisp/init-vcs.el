;;; init-vcs.el --- version control system   -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; version control system configurations.
;;
;;; Code:

(use-package magit
  :hook (git-commit-setup . git-commit-turn-on-flyspell)
  :bind (("C-x g" . magit-status)
		 ("C-x M-g" . magit-dispatch)
		 ("C-c M-g" . magit-file-dispatch))
  :config

  (use-package magit-diff
	:ensure nil
	:config
	(setq magit-diff-refine-hunk t)
	)
  )

(use-package git-modes)

(use-package with-editor
  :hook (vterm-mode . with-editor-export-editor)
  )

;; NB `diff-hl' depends on `vc'
(use-package vc-hooks
  :ensure nil
  :config
  (setq vc-follow-symlinks t
		vc-handled-backends '(Git))
  )

(use-package vc-git
  :ensure nil
  :config
  (setq vc-git-diff-switches '("--patch-with-stat" "--histogram")
		vc-git-print-log-follow t)
  )

(use-package diff-mode
  :ensure nil
  :config
  (setq diff-default-read-only t
		diff-font-lock-syntax 'hunk-also)
  )

;; Visual diff interface
(use-package ediff
  :ensure nil
  :config
  (setq ediff-show-clashes-only t
		ediff-window-setup-function 'ediff-setup-windows-plain
		ediff-split-window-function 'split-window-horizontally)
  )

;; 高亮合并冲突
(use-package smerge-mode
  :ensure nil
  )

(provide 'init-vcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vcs.el ends here
