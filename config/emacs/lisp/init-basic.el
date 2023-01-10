;;; init-basic.el --- Initialize basic configurations.  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Basic configuration
;;
;;; Code:

;; Good pixel line scrolling
(if (boundp 'pixel-scroll-precision-mode)
	(pixel-scroll-precision-mode t)
  )

(put 'narrow-to-region 'disabled nil)

;; 启动emacs时窗口最大化
(use-package frame
  :ensure nil
  :hook (window-setup . window-divider-mode)
  :bind ("<f10>" . toggle-frame-fullscreen)
  :config
  ;; Display dividers between windows
  (setq window-divider-default-places t  ;; 窗口间显示分割线
		window-divider-default-bottom-width 1
		window-divider-default-right-width 1
		window-resize-pixelwise t)
  )

(use-package emacs
  :ensure nil
  :init
  (setq modus-themes-syntax '(faint alt-syntax green-strings yellow-comments)
		modus-themes-links '(neutral-underline background)
		modus-themes-box-buttons '(variable-pitch flat faint 0.9)
		modus-themes-prompts '(intense bold)
		modus-themes-mode-line '(moody accented borderless)
		modus-themes-lang-checkers '(text-also background)
		modus-themes-hl-line '(intense)
		modus-themes-subtle-line-numbers t
		modus-themes-markup '(bold italic)
		modus-themes-paren-match '(bold intense)
		modus-themes-region '(bg-only no-extend)
		modus-themes-org-blocks 'tinted-background
		modus-themes-headings '((t . (variable-pitch extrabold)))
		)
  :bind ("<f5>" . modus-themes-toggle)
  :config
  (setq-default tab-width 4
				tab-always-indent 'complete
				tab-first-completion 'word-or-paren-or-punct
				indicate-empty-lines t  ;; 如果文件末尾有空行，以可视地形式提醒
				fill-column 120
				window-combination-resize t
				major-mode 'text-mode)

  ;; Turn on transient-mark-mode
  (transient-mark-mode t) ;; 标记高亮

  ;; (load-theme 'modus-vivendi)
  (load-theme 'modus-operandi)

  (setq scroll-margin 2           ;; better scrolling experience
		scroll-step 1
		scroll-conservatively 101 ;; > 100
		scroll-preserve-screen-position t  ;; 滚动时保持光标位置
		use-short-answers t
		visible-bell t ;; 错误操作时的窗口闪动警告
		ring-bell-function 'ignore
		display-raw-bytes-as-hex t    ;; Improve display
		use-file-dialog nil
		use-dialog-box nil  ;; 不使用对话框进行（是，否 取消）的选择，而是用minibuffer
		)
  )

(use-package files
  :ensure nil
  :config
  (setq confirm-kill-processes nil      ;; 关闭emacs 时无需额外确认
		create-lockfiles nil            ;; No lock files
		make-backup-files nil)          ;; Forbide to make backup files
  )

(use-package simple
  :ensure nil
  :bind (("C-z" . undo-redo)
		 ([remap just-one-space] . cycle-spacing))
  :config
  (setq visual-line-fringe-indicators '(nil right-curly-arrow)
		;; eliminate duplicates
		kill-do-not-save-duplicates t
		;; show the name of character in `what-cursor-position'
		what-cursor-show-names t
		set-mark-command-repeat-pop t
		indent-tabs-mode nil

		;; show column/filesize in modeline
		column-number-mode t
		size-indication-mode t)
  )

;; abbrev mode configuration
(use-package abbrev
  :ensure nil
  :hook (after-init . abbrev-mode)
  :config
  (setq save-abbrevs 'silent
		abbrev-suggest t)
  )

(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode)
  )

(use-package select
  :ensure nil
  :config
  (setq select-enable-primary t)       ;; 支持emacs和外部程序的粘贴
  )

;; Paste at point NOT at cursor 是用滚轴鼠标
(use-package mwheel
  :ensure nil
  :hook (after-init . mouse-wheel-mode)
  )

;; Auto refresh
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-avoid-polling t ;; don't do pooling for autorevert (use notifications).
		auto-revert-verbose nil ;; not show message when file changes
		auto-revert-check-vc-info t
		global-auto-revert-non-file-buffers t)
  )

;; 显示打开文件历史
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 300
		recentf-filename-handlers '(abbreviate-file-name))
  )

;; 谷歌翻译
(use-package go-translate
  :bind ("C-c g" . gts-do-translate)
  :config
  (setq gts-translate-list '(("en" "zh"))
		gts-default-translator (gts-translator
								:picker (gts-prompt-picker)
								:engines (list (gts-google-rpc-engine))
								:render (gts-buffer-render)))
  )

(provide 'init-basic)

;;; init-basic.el ends here
