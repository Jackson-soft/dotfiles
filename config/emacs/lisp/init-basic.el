;;; init-basic.el --- Initialize basic configurations.  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Basic configuration
;;
;;; Code:

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
  :config
  (setq-default tab-width 4
				tab-always-indent 'complete
				tab-first-completion 'word-or-paren-or-punct
				indicate-empty-lines t  ;; 如果文件末尾有空行，以可视地形式提醒
				fill-column 120
				window-combination-resize t)

  (transient-mark-mode t) ;; 标记高亮

  (setq scroll-margin 2           ;; better scrolling experience
		scroll-step 1
		scroll-conservatively 101 ;; > 100
		scroll-preserve-screen-position t  ;; 滚动时保持光标位置
		use-short-answers t
		mode-line-compact t
		visible-bell t ;; 错误操作时的窗口闪动警告
		ring-bell-function 'ignore
		display-raw-bytes-as-hex t    ;; Improve display
		use-file-dialog nil
		use-dialog-box nil  ;; 不使用对话框进行（是，否 取消）的选择，而是用minibuffer
		)
  )

(use-package window
  :ensure nil
  :config
  (setq switch-to-buffer-in-dedicated-window 'pop
		switch-to-buffer-obey-display-actions t)
  )

;; good pixel line scrolling
(use-package pixel-scroll
  :ensure nil
  :hook (after-init . pixel-scroll-precision-mode)
  :config
  (setq pixel-scroll-precision-interpolate-page t)
  (defalias 'scroll-up-command 'pixel-scroll-interpolate-down)
  (defalias 'scroll-down-command 'pixel-scroll-interpolate-up)
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
  (setq-default indent-tabs-mode nil)
  (setq visual-line-fringe-indicators '(nil right-curly-arrow)
		;; eliminate duplicates
		kill-do-not-save-duplicates t
		;; show the name of character in `what-cursor-position'
		what-cursor-show-names t
		set-mark-command-repeat-pop t
		save-interprogram-paste-before-kill t
        read-extended-command-predicate #'command-completion-default-include-p

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
  :bind ("C-c C-t" . gt-do-translate)
  :config
  (setq gt-default-translator
        (gt-translator
         :taker (gt-taker :langs '(en zh) :text 'word)
         :engines (list (gt-google-rpc-engine)
                        (gt-youdao-dict-engine)
                        (gt-youdao-suggest-engine))
         :render (gt-buffer-render)))
  )

(provide 'init-basic)

;;; init-basic.el ends here
