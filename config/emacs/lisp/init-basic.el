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
  :custom
  (window-divider-default-places t)  ;; 窗口间显示分割线
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)

  (window-resize-pixelwise t)
  )

(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'complete)
  (tab-first-completion 'word-or-paren-or-punct)
  (transient-mark-mode t) ;; 标记高亮
  :init
  (setq-default tab-width 4
				indicate-empty-lines t  ;; 如果文件末尾有空行，以可视地形式提醒
				fill-column 120
				window-combination-resize t)
  :config
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
		word-wrap-by-category t)
  )

(use-package window
  :ensure nil
  :custom
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t)
  )

(use-package ultra-scroll
  :hook (after-init . ultra-scroll-mode)
  :custom
  (scroll-conservatively 3) ; or whatever value you prefer, since v0.4
  (scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  )

(use-package files
  :ensure nil
  :custom
  (confirm-kill-processes nil)      ;; 关闭emacs 时无需额外确认
  (make-backup-files nil)          ;; Forbide to make backup files

  (create-lockfiles nil)            ;; No lock files
  )

(use-package simple
  :ensure nil
  :bind (("C-z" . undo-redo)
		 ([remap just-one-space] . cycle-spacing))
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (setq-default indent-tabs-mode nil)

  (setopt column-number-mode t        ;; show column/filesize in modeline
          line-number-mode t
          line-move-visual nil
          track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
          set-mark-command-repeat-pop t  ; Repeating C-SPC after popping mark pops it again
          visual-line-fringe-indicators '(nil right-curly-arrow)
          ;; eliminate duplicates
          kill-do-not-save-duplicates t
          ;; show the name of character in `what-cursor-position'
          what-cursor-show-names t
          save-interprogram-paste-before-kill t
          )
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
  :custom
  (select-enable-primary t)       ;; 支持emacs和外部程序的粘贴
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
  :custom
  (recentf-max-saved-items 300)
  (recentf-filename-handlers '(abbreviate-file-name))
  )

;; 谷歌翻译
(use-package go-translate
  :bind ("C-c C-t" . gt-do-translate)
  :custom
  (gt-default-translator (gt-translator
                          :taker (gt-taker :langs '(en zh) :text 'word)
                          :engines (list (gt-google-rpc-engine)
                                         (gt-youdao-dict-engine)
                                         (gt-youdao-suggest-engine))
                          :render (gt-buffer-render)))
  )

(provide 'init-basic)

;;; init-basic.el ends here
