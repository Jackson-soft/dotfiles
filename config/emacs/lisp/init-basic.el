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
  :hook
  (window-setup . window-divider-mode)
  :bind
  ("<f10>" . toggle-frame-fullscreen)
  :custom
  (window-divider-default-places t)  ;; 窗口间显示分割线
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)
  (window-resize-pixelwise t)
  )

(use-package emacs
  :ensure nil
  :custom
  ;; 缩进与补全
  (tab-always-indent 'complete)
  (tab-first-completion 'word-or-paren-or-punct)

  ;; 滚动与光标
  (scroll-margin 2)
  (scroll-step 1)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (line-move-visual nil)
  (track-eol t)

  ;; 界面与提示
  (use-short-answers t)
  (mode-line-compact t)
  (visible-bell t)
  (ring-bell-function 'ignore)
  (display-raw-bytes-as-hex t)
  (indicate-empty-lines t)

  ;; 文件与对话框
  (use-file-dialog nil)
  (use-dialog-box nil)

  ;; 文本显示
  (word-wrap-by-category t)

  ;; 窗口行为
  (window-combination-resize t)

  :init
  ;; 模式开关
  (transient-mark-mode 1)   ;; 高亮标记区域
  (context-menu-mode 1)     ;; 右键菜单
  (setq-default tab-width 4
				fill-column 120
				truncate-lines t) ;; 不自动换行长行

  (require-theme 'modus-themes)
  :config
  ;; Add all your customizations prior to loading the themes
  (setopt modus-themes-italic-constructs t
		  modus-themes-bold-constructs nil
		  modus-themes-common-palette-overrides modus-themes-preset-overrides-intense)

  (modus-themes-load-theme 'modus-operandi-tinted)
  )

(use-package window
  :ensure nil
  :custom
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t)
  )

(use-package ultra-scroll
  :hook
  (after-init . ultra-scroll-mode)
  :custom
  (scroll-conservatively 3) ; or whatever value you prefer, since v0.4
  (scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  )

(use-package files
  :ensure nil
  :custom
  (save-abbrevs 'silently)
  (confirm-kill-processes nil)      ;; 关闭emacs 时无需额外确认
  (make-backup-files nil)          ;; Forbide to make backup files
  (create-lockfiles nil)            ;; No lock files
  )

(use-package simple
  :ensure nil  ;; 内置包，无需安装
  :bind
  (("C-z" . undo-redo)                       ;; 撤销/重做
   ([remap just-one-space] . cycle-spacing)) ;; 空格循环压缩
  :custom
  ;; 命令补全过滤器
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; 光标移动与标记行为
  (line-move-visual nil)                 ;; 按逻辑行移动
  (track-eol t)                           ;; 保持在行尾
  (set-mark-command-repeat-pop t)         ;; 连续 C-SPC 弹出多个 mark
  ;; 视觉提示
  (visual-line-fringe-indicators '(nil right-curly-arrow))
  ;; 剪贴板与 kill-ring
  (kill-do-not-save-duplicates t)         ;; 不保存重复的 kill
  (save-interprogram-paste-before-kill t) ;; 覆盖前保存外部剪贴板
  ;; 光标信息
  (what-cursor-show-names t)              ;; `what-cursor-position` 显示字符名
  :init
  ;; 默认缩进使用空格
  (setq-default indent-tabs-mode nil)
  ;; 启用列号和行号显示
  (column-number-mode 1)
  (line-number-mode 1)
  ;; 启用重复模式（连续执行同类命令）
  (repeat-mode 1)
  ;; 启用选中即替换
  (delete-selection-mode 1)
  )

;; abbrev mode configuration
(use-package abbrev
  :ensure nil
  :hook
  (after-init . abbrev-mode)
  :custom
  (abbrev-suggest t)
  )

(use-package select
  :ensure nil
  :custom
  (select-enable-primary t)       ;; 支持emacs和外部程序的粘贴
  )

;; Paste at point NOT at cursor 是用滚轴鼠标
(use-package mwheel
  :ensure nil
  :hook
  (after-init . mouse-wheel-mode)
  )

;; Auto refresh
(use-package autorevert
  :ensure nil
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-avoid-polling t) ;; don't do pooling for autorevert (use notifications).
  (auto-revert-verbose nil) ;; not show message when file changes
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t)
  )

;; 显示打开文件历史
(use-package recentf
  :ensure nil
  :hook
  (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 300)
  )

(provide 'init-basic)

;;; init-basic.el ends here
