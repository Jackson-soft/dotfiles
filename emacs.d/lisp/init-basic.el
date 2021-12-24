;;; init-basic.el --- Initialize basic configurations.  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Basic configuration
;;
;;; Code:

;; 禁止Emacs使用Mac原生的全屏模式
(when (eq system-type 'darwin)
  ;; (setq ns-use-native-fullscreen nil)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))  ;; mac 下标题栏使用原生
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  )

;; gc 相关
(setq gc-cons-threshold (* 16 1024 1024) ;; 16M
      read-process-output-max (* 1024 1024)
      inhibit-compacting-font-caches t)  ;; Don’t compact font caches during GC.

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Good pixel line scrolling
(if (boundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t)
  )

;; 启动emacs时窗口最大化
(use-package frame
  :ensure nil
  :hook (window-setup . window-divider-mode)
  :bind ("<f10>" . toggle-frame-fullscreen)
  :config
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(font . "Fira Code-17"))

  ;; Display dividers between windows
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)

  (setq-default window-resize-pixelwise t
                frame-resize-pixelwise t)
  )

(use-package emacs
  :ensure nil
  :config
  (setq-default cursor-type 'bar
                tab-width 4
                tab-always-indent 'complete
                tab-first-completion 'word-or-paren-or-punct
                indicate-empty-lines t  ;; 如果文件末尾有空行，以可视地形式提醒
                indent-tabs-mode nil
                fill-column 120
                window-combination-resize t
                major-mode 'text-mode)

  ;; Turn on transient-mark-mode
  (transient-mark-mode t)

  (setq scroll-margin 2           ;; better scrolling experience
        scroll-step 1
        scroll-conservatively 101 ;; > 100
        scroll-preserve-screen-position t
        use-short-answers t
        visible-bell t ;; 错误操作时的窗口闪动警告
        load-prefer-newer t
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
  :bind ("C-z" . undo-redo)
  :config
  (setq visual-line-fringe-indicators '(nil right-curly-arrow)
        ;; save current clipboard text
        save-interprogram-paste-before-kill t
        ;; eliminate duplicates
        kill-do-not-save-duplicates t
        ;; show the name of character in `what-cursor-position'
        what-cursor-show-names t
        set-mark-command-repeat-pop t

        ;; show column/filesize in modeline
        column-number-mode t
        size-indication-mode t)
  )

(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode)
  :config
  (setq repeat-exit-timeout 3)
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

;; Workaround with minified source files
(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode)
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

(use-package which-key
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-idle-delay 0.5)
  )

;; 谷歌翻译
(use-package go-translate
  :bind ("C-c t" . gts-do-translate)
  :config
  (setq gts-translate-list '(("en" "zh"))
        gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-google-rpc-engine))
         :render (gts-buffer-render)))
  )

(provide 'init-basic)

;;; init-basic.el ends here
