;; init-buffer.el --- Initialize buffer configurations.  -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;;
;; IBuffer configurations.
;;

;;; Code:

;; Minibuffer 基础优化
(use-package minibuffer
  :ensure nil
  :hook
  (minibuffer-setup . cursor-intangible-mode)  ; 防止光标进入只读提示区域
  :custom
  ;; 历史记录
  (history-delete-duplicates t)
  (enable-recursive-minibuffers t)      ; 允许递归使用 minibuffer
  (minibuffer-depth-indicate-mode t)    ; 显示递归深度
  (minibuffer-electric-default-mode t)  ; 自动隐藏默认值

  ;; 补全行为
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-pcm-complete-word-inserts-delimiters t)
  (completion-cycle-threshold 3)
  (completion-auto-select t)
  (completion-ignore-case t)  ; 忽略大小写
  (completions-detailed t)

  ;; Prompt 样式
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  )

;; minibuffer history
(use-package savehist
  :ensure nil
  :hook
  (after-init . savehist-mode)
  :custom
  (history-length 1000)
  (savehist-additional-variables '(mark-ring
								   global-mark-ring
								   search-ring
								   regexp-search-ring
								   extended-command-history)
								 )
  )

(use-package ibuffer
  :ensure nil
  :bind
  ("C-x C-b" . ibuffer)
  :hook
  (ibuffer-mode . ibuffer-auto-mode)
  :custom
  (ibuffer-expert t)
  (ibuffer-movement-cycle nil)
  )

;; Display icons for buffers
(use-package nerd-icons-ibuffer
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode)
  )

(provide 'init-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-buffer.el ends here
