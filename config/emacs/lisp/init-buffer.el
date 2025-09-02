;; init-buffer.el --- Initialize buffer configurations.  -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;;
;; IBuffer configurations.
;;

;;; Code:

(use-package minibuffer
  :ensure nil
  :hook
  (minibuffer-setup . cursor-intangible-mode)
  :custom
  (minibuffer-electric-default-mode t)   ;; 当输入内容后，prompt的default值就会被隐藏
  (minibuffer-depth-indicate-mode t)   ;; 显示minibuffer深度
  (history-delete-duplicates t)        ;; minibuffer 删除重复历史
  (enable-recursive-minibuffers t)     ;; 在minibuffer 中也可以再次使用minibuffer
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))          ;; Do not allow the cursor in the minibuffer prompt
  (completion-pcm-complete-word-inserts-delimiters t)
  (completion-cycle-threshold 3)
  (completion-auto-help 'visible)
  (completion-auto-select t)
  (completion-ignore-case t)
  (completions-format 'one-column)
  (completions-sort 'historical)
  (completions-detailed t)
  (completions-group t)
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
