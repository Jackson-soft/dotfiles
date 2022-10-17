;; init-buffer.el --- Initialize buffer configurations.  -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;;
;; IBuffer configurations.
;;

;;; Code:

;; Use the `orderless' completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless partial-completion))
  )

(use-package minibuffer
  :ensure nil
  :hook (minibuffer-setup . cursor-intangible-mode)
  :custom
  (minibuffer-eldef-shorten-default t)
  (minibuffer-electric-default-mode t)   ;; 当输入内容后，prompt的default值就会被隐藏
  (minibuffer-depth-indicate-mode t)   ;; 显示minibuffer深度
  :config
  (setq history-delete-duplicates t        ;; minibuffer 删除重复历史
		enable-recursive-minibuffers t     ;; 在minibuffer 中也可以再次使用minibuffer
		read-buffer-completion-ignore-case t
		read-file-name-completion-ignore-case t

		;; Do not allow the cursor in the minibuffer prompt
		minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)

		completion-pcm-complete-word-inserts-delimiters t
		completion-cycle-threshold 3
		completion-ignore-case t
		completions-format 'one-column
		completions-max-height 20
		completion-category-overrides '((file (styles . (basic partial-completion orderless))))
		completions-detailed t
		completions-group t)
  )

;; minibuffer history
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq history-length 1000
		savehist-additional-variables '(mark-ring
										global-mark-ring
										search-ring
										regexp-search-ring
										extended-command-history)
		)
  )

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :config
  (setq ibuffer-expert t
		ibuffer-movement-cycle nil)
  )

;; Display icons for buffers
(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
  :config
  (setq all-the-icons-ibuffer-human-readable-size t)
  )

(provide 'init-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-buffer.el ends here
