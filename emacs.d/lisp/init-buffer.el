;; init-buffer.el --- Initialize buffer configurations.  -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;;
;; IBuffer configurations.
;;

;;; Code:

(use-package minibuffer
  :ensure nil
  :hook (minibuffer-setup . cursor-intangible-mode)
  :custom
  (minibuffer-eldef-shorten-default t)
  :config
  (setq history-delete-duplicates t        ;;minibuffer 删除重复历史
        enable-recursive-minibuffers t     ;;在minibuffer 中也可以再次使用minibuffer
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t

        ;; Do not allow the cursor in the minibuffer prompt
        minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)

        ;; completions-format 'vertical
        completion-styles '(basic partial-completion substring flex initials)
        completion-category-overrides '((file (styles basic substring)))
        completion-pcm-complete-word-inserts-delimiters t
        completion-cycle-threshold 3
        completions-format 'one-column
        completions-detailed t
        ;; Ignore case when complete
        completion-ignore-case t)

  (minibuffer-electric-default-mode t)   ;;当输入内容后，prompt的default值就会被隐藏
  (minibuffer-depth-indicate-mode t)   ;;显示minibuffer深度
  (file-name-shadow-mode t)
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
        savehist-autosave-interval 300)
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
