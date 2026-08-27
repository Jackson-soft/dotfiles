;;; init-completion.el --- Config for completion   -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))
                                   (eglot (styles orderless))
                                   (eglot-capf (styles orderless))))
  (completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion 表现类似 substring
  )

;; Vertico 主体
(use-package vertico
  :hook
  (after-init . vertico-mode)
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word))
  :custom
  ;; 行为设置
  (vertico-scroll-margin 0)  ;; 滚动边距
  (vertico-count 20)         ;; 显示候选数量
  (vertico-cycle t)          ;; 循环选择候选项
  )

(use-package consult
  :bind
  ( ;; C-c 系列（mode-specific-map）
   ("C-c M-x" . consult-mode-command)
   ("C-c h"   . consult-history)
   ("C-c k"   . consult-kmacro)
   ("C-c m"   . consult-man)
   ("C-h i"   . consult-info)

   ;; C-x 系列
   ([remap repeat-complex-command] . consult-complex-command) ;; C-x M-:
   ([remap switch-to-buffer]       . consult-buffer)          ;; C-x b
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x t b" . consult-buffer-other-tab)
   ("C-x r b" . consult-bookmark)
   ([remap project-switch-to-buffer] . consult-project-buffer) ;; C-x p b
   ("C-x C-r" . consult-recent-file)

   ;; 其他常用
   ([remap yank-pop] . consult-yank-pop) ;; M-y

   ;; M-g 系列（goto-map）
   ("M-g e" . consult-compile-error)
   ("M-g r" . consult-grep-match)
   ("M-g f" . consult-flymake)
   ([remap goto-line] . consult-goto-line) ;; M-g g
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)

   ;; M-s 系列（search-map）
   ("M-s d" . consult-fd)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ("M-s e" . consult-isearch-history)

   ;; Isearch 集成
   :map isearch-mode-map
   ("M-e"   . consult-isearch-history)
   ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)

   ;; Minibuffer 历史
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))
  :custom
  ;; 缩小候选集的按键
  (consult-narrow-key "<")
  ;; Xref 集成
  (xref-show-xrefs-function       #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  ;; 注册预览延迟
  (register-preview-delay 0.5)
  :config
  ;; 用 Consult 的窗口替代默认 register-preview
  (advice-add #'register-preview :override #'consult-register-window)

  ;; 预览策略：默认即时预览，对开销大的命令延迟预览
  ;; 全局 consult-preview-key 保持默认 'any（即时预览）
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  )

(use-package consult-dir
  :bind
  (("C-x C-d" . consult-dir)
   :map minibuffer-local-completion-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file))
  )

(use-package consult-todo
  :demand t
  :bind
  (("M-s t" . consult-todo)
   ("M-s T" . consult-todo-all))
  )

(use-package marginalia
  :bind
  ("M-A" . marginalia-cycle)
  :hook
  (after-init . marginalia-mode)
  )

(use-package embark
  :bind
  (("C-h B" . embark-bindings)   ;; alternative for `describe-bindings'
   ("C-."   . embark-act)        ;; 主入口
   ("C-;"   . embark-dwim)       ;; 上下文敏感的 act
   ("M-n"   . embark-next-symbol)
   ("M-p"   . embark-previous-symbol))
  :custom
  (embark-cycle-key ".")
  (embark-help-key "?")
  ;; 提示当前可用的 action
  (prefix-help-command #'embark-prefix-help-command)
  :config
  ;; 隐藏 Embark collect buffer 的 mode-line
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  )

(use-package nerd-icons-completion
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  )

(provide 'init-completion)

;;; init-completion.el ends here
