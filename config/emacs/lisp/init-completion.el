;;; init-completion.el --- Config for completion   -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package icomplete
;;   :ensure nil
;;   :hook
;;   (after-init . fido-vertical-mode)
;;   :custom
;;   (icomplete-in-buffer t)
;;   (icomplete-tidy-shadowed-file-names t)
;;   (icomplete-show-matches-on-no-input t)
;;   (icomplete-scroll t)
;;   )

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
  :custom
  ;; 行为设置
  (vertico-scroll-margin 0)  ;; 滚动边距
  (vertico-count 20)         ;; 显示候选数量
  (vertico-resize t)         ;; 自动调整 minibuffer 高度
  (vertico-cycle t)          ;; 循环选择候选项
  :init
  ;; 启用 Vertico
  (vertico-mode 1)
  :config
  ;; 目录导航扩展
  (use-package vertico-directory
	:ensure nil
	:after vertico
	:bind
	(:map vertico-map
		  ("RET"    . vertico-directory-enter)
		  ("DEL"    . vertico-directory-delete-char)
		  ("M-DEL"  . vertico-directory-delete-word))
	:hook
	(rfn-eshadow-update-overlay . vertico-directory-tidy))

  ;; 鼠标支持：滚轮滚动、点击选择候选
  (vertico-mouse-mode 1)

  ;; 重复上次补全会话
  (use-package vertico-repeat
	:ensure nil
	:after vertico
	:bind
	("M-R" . vertico-repeat)
	:hook
	(minibuffer-setup . vertico-repeat-save))

  ;; 为不同命令/类别配置不同显示模式
  (vertico-multiform-mode 1)
  (setq vertico-multiform-commands
		'((consult-imenu buffer indexed)
		  (consult-imenu-multi buffer indexed)
		  (consult-outline buffer)
		  (consult-grep buffer)
		  (consult-ripgrep buffer)
		  (consult-git-grep buffer)
		  (consult-line buffer)
		  (consult-line-multi buffer)
		  (execute-extended-command unobtrusive)))
  (setq vertico-multiform-categories
		'((file (vertico-sort-function . vertico-sort-directories-first)
				(:keymap . vertico-directory-map))
		  (consult-grep buffer)))
  )

(use-package consult
  :bind
  ( ;; C-c 系列（mode-specific-map）
   ("C-c M-x" . consult-mode-command)
   ("C-c h"   . consult-history)
   ("C-c k"   . consult-kmacro)
   ("C-c m"   . consult-man)
   ("C-c i"   . consult-info)

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

  ;; consult-line：按 M-n 自动插入光标处的 symbol 或 region
  (consult-customize
   consult-line
   :add-history (seq-some #'thing-at-point '(region symbol)))

  ;; 允许预览时执行 hl-todo-mode，使预览也能高亮 TODO/FIXME
  (add-to-list 'consult-preview-allowed-hooks 'hl-todo-mode)
  (add-to-list 'consult-preview-allowed-hooks 'elide-head-mode)

  ;; 定义常用 info 手册的搜索命令
  (consult-info-define "emacs" "efaq" "elisp" "cl" "compat")
  (consult-info-define 'completion
					   "vertico" "consult" "marginalia" "orderless"
					   "embark" "corfu" "cape")
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
  :config
  (marginalia-mode)
  )

(use-package embark
  :ensure t
  :bind
  (("C-h B" . embark-bindings)   ;; alternative for `describe-bindings'
   ("C-."   . embark-act)        ;; 主入口
   ("C-;"   . embark-dwim)       ;; 上下文敏感的 act
   ("M-n"   . embark-next-symbol)
   ("M-p"   . embark-previous-symbol))
  :custom
  (embark-cycle-key ".")
  (embark-help-key "?")
  :init
  ;; 提示当前可用的 action
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; 隐藏 Embark collect buffer 的 mode-line
  (add-to-list 'display-buffer-alist
			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
				 nil
				 (window-parameters (mode-line-format . none)))))

;; 如果你用 consult，强烈推荐加上这个
(use-package embark-consult
  :ensure t
  )

(use-package nerd-icons-completion
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  )

(provide 'init-completion)

;;; init-completion.el ends here
