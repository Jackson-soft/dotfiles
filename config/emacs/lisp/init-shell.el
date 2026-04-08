;;; init-shell.el --- Initialize eshell configurations.  -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Eshell configurations.
;;
;;; Code:

(use-package sh-script
  :ensure nil
  :mode
  (("\\.zsh\\'" . bash-ts-mode)
   ("\\.sh\\'" . bash-ts-mode)
   ("\\.*shrc\\'" . bash-ts-mode)
   ("\\.zshenv\\'" . bash-ts-mode))
  :bind
  (:map sh-mode-map
		("C-c C-e" . sh-execute-region))
  )

(use-package eat
  :bind
  (("C-`" . my/toggle-eat-drawer))
  :hook
  ;; Eshell 集成：用 Eat 替代 Term 运行 visual commands（如 htop、less 等）
  (eshell-load . eat-eshell-visual-command-mode)
  :custom
  ;; 默认使用 semi-char 模式（大部分键发送给终端，保留 C-x/C-c 等 Emacs 键）
  (eat-kill-buffer-on-exit t) ;; 进程退出后自动关闭 buffer
  :config
  (defvar my/eat-buffer-name "*eat*"
	"底部弹出的 Eat 终端 buffer 名称。")

  (defvar my/eat-window-height 15
	"底部终端窗口高度（行数）。")

  (defvar my/eat-last-buffer nil
	"记录切换到 Eat 前的 buffer。")

  (defun my/toggle-eat-drawer ()
	"在底部弹出或收起 Eat 终端，并自动进入 semi-char 模式。"
	(interactive)
	(let* ((buf (get-buffer my/eat-buffer-name))
		   (current-buf (current-buffer)))
	  (if (and buf (buffer-live-p buf) (get-buffer-window buf))
		  ;; 收起：关闭窗口，切回之前的 buffer
		  (progn
			(delete-window (get-buffer-window buf))
			(when (and (bufferp my/eat-last-buffer)
					   (buffer-live-p my/eat-last-buffer)
					   (not (eq my/eat-last-buffer buf)))
			  (switch-to-buffer my/eat-last-buffer)))
		;; 打开
		(unless (string= (buffer-name current-buf) my/eat-buffer-name)
		  (setq my/eat-last-buffer current-buf))
		(let ((drawer-win (split-window (selected-window)
										(- my/eat-window-height) 'below)))
		  (select-window drawer-win)
		  (if (and buf (buffer-live-p buf))
			  ;; buffer 已存在，直接显示并切模式
			  (progn
				(set-window-buffer drawer-win buf)
				(with-current-buffer buf
				  (when (and (derived-mode-p 'eat-mode)
							 (process-live-p (get-buffer-process buf)))
					(eat-semi-char-mode))))
			;; buffer 不存在，在当前窗口直接启动 eat
			(eat nil my/eat-buffer-name))))))
  )

(provide 'init-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
