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
  :config
  (defvar my/eat-buffer-name "*eat*"
	"底部弹出的 Eat 终端 buffer 名称。")

  (defvar my/eat-window-height 15
	"底部终端窗口高度（行数）。")

  (defvar my/eat-last-buffer nil
	"记录切换到 Eat 前的 buffer。")

  (defun my/create-eat-buffer-silently ()
	"静默创建 eat buffer，不影响当前窗口。"
	(let ((buf (get-buffer my/eat-buffer-name)))
	  (unless (and buf (buffer-live-p buf))
		;; 保存当前窗口配置，在临时窗口里启动 eat
		(save-window-excursion
		  (let ((temp-win (split-window (selected-window) nil 'below)))
			(select-window temp-win)
			;; 直接指定 buffer 名，避免生成 *eat*<1>
			(condition-case err
				(progn
				  (eat nil my/eat-buffer-name)
				  (setq buf (get-buffer my/eat-buffer-name))
				  ;; 延迟进入 char 模式
				  (when (and buf (eq major-mode 'eat-mode))
					(run-at-time 0.05 nil
								 (lambda ()
								   (when (buffer-live-p buf)
									 (with-current-buffer buf
									   (ignore-errors (eat-char-mode))))))))
			  (error
			   (message "Failed to create eat buffer: %s" err)
			   (setq buf nil)))
			(when (window-live-p temp-win)
			  (delete-window temp-win)))))
	  buf))

  (defun my/toggle-eat-drawer ()
	"在底部弹出或收起 Eat 终端，并自动进入 char 模式。"
	(interactive)
	(let* ((buf (get-buffer my/eat-buffer-name))
		   (current-buf (current-buffer)))
	  (if (and buf (buffer-live-p buf) (get-buffer-window buf))
		  ;; 收起
		  (progn
			(delete-window (get-buffer-window buf))
			(when (and (bufferp my/eat-last-buffer)
					   (buffer-live-p my/eat-last-buffer)
					   (not (eq my/eat-last-buffer buf)))
			  (switch-to-buffer my/eat-last-buffer)))
		;; 打开
		(unless (string= (buffer-name current-buf) my/eat-buffer-name)
		  (setq my/eat-last-buffer current-buf))
		(unless (and buf (buffer-live-p buf))
		  (setq buf (my/create-eat-buffer-silently)))
		(when (and buf (buffer-live-p buf))
		  (let ((drawer-win (split-window (selected-window) (- my/eat-window-height) 'below)))
			(set-window-buffer drawer-win buf)
			(select-window drawer-win)
			(with-current-buffer buf
			  (when (derived-mode-p 'eat-mode)
				(ignore-errors (eat-char-mode)))))))))

  ;; 绑定快捷键
  (global-set-key (kbd "C-`") #'my/toggle-eat-drawer)
  )

(provide 'init-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
