;; init-dired.el --- Initialize dired configurations. -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Directory configurations.
;;
;;; Code:

;; Dired 主配置
(use-package dired
  :ensure nil
  :custom
  ;; 基础行为
  (dired-dwim-target t)                  ;; 智能目标目录
  (delete-by-moving-to-trash t)          ;; 删除文件时移到回收站
  (dired-auto-revert-buffer 'dired-directory-changed-p) ;; 自动刷新
  (dired-kill-when-opening-new-dired-buffer t) ;; 打开新目录时关闭旧 buffer
  (dired-ls-F-marks-symlinks t)          ;; 标记符号链接
  (dired-mouse-drag-files t)             ;; 支持鼠标拖拽文件
  (dired-recursive-copies t)             ;; 递归拷贝
  (dired-recursive-deletes t)            ;; 递归删除
  :config
  ;; 辅助功能
  (use-package dired-aux
	:ensure nil
	:custom
	(dired-isearch-filenames 'dwim)      ;; isearch 默认搜索文件名
	(dired-create-destination-dirs 'ask) ;; 复制/移动时询问是否创建目录
	(dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))
	(dired-vc-rename-file t)))           ;; 支持 VC 重命名

;; Dired 扩展
(use-package dired-x
  :ensure nil
  :hook
  (dired-mode . dired-omit-mode)         ;; 启用隐藏模式
  :custom
  ;; 隐藏 dotfiles
  (dired-omit-files (concat dired-omit-files "\\|^\\..*$")))

(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
