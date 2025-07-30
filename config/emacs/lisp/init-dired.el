;; init-dired.el --- Initialize dired configurations. -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Directory configurations.
;;
;;; Code:

(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)    ;; Quickly copy/move file in Dired
  (delete-by-moving-to-trash t)  ;; Move files to trash when deleting
  (dired-auto-revert-buffer 'dired-directory-changed-p)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-ls-F-marks-symlinks t)
  (dired-mouse-drag-files t)
  (dired-recursive-copies t)   ;; 可以递归的进行拷贝
  (dired-recursive-deletes t)  ;; 可以递归的删除目录
  :config
  (use-package dired-aux
	:ensure nil
	:custom
	(dired-isearch-filenames 'dwim)
	(dired-create-destination-dirs 'ask)
	(dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))
	(dired-vc-rename-file t)
    )
  )

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  ;; Make dired-omit-mode hide all "dotfiles"
  (dired-omit-files (concat dired-omit-files "\\|^\\..*$"))
  )

(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
