;; init-dired.el --- Initialize dired configurations. -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Directory configurations.
;;
;;; Code:

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-dwim-target t    ;; Quickly copy/move file in Dired
        delete-by-moving-to-trash t  ;; Move files to trash when deleting
        dired-auto-revert-buffer 'dired-directory-changed-p
        dired-kill-when-opening-new-dired-buffer t
        dired-recursive-copies t   ;; 可以递归的进行拷贝
        dired-recursive-deletes t)  ;; 可以递归的删除目录
  ;; Listing directory failed but access-file worked 消除
  (use-package ls-lisp
    :ensure nil
    :if (eq system-type 'darwin)
    :config
    (setq ls-lisp-dirs-first t
          ls-lisp-use-insert-directory-program nil)
    )

  (use-package dired-aux
    :ensure nil
    :after dired
    :config
    (setq dired-isearch-filenames 'dwim
          dired-create-destination-dirs 'ask
          dired-vc-rename-file t)
    )
  )

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :config
  ;; Dont prompt about killing buffer visiting delete file
  (setq dired-clean-confirm-killing-deleted-buffers nil
        dired-omit-verbose nil
        dired-omit-files (concat dired-omit-files
                                 "\\|^.DS_Store$\\|^.cache$\\|^.git*\\|^.idea$\\|^.vscode$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))
  )

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil)
  )

(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
