;; init-dired.el --- Initialize dired configurations. -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Directory configurations.
;;
;;; Code:

(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t    ;; Quickly copy/move file in Dired
        delete-by-moving-to-trash t  ;; Move files to trash when deleting
        dired-auto-revert-buffer 'dired-directory-changed-p
        dired-kill-when-opening-new-dired-buffer t
        dired-ls-F-marks-symlinks t
        dired-mouse-drag-files t
        dired-recursive-copies t   ;; 可以递归的进行拷贝
        dired-recursive-deletes t)  ;; 可以递归的删除目录

  (use-package dired-aux
    :ensure nil
    :after dired
    :config
    (setq dired-isearch-filenames 'dwim
          dired-create-destination-dirs 'ask
          dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))
          dired-vc-rename-file t)
    )
  )

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :config
  ;; Make dired-omit-mode hide all "dotfiles"
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$"))
  )

(use-package dirvish
  :bind (("C-c v h" . dirvish)
         ("C-c v d" . dirvish-dired)
         ("C-c v s" . dirvish-side)
         :map dirvish-mode-map
         ("SPC" . dirvish-show-history)
         ("r"   . dirvish-roam)
         ("b"   . dirvish-goto-bookmark)
         ("f"   . dirvish-file-info-menu)
         ("M-a" . dirvish-mark-actions-menu)
         ("M-s" . dirvish-setup-menu)
         ("M-f" . dirvish-toggle-fullscreen)
         ([remap dired-summary] . dirvish-dispatch)
         ([remap dired-do-copy] . dirvish-yank)
         ([remap mode-line-other-buffer] . dirvish-other-buffer))
  :config
  ;; Override dired with dirvish globally
  (dirvish-override-dired-mode)
  (dirvish-peek-mode)
  (setq dirvish-attributes '(all-the-icons file-size expanded-state))
  )

(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
