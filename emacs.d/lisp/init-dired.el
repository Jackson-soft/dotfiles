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
         ("C-c v s" . dirvish-side)
         ("C-c f" . dirvish-fd)
         :map dirvish-mode-map
         ("a"   . dirvish-quick-access)
         ("f"   . dirvish-file-info-menu)
         ("y"   . dirvish-yank-menu)
         ("N"   . dirvish-narrow)
         ("^"   . dirvish-history-last)
         ("s"   . dirvish-quicksort) ; remapped `dired-sort-toggle-or-edit'
         ("?"   . dirvish-dispatch)  ; remapped `dired-summary'
         ("TAB" . dirvish-subtree-toggle)
         ("SPC" . dirvish-history-jump)
         ("M-n" . dirvish-history-go-forward)
         ("M-p" . dirvish-history-go-backward)
         ("M-l" . dirvish-ls-switches-menu)
         ("M-m" . dirvish-mark-menu)
         ("M-f" . dirvish-layout-toggle)
         ("M-s" . dirvish-setup-menu)
         ("M-e" . dirvish-emerge-menu)
         ("M-j" . dirvish-fd-jump))
  :config
  ;; Override dired with dirvish globally
  (dirvish-override-dired-mode)
  (dirvish-peek-mode)
  (setq dirvish-attributes '(all-the-icons file-size collapse subtree-state vc-state git-msg)
        dirvish-mode-line-format '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  )

(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
